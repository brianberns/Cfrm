namespace Cfrm

open System.IO
open MathNet.Numerics.LinearAlgebra

/// Per-action probability of taking each legal action in a particular
/// information set. Probabilities in each instance should sum to 1.0.
type Strategy = Vector<float>

/// Represents the set of nodes in a game-tree that are indistinguishable
/// for a given player.
type InfoSet =
    {
        /// Per-action sum of all regrets computed so far. Positive regret
        /// indicates that we would rather have taken a different action.
        /// Negative regret indicates that we are happy with the action.
        RegretSum : Vector<float>

        /// Per-action sum of all strategies computed so far.
        StrategySum : Vector<float>
    }

module InfoSet =

    /// Creates a new info set with the given number of legal actions.
    let create numActions =
        {
            RegretSum = DenseVector.zero numActions
            StrategySum = DenseVector.zero numActions
        }

    /// Creates a normalized strategy vector from the given values.
    let private normalize values : Strategy =
        let total = Vector.sum values
        if total > 0.0 then
            values / total               // normalize
        else
            (1.0 / float values.Count)   // use uniform strategy when regret is negative
                |> DenseVector.create values.Count

    /// Creates a new strategy for the given info set using the given
    /// reach probability.
    let getStrategy (reachProb : float) infoSet =
        assert(reachProb >= 0.0 && reachProb <= 1.0)

            // compute strategy from current regrets
        let strategy =
            infoSet.RegretSum
                |> Vector.map (max 0.0)
                |> normalize

            // accumulate weighted strategy sum
        let infoSet' =
            { infoSet with
                StrategySum =
                    infoSet.StrategySum + (reachProb * strategy) }

        strategy, infoSet'

    /// Accumulates the given per-action regrets.
    let accumulateRegret regrets infoSet =
        { infoSet with
            RegretSum = infoSet.RegretSum + regrets }

    /// Gets the average strategy for the given info set. This converges on
    /// a Nash equilibrium.
    let getAverageStrategy infoSet =
        infoSet.StrategySum
            |> normalize 
            |> Vector.map (fun x ->
                if x < 0.001 then 0.0 else x)   // eliminate very low probability actions
            |> normalize

/// Maps keys (which typically represent game histories) to
/// known info sets.
type InfoSetMap = Map<string (*InfoSet.Key*), InfoSet>

module InfoSetMap =

    /// Obtains an info set for the given key, creating one if
    /// it doesn't already exist.
    let getInfoSet key numActions (infoSetMap : InfoSetMap) =
        match infoSetMap |> Map.tryFind key with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet, infoSetMap
            | None ->
                let infoSet = InfoSet.create numActions
                let infoSetMap' = infoSetMap |> Map.add key infoSet
                infoSet, infoSetMap'

    /// Saves the given info set map to a file.
    let save path (infoSetMap : InfoSetMap) =
        use stream = new FileStream(path, FileMode.Create)
        use wtr = new BinaryWriter(stream)
        wtr.Write(infoSetMap.Count)
        for (KeyValue(key, infoSet)) in infoSetMap do
            wtr.Write(key)
            wtr.Write(uint8 infoSet.RegretSum.Count)
            for sum in infoSet.RegretSum do
                wtr.Write(sum)
            wtr.Write(uint8 infoSet.StrategySum.Count)
            for sum in infoSet.StrategySum do
                wtr.Write(sum)

    /// Loads a vector
    let private loadVector (rdr : BinaryReader) =
        let n = rdr.ReadByte() |> int
        [|
            for _ = 1 to n do
                rdr.ReadDouble()
        |] |> DenseVector.ofArray

    /// Loads an info set map from a file.
    let load path =
        use stream = new FileStream(path, FileMode.Open)
        use rdr = new BinaryReader(stream)
        let nInfoSets = rdr.ReadInt32()
        let infoSetMap =
            (Map.empty, seq { 1 .. nInfoSets })
                ||> Seq.fold (fun acc _ ->
                    let key = rdr.ReadString()
                    let infoSet =
                        {
                            RegretSum = loadVector rdr
                            StrategySum = loadVector rdr
                        }
                    acc |> Map.add key infoSet)
        if stream.Length <> stream.Position then
            failwith "Corrupt info set map"
        infoSetMap
