namespace Cfrm

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

    /// Creates a strategy profile from the given info set map.
    let toStrategyProfile (infoSetMap : InfoSetMap) =
        infoSetMap
            |> Map.map (fun _ infoSet ->
                let strategy =
                    infoSet
                        |> InfoSet.getAverageStrategy
                        |> Vector.toArray
                assert(strategy.Length > 1)
                strategy)
            |> StrategyProfile
