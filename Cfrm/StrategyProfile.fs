namespace Cfrm

open System.IO
open MathNet.Numerics.Distributions

/// Collection of strategies for every information set in a game.
type StrategyProfile(strategyMap : StrategyMap) =

    /// Samples an action for the game state with the given
    /// key. Answers the index of the sampled action, or None
    /// if the key isn't present in the profile.
    member _.Sample(key, rng) =
        strategyMap
            |> Map.tryFind key
            |> Option.map (fun strategy ->
                Categorical.Sample(rng, strategy))

    /// Chooses the action with the highest probability in the
    /// given game state. Answers the index of the chosen action,
    /// or None if the key isn't present in the profile.
    member _.Best(key) =
        strategyMap
            |> Map.tryFind key
            |> Option.map (fun strategy ->
                strategy
                    |> Seq.indexed
                    |> Seq.maxBy snd
                    |> fst)

    /// Full key/strategy map.
    member _.Map =
        strategyMap

    /// Full key/strategy dictionary.
    member _.ToDict() =
        strategyMap
            |> Map.toSeq
            |> dict

    /// Saves the profile to a file.
    member _.Save(path) =
        use stream = new FileStream(path, FileMode.Create)
        use wtr = new BinaryWriter(stream)
        wtr.Write(strategyMap.Count)
        for (KeyValue(key, strategy)) in strategyMap do
            wtr.Write(key)
            wtr.Write(uint8 strategy.Length)
            for prob in strategy do
                wtr.Write(float32 prob)   // reduce precision to save space

    /// Loads a profile from a file.
    static member Load(path) =
        use stream = new FileStream(path, FileMode.Open)
        use rdr = new BinaryReader(stream)
        let nStrategies = rdr.ReadInt32()
        let profile =
            (Map.empty, seq { 1 .. nStrategies })
                ||> Seq.fold (fun acc _ ->
                    let key = rdr.ReadString()
                    let strategy =
                        let nProbs = rdr.ReadByte() |> int
                        [|
                            for _ = 1 to nProbs do
                                rdr.ReadSingle() |> float
                        |]
                    acc |> Map.add key strategy)
                |> StrategyProfile
        if stream.Length <> stream.Position then
            failwith "Corrupt strategy profile"
        profile

and private StrategyMap = Map<string (*InfoSet.Key*), float[]>
