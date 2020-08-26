namespace Cfrm

open System.IO

/// Collection of strategies for every information set in a game.
type StrategyProfile(strategyMap : StrategyMap) =

    /// Key/strategy pairs in this profile.
    member __.StrategyPairs =
        strategyMap |> Map.toArray

    /// Answers the strategy with the given key.
    member __.Item
        with get(key) = strategyMap.[key]

    /// Saves the profile to a file.
    member __.Save(path) =
        use stream = new FileStream(path, FileMode.Create)
        use wtr = new BinaryWriter(stream)
        wtr.Write(strategyMap.Count)
        for (KeyValue(key, strategy)) in strategyMap do
            wtr.Write(key)
            wtr.Write(uint8 strategy.Length)
            for prob in strategy do
                wtr.Write(float32 prob)

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
        assert(stream.Length = stream.Position)
        profile

and private StrategyMap = Map<string (*InfoSet.Key*), float[]>
