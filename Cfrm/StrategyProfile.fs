namespace Cfrm

open System.IO
open System.Runtime.Serialization.Formatters.Binary

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
        let formatter = BinaryFormatter()
        use stream = new FileStream(path, FileMode.Create)
        formatter.Serialize(stream, strategyMap)

    /// Loads a profile from a file.
    static member Load(path) =
        let formatter = BinaryFormatter()
        use stream = new FileStream(path, FileMode.Open)
        formatter.Deserialize(stream)
            :?> StrategyMap
            |> StrategyProfile

and private StrategyMap = Map<string (*InfoSet.Key*), float[]>
