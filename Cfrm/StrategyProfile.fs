namespace Cfrm

open System.IO
open Newtonsoft.Json

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
        use wtr = new StreamWriter(path: string)
        JsonConvert.SerializeObject(strategyMap, Formatting.Indented)
            |> wtr.Write

    /// Loads a profile from a file.
    static member Load(path) =
        use rdr = new StreamReader(path : string)
        JsonConvert.DeserializeObject<StrategyMap>(rdr.ReadToEnd())
            |> StrategyProfile

and private StrategyMap = Map<string (*InfoSet.Key*), float[]>
