namespace Cfrm

open System.IO
open MathNet.Numerics.LinearAlgebra
open Newtonsoft.Json

/// Per-action probability of taking each legal action in
/// a particular information set.
type private Strategy = Vector<float>

/// Collection of strategies for every information set in a
/// game.
type StrategyProfile(strategyMap : StrategyMap) =

    static let serializer = JsonSerializer()

    /// Strategies in this profile.
    member __.Strategies =
        strategyMap |> Map.toArray

    /// Answers the strategy with the given key.
    member __.Item
        with get(key) = strategyMap.[key]

    /// Saves the profile to a file.
    member __.Save(path) =
        use wtr = new StreamWriter(path: string)
        serializer.Serialize(wtr, strategyMap)

    /// Loads a profile from a file.
    static member Load(path) =
        use rdr = new StreamReader(path : string)
        serializer.Deserialize(rdr, typeof<StrategyMap>)
            :?> StrategyMap
            |> StrategyProfile

and private StrategyMap = Map<string (*InfoSet.Key*), float[]>
