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

    /// Strategies in this profile.
    member __.Strategies =
        strategyMap |> Map.toArray

    /// Answers the strategy with the given key.
    member __.Item
        with get(key) = strategyMap.[key]

    /// Saves the profile to a file.
    member __.Save(path) =

        let prunedMap =
            strategyMap
                |> Map.toSeq
                |> Seq.where (fun (_, strategy) ->
                    strategy.Length > 1)
                |> Map

        use wtr = new StreamWriter(path: string)
        JsonConvert.SerializeObject(prunedMap, Formatting.Indented)
            |> wtr.Write

    /// Loads a profile from a file.
    static member Load(path) =
        use rdr = new StreamReader(path : string)
        JsonConvert.DeserializeObject<StrategyMap>(rdr.ReadToEnd())
            |> StrategyProfile

and private StrategyMap = Map<string (*InfoSet.Key*), float[]>
