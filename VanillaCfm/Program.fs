// http://modelai.gettysburg.edu/2013/cfr/
// https://github.com/tt293/medium-poker-ai/blob/master/part_7/multiplayer_kuhn_poker_cfr.py

open System
open MathNet.Numerics.LinearAlgebra

module Enum =

    /// Answers all values of the given enum type.
    let getValues<'enum> =
        Enum.GetValues(typeof<'enum>)
            |> Seq.cast<'enum>
            |> Seq.toArray

/// Shuffles the given array in place.
/// From http://rosettacode.org/wiki/Knuth_shuffle#F.23
let knuthShuffle (rng : Random) (items : _[]) =
    let swap i j =
        let item = items.[i]
        items.[i] <- items.[j]
        items.[j] <- item
    let len = items.Length
    [0 .. len - 2]
        |> Seq.iter (fun i -> swap i (rng.Next(i, len)))
    items

type InfoSet =
    {
        Key : string
        NumActions : int
        RegretSum : Vector<float>
        StrategySum : Vector<float>
    }

module InfoSet =

    let create key numActions =
        {
            Key = key
            NumActions = numActions
            RegretSum = DenseVector.zero numActions
            StrategySum = DenseVector.zero numActions
        }

    let private normalize strategy =
        let total = Vector.sum strategy
        if total > 0.0 then
            strategy / total
        else
            (1.0 / float strategy.Count)
                |> DenseVector.create strategy.Count   // uniform distribution

    let getStrategy (reach : float) infoSet =
        let strategy =
            infoSet.RegretSum
                |> Vector.map (max 0.0)
                |> normalize
        let infoSet =
            { infoSet with
                StrategySum = infoSet.StrategySum + (reach * strategy) }
        strategy, infoSet

    let getAverageStrategy infoSet =
        infoSet.StrategySum
            |> normalize 
            |> Vector.map (fun x ->
                if x < 0.001 then 0.0 else x)   // eliminate very low probability actions
            |> normalize

type InfoSetMap = Map<string, InfoSet>

/// Actions a player can take at a decision node: check and bet.
let numActions = 2

let numPlayers = 2

type Card =
    | Jack = 11
    | Queen = 12
    | King = 13

/// Jack, Queen, King
let numCards = Enum.getValues<Card>.Length

let cardStr card =
    match card with
        | Card.Jack ->  "J"
        | Card.Queen -> "Q"
        | Card.King ->  "K"
        | _ -> failwith "Unexpected"

module InfoSetMap =

    let getInfoSet card history (infoSetMap : InfoSetMap) =

        let key = sprintf "%s %s" (cardStr card) history
        match infoSetMap |> Map.tryFind key with
            | Some infoSet -> infoSet, infoSetMap
            | None ->
                let infoSet = InfoSet.create key numActions
                let infoSetMap = infoSetMap |> Map.add key infoSet
                infoSet, infoSetMap

let getCounterFactualReach (probs : Vector<_>) iPlayer =
    seq {
        for i = 0 to probs.Count - 1 do
            if i <> iPlayer then
                yield probs.[i]
    } |> Seq.fold (*) 1.0

let cfr infoSetMap (cards : Card[]) =

    let rec loop infoSetMap (history : string) (reaches : Vector<_>) =

        let n = history.Length
        let iPlayer = n % numPlayers
        let iOpponent = 1 - iPlayer
        let sign =
            if cards.[iPlayer] > cards.[iOpponent] then 1.0
            else -1.0

        let terminalUtility = function
            | "cbc" ->   // player 1 wins ante only
                assert(iPlayer = 1)
                [| -1.0; 1.0 |] |> DenseVector.ofArray |> Some
            | "bc" ->    // player 0 wins ante only
                assert(iPlayer = 0)
                [| 1.0; -1.0 |] |> DenseVector.ofArray |> Some
            | "cc" ->    // no bets: high card wins ante only
                assert(iPlayer = 0)
                [| sign * 1.0; sign * -1.0 |] |> DenseVector.ofArray |> Some
            | "cbb" ->   // two bets: high card wins ante and bet
                assert(iPlayer = 1)
                [| sign * -2.0; sign * 2.0 |] |> DenseVector.ofArray |> Some
            | "bb" ->    // two bets: high card wins ante and bet
                assert(iPlayer = 0)
                [| sign * 2.0; sign * -2.0 |] |> DenseVector.ofArray |> Some
            | _ -> None

        match terminalUtility history with
            | Some util -> util, infoSetMap
            | _ ->

                let infoSet, infoSetMap =
                    infoSetMap |> InfoSetMap.getInfoSet cards.[iPlayer] history
                let strategy, infoSet =
                    infoSet |> InfoSet.getStrategy reaches.[iPlayer]
                // no need to add the modified info set back into the map yet, because it shouldn't be visited again recursively

                // let counterFactualValues, infoSetMap =
                let counterFactualValues, infoSetMaps =
                    [| "c"; "b" |]
                        |> Seq.indexed
                        |> Seq.scan (fun (_, accMap) (ia, action) ->
                            let nextHistory = history + action
                            let reaches =
                                reaches
                                    |> Vector.mapi (fun ip reach ->
                                        if ip = iPlayer then reach * strategy.[ia]
                                        else reach)
                            if iPlayer = 0 then
                                loop accMap nextHistory reaches
                            else
                                loop accMap nextHistory reaches) (DenseVector.ofArray Array.empty, infoSetMap)
                        |> Seq.toArray
                        |> Array.unzip
                assert(counterFactualValues.Length = numActions + 1)
                let counterFactualValues = counterFactualValues.[1..] |> DenseMatrix.ofRowSeq
                let infoSetMap = infoSetMaps |> Array.last

                let nodeValues = strategy * counterFactualValues
                let infoSet =
                    let cfReach = getCounterFactualReach reaches iPlayer
                    let regretSum =
                        infoSet.RegretSum
                            |> Vector.mapi (fun ia oldRegret ->
                                let regret = counterFactualValues.[ia, iPlayer] - nodeValues.[iPlayer]
                                oldRegret + (cfReach * regret))
                    { infoSet with RegretSum = regretSum }
                let infoSetMap = infoSetMap |> Map.add infoSet.Key infoSet
                nodeValues, infoSetMap

    loop infoSetMap "" (DenseVector.create numPlayers 1.0)

[<EntryPoint>]
let main argv =

    let cards = Enum.getValues<Card>
    let numIterations = 100000
    let rng = Random(0)
    let accUtils, infoSetMap =
        ((DenseVector.zero numPlayers, Map.empty), [|1..numIterations|])
            ||> Seq.fold (fun (accUtils, accMap) _ ->
                knuthShuffle rng cards |> ignore
                let utils, accMap = cfr accMap cards
                accUtils + utils, accMap)
    let expectedGameValues = accUtils / (float) numIterations

    printfn "Expected value: %A" expectedGameValues
    for (key, infoSet : InfoSet) in infoSetMap |> Map.toSeq do
        printfn "%s: %A" key (infoSet |> InfoSet.getAverageStrategy)

    // https://en.wikipedia.org/wiki/Kuhn_poker#Optimal_strategy
    let epsilon = 0.03
    assert(abs(expectedGameValues.[0] - (-1.0/18.0)) < epsilon)
    let get key i =
        (infoSetMap.[key] |> InfoSet.getAverageStrategy).[i]
    let alpha = get "J " 1
    assert(alpha >= 0.0 && alpha <= 1.0/3.0)
    assert(abs(get "Q " 0 - 1.0) < epsilon)
    assert(abs(get "Q cb" 1 - (alpha + 1.0/3.0)) < epsilon)
    assert(abs(get "K " 1 - (3.0 * alpha)) < epsilon)

    0
