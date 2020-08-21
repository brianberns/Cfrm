// http://modelai.gettysburg.edu/2013/cfr/
// https://github.com/tt293/medium-poker-ai/blob/master/part_7/multiplayer_kuhn_poker_cfr.py

open System

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

/// Actions a player can take at a decision node: check and bet.
let numActions = 2

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

type Vector =

    | Vector of float[]

    member vector.Length =
        let (Vector array) = vector
        array.Length

    member vector.Item(i) =
        let (Vector array) = vector
        array.[i]

    static member (+)(Vector array1, Vector array2) =
        Array.zip array1 array2
            |> Array.map (fun (x, y) -> x + y)
            |> Vector

    static member (-)(Vector array, x) =
        array
            |> Array.map (fun y -> y - x)
            |> Vector

    static member (*)(x, Vector array) =
        array
            |> Array.map ((*) x)
            |> Vector

    static member (*)(Vector array1, Vector array2) =
        Array.zip array1 array2
            |> Array.map (fun (x, y) -> x * y)
            |> Vector

    static member (/)(Vector array, den) =
        array
            |> Array.map (fun num -> num / den)
            |> Vector

module Vector =

    let zeroCreate count =
        Vector (Array.zeroCreate count)

    let map mapping (Vector array) =
        array
            |> Array.map mapping
            |> Vector

    let sum (Vector array) =
        Array.sum array

type InfoSet =
    {
        Key : string
        NumActions : int
        RegretSum : Vector
        StrategySum : Vector
    }

module InfoSet =

    let create key numActions =
        {
            Key = key
            NumActions = numActions
            RegretSum = Vector.zeroCreate numActions
            StrategySum = Vector.zeroCreate numActions
        }

    let private normalize strategy =
        let total = Vector.sum strategy
        if total > 0.0 then
            strategy / total
        else
            let value = 1.0 / float strategy.Length
            strategy |> Vector.map (fun _ -> value)   // replace with uniform distribution

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

module InfoSetMap =

    let getInfoSet card history (infoSetMap : InfoSetMap) =

        let key = sprintf "%s %s" (cardStr card) history
        match infoSetMap |> Map.tryFind key with
            | Some infoSet -> infoSet, infoSetMap
            | None ->
                let infoSet = InfoSet.create key numActions
                let infoSetMap = infoSetMap |> Map.add key infoSet
                infoSet, infoSetMap

let cfr infoSetMap (cards : Card[]) =

    let rec loop infoSetMap (history : string) reach0 reach1 =

        let n = history.Length
        let iPlayer = n % 2
        let iOpponent = 1 - iPlayer
        let sign =
            if cards.[iPlayer] > cards.[iOpponent] then 1
            else -1

        let terminalUtility = function
            | "cbc" | "bc" -> Some 1            // player's bet was followed by a check, so player wins ante
            | "cc" -> Some (sign * 1)           // no bets: high card wins ante only
            | "cbb" | "bb" -> Some (sign * 2)   // two bets: high card wins ante and bet
            | _ -> None

        match terminalUtility history with
            | Some util -> float util, infoSetMap
            | _ ->

                let infoSet, infoSetMap =
                    infoSetMap |> InfoSetMap.getInfoSet cards.[iPlayer] history
                let strategy, infoSet =
                    let reach = [| reach0; reach1 |].[iPlayer]
                    infoSet |> InfoSet.getStrategy reach
                // no need to add the modified info set back into the map yet, because it shouldn't be visited again recursively

                let actionUtils, infoSetMap =
                    [| "c"; "b" |]
                        |> Array.indexed
                        |> Array.mapFold (fun accMap (i, action) ->
                            let nextHistory = history + action
                            let util, accMap =
                                if iPlayer = 0 then
                                    loop accMap nextHistory (reach0 * strategy.[i]) reach1
                                else
                                    loop accMap nextHistory reach0 (reach1 * strategy.[i])
                            -1.0 * util, accMap) infoSetMap
                        |> fun (utils, accMap) ->
                            Vector utils, accMap

                let nodeUtil = Vector.sum (strategy * actionUtils)
                let regret = actionUtils - nodeUtil
                let infoSet =
                    let regretSum =
                        let reach = [| reach1; reach0 |].[iPlayer]
                        infoSet.RegretSum + (reach * regret)
                    { infoSet with RegretSum = regretSum }
                let infoSetMap = infoSetMap |> Map.add infoSet.Key infoSet
                nodeUtil, infoSetMap

    loop infoSetMap "" 1.0 1.0

[<EntryPoint>]
let main argv =

    let cards = Enum.getValues<Card>
    let numIterations = 100000
    let rng = Random(0)
    let accUtil, infoSetMap =
        ((0.0, Map.empty), [|1..numIterations|])
            ||> Seq.fold (fun (accUtil, accMap) _ ->
                knuthShuffle rng cards |> ignore
                let util, accMap = cfr accMap cards
                accUtil + util, accMap)
    let expectedGameValue = accUtil / (float) numIterations

    printfn "Player 1 expected value: %g" expectedGameValue
    printfn "Player 2 expected value: %g" -expectedGameValue
    for (key, infoSet : InfoSet) in infoSetMap |> Map.toSeq do
        printfn "%s: %A" key (infoSet |> InfoSet.getAverageStrategy)

    // https://en.wikipedia.org/wiki/Kuhn_poker#Optimal_strategy
    let epsilon = 0.03
    assert(abs(expectedGameValue - (-1.0/18.0)) < epsilon)
    let get key i =
        (infoSetMap.[key] |> InfoSet.getAverageStrategy).[i]
    let alpha = get "J " 1
    assert(alpha >= 0.0 && alpha <= 1.0/3.0)
    assert(abs(get "Q " 0 - 1.0) < epsilon)
    assert(abs(get "Q cb" 1 - (alpha + 1.0/3.0)) < epsilon)
    assert(abs(get "K " 1 - (3.0 * alpha)) < epsilon)

    0
