// Vanilla Counterfactual Regret Minimization: https://justinsermeno.com/posts/cfr/

open System
open System.Collections.Generic

module Enum =

    /// Answers all values of the given enum type.
    let getValues<'enum> =
        Enum.GetValues(typeof<'enum>)
            |> Seq.cast<'enum>
            |> Seq.toArray

/// Actions a player can take at a decision node: check and bet.
let numActions = 2

type Card =
    | Jack = 11
    | Queen = 12
    | King = 13

/// Jack, Queen, King
let numCards = Enum.getValues<Card>.Length

let isTerminalHistory history =
    match history with
        | "rrcc"
        | "rrcbc"
        | "rrcbb"
        | "rrbc"
        | "rrbb" -> true
        | _ -> false

let terminalUtil (history : string) (card1 : Card) (card2 : Card) =

    let n = history.Length
    let cardPlayer = if n % 2 = 0 then card1 else card2
    let cardOpponent = if n % 2 = 0 then card2 else card1

    match history with
        | "rrcbc"
        | "rrbc" -> 1
        | "rrcc" ->
            if cardPlayer > cardOpponent then 1
            else -1
        | "rrcbb"
        | "rrbb" ->
            if cardPlayer > cardOpponent then 2
            else -2
        | _ -> failwith "Unexpcted"

let cardStr card =
    match card with
        | Card.Jack ->  "J"
        | Card.Queen -> "Q"
        | Card.King ->  "K"
        | _ -> failwith "Unexpected"

type Vector =

    | Vector of float[]

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

    let replicate n initial =
        Vector (Array.replicate n initial)

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
        Strategy : Vector
        ReachProb : float
        ReachProbSum : float
    }

module InfoSet =

    let create key numActions =
        {
            Key = key
            NumActions = numActions
            RegretSum = Vector.zeroCreate numActions
            StrategySum = Vector.zeroCreate numActions
            Strategy = Vector.replicate numActions (1.0 / float numActions)
            ReachProb = 0.0
            ReachProbSum = 0.0
        }

    let calcStrategy infoSet =
        let strategy = infoSet.RegretSum |> Vector.map (max 0.0)
        let total = Vector.sum strategy
        if total > 0.0 then
            strategy / total
        else
            Vector.replicate numActions (1.0 / float numActions)

    let nextStrategy infoSet =
        {
            infoSet with
                StrategySum = infoSet.StrategySum + (infoSet.ReachProb * infoSet.Strategy)
                Strategy = calcStrategy infoSet
                ReachProbSum = infoSet.ReachProbSum + infoSet.ReachProb
                ReachProb = 0.0
        }

    let getAverageStrategy infoSet =
        let strategy =
            (infoSet.StrategySum / infoSet.ReachProbSum)
                |> Vector.map (fun x ->
                    if x < 0.0001 then 0.0
                    else x)
        let total = Vector.sum strategy
        strategy / total


let getInfoSet infoSetMap card history =

    let key = sprintf "%s %s" (cardStr card) history
    match infoSetMap |> Map.tryFind key with
        | Some infoSet -> infoSet, infoSetMap
        | None ->
            let infoSet = InfoSet.create key numActions
            let infoSetMap = infoSetMap |> Map.add key infoSet
            infoSet, infoSetMap

let rec cfr infoSetMap history card1 card2 pr1 pr2 prC =

    if isTerminalHistory history then
        let util = terminalUtil history card1 card2 |> float
        util, infoSetMap
    else
        let n = history.Length
        let isPlayer1 = (n % 2 = 0)
        let infoSet, infoSetMap =
            getInfoSet
                infoSetMap
                (if isPlayer1 then card1 else card2)
                history
        let strategy = infoSet.Strategy
        let infoSet =
            let prob = if isPlayer1 then pr1 else pr2
            { infoSet with ReachProb = infoSet.ReachProb + prob }

        let actionUtils, infoSetMap =
            [| "c"; "b" |]
                |> Array.indexed
                |> Array.mapFold (fun acc (i, action) ->
                    let nextHistory = history + action
                    if isPlayer1 then
                        let util, acc = cfr acc nextHistory card1 card2 (pr1 * strategy.[i]) pr2 prC
                        -1.0 * util, acc
                    else
                        let util, acc = cfr acc nextHistory card1 card2 pr1 (pr2 * strategy.[i]) prC
                        -1.0 * util, acc) infoSetMap
                |> fun (utils, acc) -> Vector utils, acc

        let util = (actionUtils * strategy) |> Vector.sum
        let regrets = actionUtils - util
        let infoSet =
            let regret =
                if isPlayer1 then
                    pr2 * prC * regrets
                else
                    pr1 * prC * regrets
            { infoSet with RegretSum = infoSet.RegretSum + regret }
        let infoSetMap = infoSetMap |> Map.add infoSet.Key infoSet
        util, infoSetMap

let chanceUtil infoSetMap =

    let cardPairs =
        [|
            for card1 in Enum.getValues<Card> do
                for card2 in Enum.getValues<Card> do
                    if card1 <> card2 then
                        yield card1, card2
        |]

    let den = float cardPairs.Length
    let num, infoSetMap =
        ((0.0, infoSetMap), cardPairs)
            ||> Seq.fold (fun (accUtil, accMap) (card1, card2) ->
                let util, accMap = cfr accMap "rr" card1 card2 1.0 1.0 (1.0 / den)
                accUtil + util, accMap)
    num / den, infoSetMap

[<EntryPoint>]
let main argv =

    let numIterations = 100000
    let accUtil, infoSetMap =
        ((0.0, Map.empty), [|1..numIterations|])
            ||> Seq.fold (fun (accUtil, accMap) _ ->
                let util, accMap = chanceUtil accMap
                let accMap =
                    accMap
                        |> Map.map (fun _ infoSet ->
                            infoSet |> InfoSet.nextStrategy)
                accUtil + util, accMap)
    let expectedGameValue = accUtil / (float) numIterations

    printfn "Player 1 expected value: %g" expectedGameValue
    printfn "Player 2 expected value: %g" -expectedGameValue
    for (key, infoSet : InfoSet) in infoSetMap |> Map.toSeq do
        printfn "%s: %A" key (infoSet |> InfoSet.getAverageStrategy)

    0
