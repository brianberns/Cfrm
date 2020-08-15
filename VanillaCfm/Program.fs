// Vanilla Counterfactual Regret Minimization: https://justinsermeno.com/posts/cfr/

open System.Collections.Generic

/// Actions a player can take at a decision node: check and bet.
let numActions = 2

/// Jack, Queen, King
let numCards = 3

let isChanceNode history =
    history = ""

let isTerminalHistory history =
    match history with
        | "rrcc"
        | "rrcbc"
        | "rrcbb"
        | "rrbc"
        | "rrbb" -> true
        | _ -> false

let terminalUtil (history : string) card1 card2 =

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
        | 0 -> "J"
        | 1 -> "Q"
        | 2 -> "K"
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

type InfoSet(key) =

    let mutable regretSum = Vector.zeroCreate numActions
    let mutable strategySum = Vector.zeroCreate numActions
    let mutable strategy = Vector.replicate numActions (1.0 / float numActions)
    let mutable reachPr = 0.0
    let mutable reachPrSum = 0.0

    let calcStrategy () =
        let strategy = regretSum |> Vector.map (max 0.0)
        let total = Vector.sum strategy
        if total > 0.0 then
            strategy / total
        else
            Vector.replicate numActions (1.0 / float numActions)

    member __.NextStrategy() =
        strategySum <- strategySum + (reachPr * strategy)
        strategy <- calcStrategy ()
        reachPrSum <- reachPrSum + reachPr
        reachPr <- 0.0

    member __.GetAverageStrategy() =
        let strategy =
            (strategySum / reachPrSum)
                |> Vector.map (fun x ->
                    if x < 0.0001 then 0.0
                    else x)
        let total = Vector.sum strategy
        strategy / total

    member __.Strategy = strategy

    member __.ReachPr
        with set (x) = reachPr <- x
        and get () = reachPr

    member __.RegretSum
        with set (x) = regretSum <- x
        and get () = regretSum

    (*
    member __.Key = key
    member __.RegretSum = regretSum
    member __.StrategySum = strategySum
    *)

let getInfoSet (infoSetMap : Dictionary<string, InfoSet>) card history =

    let key = cardStr card + " " + history
    let flag, infoSet = infoSetMap.TryGetValue(key)
    if flag then infoSet
    else
        let infoSet = InfoSet(key)
        infoSetMap.[key] <- infoSet
        infoSet

let rec cfr infoSetMap history card1 card2 pr1 pr2 prC =

    if isChanceNode history then
        chanceUtil infoSetMap |> float
    elif isTerminalHistory history then
        terminalUtil history card1 card2 |> float
    else
        let n = history.Length
        let isPlayer1 =
            n % 2 = 0
        let infoSet =
            getInfoSet
                infoSetMap
                (if isPlayer1 then card1 else card2)
                history
        let strategy = infoSet.Strategy
        if isPlayer1 then
            infoSet.ReachPr <- infoSet.ReachPr + pr1
        else
            infoSet.ReachPr <- infoSet.ReachPr + pr2

        let actionUtils =
            [| "c"; "b" |]
                |> Array.indexed
                |> Array.map (fun (i, action) ->
                    let nextHistory = history + action
                    if isPlayer1 then
                        -1.0 * (cfr infoSetMap nextHistory card1 card2 (pr1 * strategy.[i]) pr2 prC)
                    else
                        -1.0 * cfr infoSetMap nextHistory card1 card2 pr1 (pr2 * strategy.[i]) prC)
                |> Vector

        let util = (actionUtils * strategy) |> Vector.sum
        let regrets = actionUtils - util
        if isPlayer1 then
            infoSet.RegretSum <- infoSet.RegretSum + (pr2 * prC * regrets)
        else
            infoSet.RegretSum <- infoSet.RegretSum + (pr1 * prC * regrets)
        util

and chanceUtil infoSetMap =
    let mutable expectedValue = 0.0
    let numPossibilities = float (2 * 3)
    for i = 0 to numCards - 1 do
        for j = 0 to numCards - 1 do
            if i <> j then
                expectedValue <- expectedValue + cfr infoSetMap "rr" i j 1.0 1.0 (1.0 / numPossibilities)
    expectedValue / numPossibilities

let report expectedGameValue (infoSetMap : Dictionary<string, InfoSet>) =

    printfn "Player 1 expected value: %g" expectedGameValue
    printfn "Player 2 expected value: %g" -expectedGameValue
    for (KeyValue(key, infoSet)) in infoSetMap do
        printfn "%s: %A" key (infoSet.GetAverageStrategy())

[<EntryPoint>]
let main argv =

    let mutable infoSetMap = Dictionary<string, InfoSet>()
    let numIterations = 100000
    let mutable expectedGameValue = 0.0

    for _ = 1 to numIterations do
        expectedGameValue <- expectedGameValue + cfr infoSetMap "" -1 -1 1.0 1.0 1.0
        for (KeyValue(_, infoSet)) in infoSetMap do
            infoSet.NextStrategy()

    expectedGameValue <- expectedGameValue / (float) numIterations

    report expectedGameValue infoSetMap

    0
