// http://modelai.gettysburg.edu/2013/cfr/
// https://github.com/tt293/medium-poker-ai/blob/master/part_7/multiplayer_kuhn_poker_cfr.py

open System
open MathNet.Numerics.LinearAlgebra

type InfoSet =
    {
        RegretSum : Vector<float>
        StrategySum : Vector<float>
    }

module InfoSet =

    let create numActions =
        {
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

module InfoSetMap =

    let getInfoSet key numActions (infoSetMap : InfoSetMap) =
        match infoSetMap |> Map.tryFind key with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet, infoSetMap
            | None ->
                let infoSet = InfoSet.create numActions
                let infoSetMap = infoSetMap |> Map.add key infoSet
                infoSet, infoSetMap

type IGameState<'priv, 'action> =
    interface
        abstract member NumPlayers : int
        abstract member CurrentPlayerIdx : int
        abstract member Infos : int (*iPlayer*) -> seq<'priv>
        abstract member Key : string
        abstract member TerminalValuesOpt : Option<Vector<float>>
        abstract member LegalActions : 'action[]
        abstract member AddAction : 'action -> IGameState<'priv, 'action>
    end

let getCounterFactualReach (probs : Vector<_>) iPlayer =
    seq {
        for i = 0 to probs.Count - 1 do
            if i <> iPlayer then
                yield probs.[i]
    } |> Seq.fold (*) 1.0

let cfr infoSetMap (initialState : IGameState<_, _>) =

    let rec loop infoSetMap (reaches : Vector<_>) (gameState : IGameState<_, _>) =

        match gameState.TerminalValuesOpt with
            | Some values -> values, infoSetMap
            | _ ->

                let infoSet, infoSetMap =
                    infoSetMap
                        |> InfoSetMap.getInfoSet
                            gameState.Key
                            gameState.LegalActions.Length
                let strategy, infoSet =
                    infoSet
                        |> InfoSet.getStrategy
                            reaches.[gameState.CurrentPlayerIdx]
                // no need to add the modified info set back into the map yet, because it shouldn't be visited again recursively

                let counterFactualValues, infoSetMaps =
                    gameState.LegalActions
                        |> Seq.indexed
                        |> Seq.scan (fun (_, accMap) (ia, action) ->
                            let nextState = gameState.AddAction(action)
                            let reaches =
                                reaches
                                    |> Vector.mapi (fun ip reach ->
                                        if ip = gameState.CurrentPlayerIdx then
                                            reach * strategy.[ia]
                                        else
                                            reach)
                            loop accMap reaches nextState)
                                (DenseVector.ofArray Array.empty, infoSetMap)
                        |> Seq.toArray
                        |> Array.unzip
                assert(counterFactualValues.Length = gameState.LegalActions.Length + 1)
                let counterFactualValues = counterFactualValues.[1..] |> DenseMatrix.ofRowSeq
                let infoSetMap = infoSetMaps |> Array.last

                let nodeValues = strategy * counterFactualValues
                let infoSet =
                    let cfReach = getCounterFactualReach reaches gameState.CurrentPlayerIdx
                    let regretSum =
                        infoSet.RegretSum
                            |> Vector.mapi (fun ia oldRegret ->
                                let regret = counterFactualValues.[ia, gameState.CurrentPlayerIdx] - nodeValues.[gameState.CurrentPlayerIdx]
                                oldRegret + (cfReach * regret))
                    { infoSet with RegretSum = regretSum }
                let infoSetMap = infoSetMap |> Map.add gameState.Key infoSet
                nodeValues, infoSetMap

    loop infoSetMap (DenseVector.create initialState.NumPlayers 1.0) initialState

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

type Card =
    | Jack = 11
    | Queen = 12
    | King = 13

type Action =
    | Check = 0
    | Bet = 1

type KuhnPokerState(cards : Card[(*iPlayer*)], actions : Action[]) =

    let currentPlayerIdx =
        actions.Length % 2

    let actionString =
        actions
            |> Array.map (function
                | Action.Check -> 'c'
                | Action.Bet -> 'b'
                | _ -> failwith "Unexpected action")
            |> String

    let key =
        let cardChar =
            match cards.[currentPlayerIdx] with
                | Card.Jack ->  'J'
                | Card.Queen -> 'Q'
                | Card.King ->  'K'
                | _ -> failwith "Unexpected card"
        sprintf "%c%s" cardChar actionString

    let terminalValuesOpt =
        match actionString with
            | "cbc" ->   // player 1 wins ante only
                Some [| -1; 1 |]
            | "bc" ->    // player 0 wins ante only
                Some [| 1; -1 |]
            | "cc" ->    // no bets: high card wins ante only
                let sign = compare cards.[0] cards.[1]
                Some [| sign * 1; sign * -1 |]
            | "cbb" ->   // two bets: high card wins ante and bet
                let sign = compare cards.[1] cards.[0]
                Some [| sign * -2; sign * 2 |]
            | "bb" ->    // two bets: high card wins ante and bet
                let sign = compare cards.[0] cards.[1]
                Some [| sign * 2; sign * -2 |]
            | _ -> None
            |> Option.map (Array.map float >> DenseVector.ofArray)

    do
        assert(cards.Length = 2)

    interface IGameState<Card, Action> with
        member __.NumPlayers = 2
        member __.CurrentPlayerIdx = currentPlayerIdx
        member __.Infos(iPlayer) = cards.[iPlayer] |> Seq.replicate 1
        member __.Key = key
        member __.TerminalValuesOpt = terminalValuesOpt
        member __.LegalActions = Enum.getValues<Action>
        member __.AddAction(action) =
            let actions' =
                [| yield! actions; yield action |]
            KuhnPokerState(cards, actions') :> _

    static member Create(cards) = KuhnPokerState(cards, Array.empty)

[<EntryPoint>]
let main argv =

    let deck = Enum.getValues<Card>
    let numIterations = 100000
    let rng = Random(0)
    let accUtils, infoSetMap =
        ((DenseVector.zero 2, Map.empty), [|1..numIterations|])
            ||> Seq.fold (fun (accUtils, accMap) _ ->
                let cards = knuthShuffle rng deck |> Array.take 2
                let state = KuhnPokerState.Create(cards)
                let utils, accMap = cfr accMap state
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
    let alpha = get "J" 1
    assert(alpha >= 0.0 && alpha <= 1.0/3.0)
    assert(abs(get "Q" 0 - 1.0) < epsilon)
    assert(abs(get "Qcb" 1 - (alpha + 1.0/3.0)) < epsilon)
    assert(abs(get "K" 1 - (3.0 * alpha)) < epsilon)

    0
