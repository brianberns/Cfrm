namespace Cfrm.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Cfrm
open Cfrm.Test

type Card =
    | Jack = 11
    | Queen = 12
    | King = 13

type KuhnPokerAction =
    | Check = 0
    | Bet = 1

module KuhnPoker =

    let numPlayers = 2

/// Kuhn poker.
type KuhnPokerState(cards : Card[(*iPlayer*)], actions : KuhnPokerAction[]) =
    inherit GameState<KuhnPokerAction>()

    let currentPlayerIdx =
        actions.Length % KuhnPoker.numPlayers

    let actionString =
        actions
            |> Array.map (fun action ->
                action.ToString().ToLower()[0])
            |> String

    let key =
        let cardChar =
            cards[currentPlayerIdx].ToString()[0]
        sprintf "%c%s" cardChar actionString

    let terminalValuesOpt =
        match actionString with
            | "cbc" ->   // player 1 wins ante only
                Some [| -1.0; 1.0 |]
            | "bc" ->    // player 0 wins ante only
                Some [| 1.0; -1.0 |]
            | "cc" ->    // no bets: high card wins ante only
                let sign = compare cards[0] cards[1] |> float
                Some [| sign * 1.0; sign * -1.0 |]
            | "cbb" ->   // two bets: high card wins ante and bet
                let sign = compare cards[1] cards[0] |> float
                Some [| sign * -2.0; sign * 2.0 |]
            | "bb" ->    // two bets: high card wins ante and bet
                let sign = compare cards[0] cards[1] |> float
                Some [| sign * 2.0; sign * -2.0 |]
            | _ -> None

    let legalActions =
        [| KuhnPokerAction.Check; KuhnPokerAction.Bet |]

    do
        Assert.AreEqual(KuhnPoker.numPlayers, cards.Length)

    override _.CurrentPlayerIdx =
        currentPlayerIdx

    override _.Key =
        key

    override _.TerminalValuesOpt =
        terminalValuesOpt

    override _.LegalActions =
        legalActions

    override _.AddAction(action) =
        let actions' =
            [| yield! actions; yield action |]
        KuhnPokerState(cards, actions') :> _

    static member Create(cards) =
        KuhnPokerState(cards, Array.empty)

[<TestClass>]
type KuhnPokerTest () =

    let createGame i =
        let cards =
            match i % 6 with
                | 0 -> [| Card.Jack; Card.Queen |]
                | 1 -> [| Card.Jack; Card.King |]
                | 2 -> [| Card.Queen; Card.Jack |]
                | 3 -> [| Card.Queen; Card.King |]
                | 4 -> [| Card.King; Card.Jack |]
                | 5 -> [| Card.King; Card.Queen |]
                | _ -> failwith "Unexpected"
        KuhnPokerState.Create(cards)

    let minimize batchSize numBatches delta =

        let batchNums = seq { 1 .. numBatches }
        let initialBatch =
            CfrBatch.create KuhnPoker.numPlayers createGame
        let finalBatch =
            (initialBatch, batchNums)
                ||> Seq.fold (fun inBatch _ ->
                    let outBatch =
                        inBatch
                            |> CounterFactualRegret.minimizeBatch batchSize
                    printfn "Expected value: %A" outBatch.ExpectedGameValues
                    outBatch)

        for (KeyValue(key, strategy)) in finalBatch.StrategyProfile.Map do
            printfn "%s: %A" key strategy

        let path = "Kuhn.tmp.strategy"
        finalBatch.StrategyProfile.Save(path)
        let strategyProfile = StrategyProfile.Load(path)

        // https://en.wikipedia.org/wiki/Kuhn_poker#Optimal_strategy
        Assert.AreEqual(finalBatch.ExpectedGameValues[0], -1.0/18.0, delta)
        let get key i = strategyProfile.Map[key][i]
        let alpha = get "J" 1
        Assert.IsTrue(alpha >= 0.0)
        Assert.IsTrue(alpha <= 1.0/3.0)
        Assert.AreEqual(get "Q" 0, 1.0, delta)
        Assert.AreEqual(get "Qcb" 1, alpha + 1.0/3.0, delta)
        Assert.AreEqual(get "K" 1, 3.0 * alpha, delta)

        let path = "Kuhn.tmp.batch"
        finalBatch |> CfrBatch.save path
        let finalBatch' = CfrBatch.load path finalBatch.GetInitialState
        Assert.AreEqual(finalBatch.Utilities, finalBatch'.Utilities)
        Assert.AreEqual(finalBatch.NumIterations, finalBatch'.NumIterations)
        Assert.AreEqual(finalBatch.InfoSetMap, finalBatch'.InfoSetMap)

    let play rng i (players : StrategyProfile[]) =
        
        let rec loop (gameState : GameState<_>) =
            match gameState.TerminalValuesOpt with
                | None ->
                    let iAction =
                        let profile = players[gameState.CurrentPlayerIdx]
                        profile.Sample(gameState.Key, rng)
                            |> Option.get
                    gameState.LegalActions[iAction]
                        |> gameState.AddAction
                        |> loop
                | Some values ->
                    Assert.AreEqual(KuhnPoker.numPlayers, values.Length)
                    values[0], values[1]

        loop (createGame i)

    member _.Minimize(batchSize, numBatches, delta) =
        minimize batchSize numBatches delta

    [<TestMethod>]
    member this.Solve() =
        this.Minimize(100000, 10, 0.005)

    [<TestMethod>]
    member _.Play() =

        let nashProfile = StrategyProfile.Load("Kuhn.strategy")
        let randomProfile =
            nashProfile.Map
                |> Map.toSeq
                |> Seq.map (fun (key, strategy) ->
                    let uniform =
                        let len = strategy.Length
                        Array.replicate len (1.0 / float len)
                    key, uniform)
                |> Map
                |> StrategyProfile
        let profiles =
            [| nashProfile; randomProfile |]

        let rng = Random(0)
        let numIterations = 100000
        let payoff0 =
            Array.init numIterations (fun i ->
                profiles |> play rng i |> fst)
                |> Array.sum
        let winRate = payoff0 / float numIterations
        Assert.IsTrue(winRate > 0.15, winRate.ToString())
