namespace Cfrm.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Cfrm

[<AutoOpen>]
module RandomExt =
    type Random with

        /// Shuffles the given array in place.
        /// From http://rosettacode.org/wiki/Knuth_shuffle#F.23
        member rng.Shuffle(items : _[]) =
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
    inherit GameState<Action>()

    let currentPlayerIdx =
        actions.Length % 2

    let actionString =
        actions
            |> Array.map (fun action ->
                action.ToString().ToLower().[0])
            |> String

    let key =
        let cardChar =
            cards.[currentPlayerIdx].ToString().[0]
        sprintf "%c%s" cardChar actionString

    let terminalValuesOpt =
        match actionString with
            | "cbc" ->   // player 1 wins ante only
                Some [| -1.0; 1.0 |]
            | "bc" ->    // player 0 wins ante only
                Some [| 1.0; -1.0 |]
            | "cc" ->    // no bets: high card wins ante only
                let sign = compare cards.[0] cards.[1] |> float
                Some [| sign * 1.0; sign * -1.0 |]
            | "cbb" ->   // two bets: high card wins ante and bet
                let sign = compare cards.[1] cards.[0] |> float
                Some [| sign * -2.0; sign * 2.0 |]
            | "bb" ->    // two bets: high card wins ante and bet
                let sign = compare cards.[0] cards.[1] |> float
                Some [| sign * 2.0; sign * -2.0 |]
            | _ -> None

    let legalActions =
        [| Action.Check; Action.Bet |]

    do
        Assert.AreEqual(2, cards.Length)

    override __.CurrentPlayerIdx =
        currentPlayerIdx

    override __.Key =
        key

    override __.TerminalValuesOpt =
        terminalValuesOpt

    override __.LegalActions =
        legalActions

    override __.AddAction(action) =
        let actions' =
            [| yield! actions; yield action |]
        KuhnPokerState(cards, actions') :> _

    static member Create(cards) =
        KuhnPokerState(cards, Array.empty)

[<TestClass>]
type KuhnPokerTest () =

    let deck = [| Card.Jack; Card.Queen; Card.King |]

    let createGame (rng : Random) =
        rng.Shuffle(deck)
            |> Array.take 2
            |> KuhnPokerState.Create

    let minimize batchSize numBatches delta =

        let rng = Random(0)
        let batchNums = seq { 1 .. numBatches }
        let initialBatch =
            CfrBatch.create 2 (fun _ ->
                createGame rng)
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
        Assert.AreEqual(finalBatch.ExpectedGameValues.[0], -1.0/18.0, delta)
        let get key i = strategyProfile.Map.[key].[i]
        let alpha = get "J" 1
        Assert.IsTrue(alpha >= 0.0)
        Assert.IsTrue(alpha <= 1.0/3.0)
        Assert.AreEqual(get "Q" 0, 1.0, delta)
        Assert.AreEqual(get "Qcb" 1, alpha + 1.0/3.0, delta)
        Assert.AreEqual(get "K" 1, 3.0 * alpha, delta)

    let play rng (players : StrategyProfile[]) =
        
        let rec loop (gameState : GameState<_>) =
            match gameState.TerminalValuesOpt with
                | None ->
                    let iAction =
                        let profile = players.[gameState.CurrentPlayerIdx]
                        profile.Sample(gameState.Key, rng)
                            |> Option.get
                    gameState.LegalActions.[iAction]
                        |> gameState.AddAction
                        |> loop
                | Some values ->
                    Assert.AreEqual(2, values.Length)
                    values.[0], values.[1]

        loop (createGame rng)

    member __.Minimize(batchSize, numBatches, delta) =
        minimize batchSize numBatches delta

    [<TestMethod>]
    member this.Solve() =
        this.Minimize(10000, 10, 0.03)

    [<TestMethod>]
    member __.Play() =

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
            Array.init numIterations (fun _ ->
                profiles |> play rng |> fst)
                |> Array.sum
        let winRate = payoff0 / float numIterations
        Assert.IsTrue(winRate > 0.15, winRate.ToString())
