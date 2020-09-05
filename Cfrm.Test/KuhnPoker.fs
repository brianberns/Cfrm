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

    let minimize numIterations delta =

        let rng = Random(0)
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize numIterations 2 (fun i ->
                createGame rng)

        printfn "Expected value: %A" expectedGameValues
        for (KeyValue(key, strategy)) in strategyProfile.Map do
            printfn "%s: %A" key strategy

        let path = "Kuhn.tmp.strategy"
        strategyProfile.Save(path)
        let strategyProfile = StrategyProfile.Load(path)

        // https://en.wikipedia.org/wiki/Kuhn_poker#Optimal_strategy
        Assert.AreEqual(expectedGameValues.[0], -1.0/18.0, delta)
        let get key i = strategyProfile.Map.[key].[i]
        let alpha = get "J" 1
        Assert.IsTrue(alpha >= 0.0)
        Assert.IsTrue(alpha <= 1.0/3.0)
        Assert.AreEqual(get "Q" 0, 1.0, delta)
        Assert.AreEqual(get "Qcb" 1, alpha + 1.0/3.0, delta)
        Assert.AreEqual(get "K" 1, 3.0 * alpha, delta)

    /// Selects the index of an item from the given probability distribution.
    /// E.g. [.25; .25; .5] will select index 2 half the time.
    let selectFrom (rng : Random) probs =
        Assert.IsFalse(probs |> Array.isEmpty)
        Assert.IsTrue(probs |> Array.forall (fun prob -> prob >= 0.0 && prob <= 1.0))

            // skip the last value, assuming it is 1.0 - sum of previous values
        let sums =
            [|
                yield! (0.0, probs.[0..probs.Length-2])
                    ||> Seq.scan (+)
                    |> Seq.skip 1
                yield 1.0
            |]
        assert(sums.Length = probs.Length)

        let r = rng.NextDouble()
        let i = sums |> Array.findIndex (fun prob -> prob >= r)
        assert(i < probs.Length)
        i

    let play rng (players : StrategyProfile[]) =
        
        let rec loop (gameState : GameState<_>) =
            match gameState.TerminalValuesOpt with
                | None ->
                    let iAction =
                        let profile = players.[gameState.CurrentPlayerIdx]
                        profile.Map.[gameState.Key]
                            |> selectFrom rng
                    gameState.LegalActions.[iAction]
                        |> gameState.AddAction
                        |> loop
                | Some values ->
                    Assert.AreEqual(2, values.Length)
                    values.[0], values.[1]

        loop (createGame rng)

    member __.Minimize(numIterations, delta) =
        minimize numIterations delta

    [<TestMethod>]
    member this.Solve() =
        this.Minimize(100000, 0.03)

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
        Assert.IsTrue(payoff0 / float numIterations > 0.15)
