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

    let terminalValues =
        match actionString with
            | "cbc" ->   // player 1 wins ante only
                [| -1.0; 1.0 |]
            | "bc" ->    // player 0 wins ante only
                [| 1.0; -1.0 |]
            | "cc" ->    // no bets: high card wins ante only
                let sign = compare cards.[0] cards.[1] |> float
                [| sign * 1.0; sign * -1.0 |]
            | "cbb" ->   // two bets: high card wins ante and bet
                let sign = compare cards.[1] cards.[0] |> float
                [| sign * -2.0; sign * 2.0 |]
            | "bb" ->    // two bets: high card wins ante and bet
                let sign = compare cards.[0] cards.[1] |> float
                [| sign * 2.0; sign * -2.0 |]
            | _ -> null

    let legalActions =
        [| Action.Check; Action.Bet |]

    do
        Assert.AreEqual(2, cards.Length)

    interface IGameState<Card, Action> with

        member __.CurrentPlayerIdx =
            currentPlayerIdx

        member __.Infos(iPlayer) =
            cards.[iPlayer] |> Seq.replicate 1

        member __.Key =
            key

        member __.TerminalValues =
            terminalValues

        member __.LegalActions =
            legalActions

        member __.AddAction(action) =
            let actions' =
                [| yield! actions; yield action |]
            KuhnPokerState(cards, actions') :> _

    static member Create(cards) =
        KuhnPokerState(cards, Array.empty)

[<TestClass>]
type KuhnPokerTest () =

    let run numIterations delta =

        let deck = [| Card.Jack; Card.Queen; Card.King |]
        let rng = Random(0)

        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize numIterations 2 (fun () ->
                rng.Shuffle(deck)
                    |> Array.take 2
                    |> KuhnPokerState.Create)

        printfn "Expected value: %A" expectedGameValues
        for (key, strategy) in strategyProfile.Strategies do
            printfn "%s: %A" key strategy

        let path = "Kuhn.json"
        strategyProfile.Save(path)
        let strategyProfile = StrategyProfile.Load(path)

        // https://en.wikipedia.org/wiki/Kuhn_poker#Optimal_strategy
        Assert.AreEqual(expectedGameValues.[0], -1.0/18.0, delta)
        let get key i = strategyProfile.[key].[i]
        let alpha = get "J" 1
        Assert.IsTrue(alpha >= 0.0)
        Assert.IsTrue(alpha <= 1.0/3.0)
        Assert.AreEqual(get "Q" 0, 1.0, delta)
        Assert.AreEqual(get "Qcb" 1, alpha + 1.0/3.0, delta)
        Assert.AreEqual(get "K" 1, 3.0 * alpha, delta)

    [<TestMethod>]
    member __.Run() =
        run 100000 0.03
