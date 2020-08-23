namespace Cfrm.Test

open System
open Cfrm

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
            |> Option.map (Array.map float)

    do
        assert(cards.Length = 2)

    interface IGameState<Card, Action> with

        member __.CurrentPlayerIdx =
            currentPlayerIdx

        member __.Infos(iPlayer) =
            cards.[iPlayer] |> Seq.replicate 1

        member __.Key =
            key

        member __.TerminalValuesOpt =
            terminalValuesOpt

        member __.LegalActions =
            [| Action.Check; Action.Bet |]

        member __.AddAction(action) =
            let actions' =
                [| yield! actions; yield action |]
            KuhnPokerState(cards, actions') :> _

    static member Create(cards) =
        KuhnPokerState(cards, Array.empty)

module Program =

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

    [<EntryPoint>]
    let main argv =

        let deck = [| Card.Jack; Card.Queen; Card.King |]
        let numIterations = 100000
        let rng = Random(0)

        let expectedGameValues, infoSetMap =
            CounterFactualRegret.run numIterations 2 (fun () ->
                knuthShuffle rng deck
                    |> Array.take 2
                    |> KuhnPokerState.Create)

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
