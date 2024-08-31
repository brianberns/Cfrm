namespace Cfrm.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Cfrm
open Cfrm.Test

type Rank =
    | Jack = 11
    | Queen = 12
    | King = 13

type Suit =
    | Hearts = 0
    | Spades = 1

type Card =
    {
        Rank : Rank
        Suit : Suit
    }

type PokerAction =
    | Check = 0
    | Raise = 1
    | Fold = 2
    | Reraise = 3
    | Call = 4

type Round = PokerAction[]

module LeducPoker =

    let numPlayers = 2

/// Leduc poker.
type LeducPokerState(
    currentPlayerIdx,
    playerCards : Card[(*iPlayer*)],
    communityCard : Card,
    rounds : Round[]) =
    inherit GameState<PokerAction>()

    let actionString =
        rounds
            |> Array.map (fun round ->
                round
                    |> Array.map (fun action ->
                        action.ToString()[0..1])
                    |> String.concat "")
            |> String.concat "."

    let key =
        let toChar card =
            card.Rank.ToString()[0]
        let playerCardChar = toChar playerCards[currentPlayerIdx]
        if rounds.Length > 1 then
            let comCardChar = toChar communityCard
            sprintf "%c%c.%s" playerCardChar comCardChar actionString
        else
            sprintf "%c.%s" playerCardChar actionString

    let terminalValuesOpt =
        match actionString with
            | _ -> None

    let legalActions =
        [| PokerAction.Check; PokerAction.Bet |]

    do
        Assert.IsTrue(Seq.contains rounds.Length [1; 2])
        Assert.AreEqual(LeducPoker.numPlayers, playerCards.Length)

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
        LeducPokerState(playerCards, actions') :> _

    static member Create(cards) =
        LeducPokerState(cards, Array.empty)

[<TestClass>]
type LeducPokerTest () =

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
        LeducPokerState.Create(cards)

    [<TestMethod>]
    member _.Minimize() =
        let numIterations = 100000
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize numIterations 2 createGame
        printfn "%A" expectedGameValues
        printfn "%A" strategyProfile
