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
    | Hearts
    | Spades

type Card =
    {
        Rank : Rank
        Suit : Suit
    }

type PokerAction =
    | Check
    | Raise
    | Fold
    | Call
    | Reraise

type Round = PokerAction[]

module LeducPoker =

    let numPlayers = 2

    let potSize (rounds : Round[]) =

        let betSize iRound action =
            let small = 2 * (iRound + 1)
            match action with
                | Raise | Call -> small
                | Reraise -> 2 * small
                | _ -> 0
        
        let ante = numPlayers
        ante + Seq.sum [
            for iRound = 0 to 1 do
                if rounds.Length > iRound then
                    rounds[iRound]
                        |> Seq.sumBy (betSize iRound)
                else 0
        ]

    let legalActions (round : Round) =
        match round with

                // first player's turn
            | [||] -> [| Check; Raise |]

                // second player's turn
            | [| Check |] -> [| Check; Raise |]
            | [| Raise |] -> [| Fold; Call; Reraise |]

                // first player's second turn
            | [| Check; Check |] -> [| |]
            | [| Check; Raise |] -> [| Fold; Call; Reraise |]
            | [| Raise; _ |] -> [| |]

                // second player's second turn (they don't really have one)
            | [| Check; Raise; _ |] -> [| |]

            | _ -> failwith "Unexpected"

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
        let roundActions =
            rounds
                |> Array.last
                |> LeducPoker.legalActions
        if roundActions.Length = 0 && rounds.Length = 1 then
            LeducPoker.legalActions Array.empty   // start second round
        else
            roundActions

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
