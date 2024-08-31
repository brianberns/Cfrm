namespace Cfrm.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Cfrm
open Cfrm.Test

type LeducPokerAction =
    | Check
    | Raise
    | Fold
    | Call
    | Reraise

type Round = LeducPokerAction[]

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

                // prevent second player's second turn
            | [| Check; Raise; _ |] -> [| |]

            | _ -> failwith "Unexpected"

/// Leduc poker.
type LeducPokerState(
    playerCards : Card[(*iPlayer*)],
    communityCard : Card,
    rounds : Round[]) =
    inherit GameState<LeducPokerAction>()

    let curRound = Array.last rounds

    let currentPlayerIdx =
        curRound.Length % LeducPoker.numPlayers

    let actionString =
        rounds
            |> Array.map (fun round ->
                round
                    |> Array.map (fun action ->
                        action.ToString()[0..1])
                    |> String.concat "")
            |> String.concat "."

    let key =
        let toChar card = card.ToString()[0]
        let playerCardChar = toChar playerCards[currentPlayerIdx]
        if rounds.Length > 1 then
            let comCardChar = toChar communityCard
            sprintf "%c%c.%s" playerCardChar comCardChar actionString
        else
            sprintf "%c.%s" playerCardChar actionString

    let legalActions =
        LeducPoker.legalActions curRound

    let terminalValuesOpt =
        (*
        match actionString with
            | _ -> None
        *)
        if legalActions.Length = 0 then Some [| 0.0; 0.0 |]
        else None

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
        let curRound' =
            [|
                yield! curRound
                yield action
            |]
        let rounds' =
            [|
                if rounds.Length = 1 then
                    yield curRound'
                    let startSecond =
                        curRound'
                            |> LeducPoker.legalActions
                            |> Array.isEmpty
                    if startSecond then
                        yield Array.empty
                else
                    yield rounds[0]
                    yield curRound'
            |]
        LeducPokerState(playerCards, communityCard, rounds')

    static member Create(playerCards, communityCard) =
        LeducPokerState(
            playerCards,
            communityCard,
            [| Array.empty |])

[<TestClass>]
type LeducPokerTest () =

    let deck =
        [|
            Card.Jack
            Card.Queen
            Card.King
            Card.Jack
            Card.Queen
            Card.King
        |]

    let rng = Random(0)

    let createGame _ =
        rng.Shuffle(deck)
        LeducPokerState.Create(deck[0..1], deck[2])

    [<TestMethod>]
    member _.Minimize() =
        let numIterations = 10000
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize numIterations 2 createGame
        printfn "%A" expectedGameValues
        printfn "%A" strategyProfile.Map.Count
        for (KeyValue(key, value)) in strategyProfile.Map do
            printfn "%A: %A" key value
