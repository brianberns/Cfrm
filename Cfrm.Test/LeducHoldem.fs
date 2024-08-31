namespace Cfrm.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Cfrm
open Cfrm.Test

type LeducHoldemAction =
    | Check
    | Bet
    | Fold
    | Call
    | Raise

type Round = LeducHoldemAction[]

/// Leduc Hold'em.
module LeducHoldem =

    let numPlayers = 2

    let potSize (rounds : Round[]) =

        let betSize iRound action =
            let small = 2 * (iRound + 1)
            match action with
                | Bet | Call -> small
                | Raise -> 2 * small
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
            | [||] -> [| Check; Bet |]

                // second player's turn
            | [| Check |] -> [| Check; Bet |]
            | [| Bet |] -> [| Fold; Call; Raise |]

                // first player's second turn
            | [| Check; Check |] -> [| |]
            | [| Check; Bet |] -> [| Fold; Call; Raise |]
            | [| Bet; Raise |] -> [| Fold; Call |]
            | [| Bet; _ |] -> [| |]

                // second player's second turn
            | [| Check; Bet; Raise |] -> [| Fold; Call |]
            | [| Check; Bet; _ |]
            | [| Bet; Raise; _ |] -> [| |]

                // no more turns
            | [| Check; Bet; Raise; _ |] -> [| |]

            | _ -> failwith "Unexpected"

/// Leduc Hold'em.
type LeducHoldemState(
    playerCards : Card[(*iPlayer*)],
    communityCard : Card,
    rounds : Round[]) =
    inherit GameState<LeducHoldemAction>()

    let curRound = Array.last rounds

    let currentPlayerIdx =
        curRound.Length % LeducHoldem.numPlayers

    let history =
        rounds
            |> Array.map (fun round ->
                round
                    |> Array.map (function
                        | Check -> "Check"
                        | Bet -> "Bet"
                        | Fold -> "Fold"
                        | Call -> "Call"
                        | Raise -> "Raise")
                    |> String.concat "")
            |> String.concat "."

    let key =
        let toChar card = card.ToString()[0]
        let playerCardChar = toChar playerCards[currentPlayerIdx]
        if rounds.Length > 1 then
            let comCardChar = toChar communityCard
            sprintf "%c%c.%s" playerCardChar comCardChar history
        else
            sprintf "%c.%s" playerCardChar history

    let legalActions =
        LeducHoldem.legalActions curRound

    let terminalValuesOpt =
        if legalActions.Length = 0 then
            let size : float = LeducHoldem.potSize rounds
            match Array.last curRound with
                | Fold ->
                    let value =
                        if currentPlayerIdx = 0 then size
                        else -size
                    Some [| value; -value |]
                | Check
                | Call ->
                    let value =
                        if playerCards[0] = communityCard then
                            size
                        elif playerCards[1] = communityCard then
                            -size
                        elif playerCards[0] > playerCards[1] then
                            size
                        else
                            -size
                    Some [| value; -value |]
                | _ -> failwith $"Unexpected: {key}"
        else None

    do
        Assert.IsTrue(Seq.contains rounds.Length [1; 2])
        Assert.AreEqual(LeducHoldem.numPlayers, playerCards.Length)

    override _.CurrentPlayerIdx =
        currentPlayerIdx

    override _.Key =
        key

    override _.TerminalValuesOpt =
        match terminalValuesOpt with
            | Some values ->
                printfn "Values of %s, %A, %A: %A" history playerCards communityCard values
            | None -> ()
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
                    let roundOver =
                        curRound'
                            |> LeducHoldem.legalActions
                            |> Array.isEmpty
                    if roundOver && action <> Fold then
                        yield Array.empty
                else
                    yield rounds[0]
                    yield curRound'
            |]
        LeducHoldemState(playerCards, communityCard, rounds')

    static member Create(playerCards, communityCard) =
        LeducHoldemState(
            playerCards,
            communityCard,
            [| Array.empty |])

[<TestClass>]
type LeducHoldemTest () =

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
        LeducHoldemState.Create(deck[0..1], deck[2])

    [<TestMethod>]
    member _.Minimize() =
        let numIterations = 1
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize numIterations 2 createGame
        printfn "%A" expectedGameValues
        printfn "%A" strategyProfile.Map.Count
        for (KeyValue(key, value)) in strategyProfile.Map do
            printfn "%s: %A" key value
