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
    let numRounds = 2
    let ante = 1

    let investment (rounds : Round[]) playerIdx =

        let betSize iRound action =
            let small = 2 * (iRound + 1)
            match action with
                | Bet | Call -> small
                | Raise -> 2 * small
                | _ -> 0

        let sum =
            rounds
                |> Seq.indexed
                |> Seq.sumBy (fun (iRound, round) ->
                    round
                        |> Seq.indexed
                        |> Seq.where (fun (iPlay, _) ->
                            iPlay % numPlayers = playerIdx)
                        |> Seq.sumBy (fun (_, action) ->
                            betSize iRound action))
        ante + sum

    let legalActions : Round -> _ = function

            // first player's first turn
        | [||] -> [| Check; Bet |]

            // second player's first turn
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
                        | Check -> 'x'
                        | Bet -> 'b'
                        | Fold -> 'f'
                        | Call -> 'c'
                        | Raise -> 'r')
                    |> String)
            |> String.concat "d"

    let key =
        let toChar card = card.ToString()[0]
        let playerCardChar = toChar playerCards[currentPlayerIdx]
        if rounds.Length > 1 then
            let comCardChar = toChar communityCard
            sprintf "%c%c %s" playerCardChar comCardChar history
        else
            sprintf "%c %s" playerCardChar history

    let legalActions =
        LeducHoldem.legalActions curRound

    let terminalValuesOpt =
        if legalActions.Length = 0 then
            match Array.last curRound with
                | Fold ->
                    let iLoser =
                        if currentPlayerIdx = 0 then 1
                        else 0
                    let size : float = LeducHoldem.investment rounds iLoser
                    let value =
                        if currentPlayerIdx = 0 then size
                        else -size
                    Some [| value; -value |]
                | Check
                | Call ->
                    let iLoserOpt =
                        if playerCards[0] = communityCard then Some 1
                        elif playerCards[1] = communityCard then Some 0
                        elif playerCards[0] > playerCards[1] then Some 1
                        elif playerCards[0] < playerCards[1] then Some 0
                        else None
                    match iLoserOpt with
                        | Some iLoser ->
                            let size : float = LeducHoldem.investment rounds iLoser
                            let value =
                                if iLoser = 0 then -size
                                else size
                            Some [| value; -value |]
                        | None -> Some [| 0.0; 0.0 |]
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
        LeducHoldemState.Create(
            deck[0..1],
            deck[2])

    [<TestMethod>]
    member _.Minimize() =
        let numIterations = 50000
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize numIterations 2 createGame
        Assert.AreEqual(288, strategyProfile.Map.Count)
        Assert.AreEqual(-0.08, expectedGameValues[0], 0.02)
