namespace Cfrm.Test

open System

open Microsoft.VisualStudio.TestTools.UnitTesting

open Cfrm
open Cfrm.Test

/// Possible actions in Leduc Hold'em.
type LeducHoldemAction =

    /// Passes turn to the next player.
    | Check

    /// Places a bet.
    | Bet

    /// Surrenders the pot.
    | Fold

    /// Matches the current bet.
    | Call

    /// Matches the current bet, and places a new bet.
    | Raise

/// A round is a series of actions.
type Round = LeducHoldemAction[]

/// Leduc Hold'em.
module LeducHoldem =

    let numPlayers = 2
    let numRounds = 2
    let ante = 1

    /// How much has the given player invested in the pot?
    let investment (rounds : Round[]) playerIdx =

        let betSize iRound action =
            let small = 2 * (iRound + 1)   // 2, 4, ...
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

    /// What are the legal actions at this point in the
    /// given round?
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

/// Leduc Hold'em game state.
type LeducHoldemState(
    playerCards : Card[(*iPlayer*)],
    communityCard : Card,
    rounds : Round[]) =
    inherit GameState<LeducHoldemAction>()

    /// Current round. Will be empty at the start of a round.
    let curRound = Array.last rounds

    /// Whose turn is it?
    let currentPlayerIdx =
        curRound.Length % LeducHoldem.numPlayers

    /// Info set key from the current player's point of view.
    // https://github.com/scfenton6/leduc-cfr-poker-bot
    let key =
        let history =
            rounds
                |> Array.map (
                    Array.map (function
                        | Check -> 'x'
                        | Bet -> 'b'
                        | Fold -> 'f'
                        | Call -> 'c'
                        | Raise -> 'r')
                        >> String)
                |> String.concat "d"
        let toChar card = card.ToString()[0]
        let playerCardChar = toChar playerCards[currentPlayerIdx]
        if rounds.Length > 1 then
            let comCardChar = toChar communityCard
            sprintf "%c%c %s" playerCardChar comCardChar history
        else
            sprintf "%c %s" playerCardChar history

    /// Legal actions at this point in the game.
    let legalActions =
        LeducHoldem.legalActions curRound

    /// Outcome of this game, if it is over.
    let terminalValuesOpt =
        if legalActions.Length = 0 then
            match Array.last curRound with

                    // current player wins
                | Fold ->
                    let iLoser =
                        if currentPlayerIdx = 0 then 1
                        else 0
                    let size : float =
                        LeducHoldem.investment rounds iLoser
                    let value =
                        if currentPlayerIdx = 0 then size
                        else -size
                    Some [| value; -value |]

                    // showdown
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
                            let size : float =
                                LeducHoldem.investment rounds iLoser
                            let value =
                                if iLoser = 0 then -size
                                else size
                            Some [| value; -value |]
                        | None -> Some [| 0.0; 0.0 |]
                | _ -> failwith $"Unexpected: {key}"
        else None

    do
        Assert.IsTrue(rounds.Length > 0)
        Assert.IsTrue(rounds.Length <= LeducHoldem.numRounds)
        Assert.AreEqual(LeducHoldem.numPlayers, playerCards.Length)

    override _.CurrentPlayerIdx = currentPlayerIdx
    override _.Key = key
    override _.TerminalValuesOpt = terminalValuesOpt
    override _.LegalActions = legalActions

    override _.AddAction(action) =

            // add action to the current round
        let curRound' =
            [|
                yield! curRound
                yield action
            |]

            // create next game state
        let rounds' =
            [|
                if rounds.Length = 1 then
                    yield curRound'
                    let roundOver =
                        curRound'
                            |> LeducHoldem.legalActions
                            |> Array.isEmpty
                    if roundOver && action <> Fold then   // start next round?
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

module List = 

    // http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    let rec permutations = function
        | []      -> seq [List.empty]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)
    and insertions x = function
        | []             -> [[x]]
        | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

[<TestClass>]
type LeducHoldemTest () =

    let decks =
        [
            Card.Jack
            Card.Queen
            Card.King
            Card.Jack
            Card.Queen
            Card.King
        ]
            |> List.permutations
            |> Seq.map Seq.toArray
            |> Seq.toArray

    let createGame i =
        let deck = decks[i % decks.Length]
        LeducHoldemState.Create(
            deck[0..1],
            deck[2])

    [<TestMethod>]
    member _.Minimize() =
        let numIterations = 50000
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize
                numIterations
                LeducHoldem.numPlayers
                createGame
        Assert.AreEqual(288, strategyProfile.Map.Count)
        Assert.AreEqual(-0.08, expectedGameValues[0], 0.02)
