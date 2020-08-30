namespace Cfrm

/// Immutable representation of a game state.
type IGameState<'action> =
    interface

        /// Current player's 0-based index.
        abstract member CurrentPlayerIdx : int

        /// Unique key for this game state.
        abstract member Key : string

        /// Per-player payoffs if this is a terminal game state; null otherwise.
        abstract member TerminalValues : float[]

        /// Legal actions available in this game state.
        abstract member LegalActions : 'action[]

        /// Moves to the next game state by taking the given action.
        abstract member AddAction : 'action -> IGameState<'action>
    end

[<AutoOpen>]
module GameStateExt =

    type IGameState<'action> with

        /// Per-player payoffs if this is a terminal game state; None otherwise.
        member this.TerminalValuesOpt =
            Option.ofObj this.TerminalValues
