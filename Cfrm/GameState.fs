namespace Cfrm

/// Immutable representation of a game state.
type IGameState<'priv, 'action> =
    interface

        /// Current player's 0-based index.
        abstract member CurrentPlayerIdx : int

        /// Per-player private information.
        abstract member Infos : int (*iPlayer*) -> seq<'priv>

        /// Unique key for this game state.
        abstract member Key : string

        /// Per-player payoffs if this is a terminal game state; null otherwise.
        abstract member TerminalValues : float[]

        /// Legal actions available in this game state.
        abstract member LegalActions : 'action[]

        /// Moves to the next game state by taking the given action.
        abstract member AddAction : 'action -> IGameState<'priv, 'action>
    end
