# Counterfactual Regret Minimization

[Counterfactual Regret Minimization](https://www.quora.com/What-is-an-intuitive-explanation-of-counterfactual-regret-minimization) (usually abbreviated as "CFR") is a technique for solving [imperfect-information games](https://en.wikipedia.org/wiki/Perfect_information), which are games in which some information is hidden from players. For example, in most card games, players are dealt cards that they keep hidden from the other players. Solving such a game means finding a [Nash equilibrium](https://en.wikipedia.org/wiki/Nash_equilibrium), which is a strategy that cannot be improved further by incremental changes.

## Representing game state
Cfrm is a .NET library for applying CFR to a game of two or more players. To solve such a game, you create a concrete class that inherits from an abstract base class called `GameState<TAction>` that defines the state of the game from the current player's point of view (this state is known as an "information set" in game theory). The type parameter `TAction` defines the type of actions available in the game (e.g. playing a card, making a bet, etc.). This can be an `enum` or any other type (such as a `Card` class).

The methods of `GameState` that you must override are:
* `int CurrentPlayerIdx`
This is the 0-based index of the current player. For example, if there are four players in a game, their indexes are `0`, `1`, `2`, and `3`. Players do not necessarily play in that order, though. (E.g. In trick-taking games, the player who takes a trick typically leads on the next trick.)
* `float[] TerminalValues`
If the game is in a terminal state (i.e. the game is over), this member answers an array of "payoff" values for each player. For example, in a two-player game, if player 0 wins, she might receive 1 point, while player 1 would receive -1 point for losing, resulting in a payoff array of `[ 1, -1]` for that outcome. If the sum of the payoffs in a terminal state is always 0, then the game is "zero-sum". If the game is not over, then this member answers `null`.
* `TAction[] LegalActions`
If the game is not in a terminal state, this member answers an array of legal actions for the player whose turn it is in the current state. For example, in a poker game, these actions might be `Bet` or `Fold`, while in Bridge, the legal actions would be specific bids or cards.
* `GameState<TAction> AddAction(TAction action)`
This method advances the game to the next state by taking the given action on behalf of the current player.
* `string Key`
This member answers a string that uniquely describes the state of the game from the point of view of the current player. For example, in a card game, the unique key might contain the history of the game to this point (i.e. all the cards played so far, by all players) plus the hidden cards remaining in the current player's hand.

References:
* [Vanilla Counterfactual Regret Minimization for Engineers](https://justinsermeno.com/posts/cfr/): Walkthrough of a Python implementation of 2-player CFR
* [An Introduction to Counterfactual Regret Minimization](http://modelai.gettysburg.edu/2013/cfr/): Detailed overview of CFR with a Java implementation
* [Multiplayer CFR](https://medium.com/ai-in-plain-english/building-a-poker-ai-part-7-exploitability-multiplayer-cfr-and-3-player-kuhn-poker-25f313bf83cf): Multiplayer support [in Python](https://github.com/tt293/medium-poker-ai/blob/master/part_7/multiplayer_kuhn_poker_cfr.py).
<!--stackedit_data:
eyJoaXN0b3J5IjpbMTYyMjkxNzA1MCwxOTkwNzMzMzI3LDEwND
A3MTg3MTNdfQ==
-->