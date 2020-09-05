# Counterfactual Regret Minimization

Counterfactual Regret Minimization (usually abbreviated as "CFR") is a technique for solving imperfect-information games. "Imperfect information" means players have different information about the game. For example, in most card games, players are dealt cards that they keep hidden from the other players. Solving such a game means finding a Nash equilibrium, which is a strategy that cannot be improved further by incremental changes.

Cfrm is a .NET framework for applying CFR to a game of two or more players. To solve such a game, you define a concrete class that inherits from an abstract base class called `GameState<TAction>` that defines the behavior of the game. The type parameter `TAction` describes the type of actions available in the game (e.g. playing a card, making a bet, etc.).

The methods of `GameState` that you must override are:
* `int CurrentPlayerIdx`
This is 0-based index of the current player. So if there are four players in a game, their indexes are `0`, `1`, `2`, and `3`.
* `float[] TerminalValues`
If the game is in a terminal state (i.e. the game is over), this member answers an array of "payoff" values for each player. For example, in a two-player game, if player 0 wins, she might receive 1 point, while player 1 would receive -1 point for losing, resulting in a payoff array of `[ 1, -1]` for that outcome. If the sum of the payoffs in all terminal states is 0, then the game is "zero-sum". If the game is not over, then this member answers `null`.
* `TAction[] LegalActions`
If the game is not in a terminal state, this member 

References:
* [Vanilla Counterfactual Regret Minimization for Engineers](https://justinsermeno.com/posts/cfr/): Walkthrough of a Python implementation of 2-player CFR
* [An Introduction to Counterfactual Regret Minimization](http://modelai.gettysburg.edu/2013/cfr/): Detailed overview of CFR with a Java implementation
* [Multiplayer CFR](https://medium.com/ai-in-plain-english/building-a-poker-ai-part-7-exploitability-multiplayer-cfr-and-3-player-kuhn-poker-25f313bf83cf): Multiplayer support [in Python](https://github.com/tt293/medium-poker-ai/blob/master/part_7/multiplayer_kuhn_poker_cfr.py).
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTEyNDIzNzkyNDQsMTA0MDcxODcxM119
-->