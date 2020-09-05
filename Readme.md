# Counterfactual Regret Minimization

Counterfactual Regret Minimization (usually abbreviated as "CFR") is a technique for solving imperfect-information games. "Imperfect information" means players have different information about the game. For example, in most card games, players are dealt cards that they keep hidden from the other players. Solving such a game means finding a Nash equilibrium, which is a strategy that cannot be improved further by incremental changes.

Cfrm is a .NET framework for applying CFR to a game. It supports games with two or more players. To solve such a game, you define a concrete class that inherits from an abstract base class called `GameState` that defines the behavior of the game. The methods you must implement are:
* `int CurrentPlayerIdx`
This is 0-based index of the current player.

References:
* [Vanilla Counterfactual Regret Minimization for Engineers](https://justinsermeno.com/posts/cfr/): Walkthrough of a Python implementation of 2-player CFR
* [An Introduction to Counterfactual Regret Minimization](http://modelai.gettysburg.edu/2013/cfr/): Detailed overview of CFR with a Java implementation
* [Multiplayer CFR](https://medium.com/ai-in-plain-english/building-a-poker-ai-part-7-exploitability-multiplayer-cfr-and-3-player-kuhn-poker-25f313bf83cf): Multiplayer support [in Python](https://github.com/tt293/medium-poker-ai/blob/master/part_7/multiplayer_kuhn_poker_cfr.py).
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTEyNzA4Njg4LDEwNDA3MTg3MTNdfQ==
-->