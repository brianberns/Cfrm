using System;
using System.Collections.Generic;
using System.Linq;

using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Cfrm.Test
{
    [TestClass]
    public class KuhnPoker
    {
        enum Card
        {
            Jack,
            Queen,
            King
        }

        enum Action
        {
            Check,
            Bet
        }

        class KuhnPokerState
            : IGameState<Card, Action>
        {
            public KuhnPokerState(Card[] cards)
                : this(cards, new Action[0])
            {
            }

            private KuhnPokerState(Card[] cards, Action[] actions)
            {
                Assert.AreEqual(2, cards.Length);
                _cards = cards;
                _actions = actions;
            }
            private readonly Card[] _cards;
            private readonly Action[] _actions;

            private string ActionString
            {
                get
                {
                    var chars = _actions
                        .Select(action =>
                            action.ToString().ToLower()[0])
                        .ToArray();
                    return new string(chars);
                }
            }

            public int CurrentPlayerIdx =>
                _actions.Length % 2;

            public string Key =>
                $"{_cards[this.CurrentPlayerIdx].ToString()[0]}{this.ActionString}";

            public double[] TerminalValues
            {
                get
                {
                    int sign;
                    switch (this.ActionString)
                    {
                        case "cbc":   // player 1 wins ante only
                            return new double[] { -1, 1 };
                        case "bc":    // player 0 wins ante only
                            return new double[] { 1, -1 };
                        case "cc":    // no bets: high card wins ante only
                            sign = _cards[0].CompareTo(_cards[1]);
                            return new double[] { sign * 1, sign * -1 };
                        case "cbb":   // two bets: high card wins ante and bet
                            sign = _cards[1].CompareTo(_cards[0]);
                            return new double[] { sign * -2, sign * 2 };
                        case "bb":    // two bets: high card wins ante and bet
                            sign = _cards[0].CompareTo(_cards[1]);
                            return new double[] { sign * 2, sign * -2 };
                        default: return null;
                    }
                }
            }

            public Action[] LegalActions { get; } =
                new Action[] { Action.Check, Action.Bet };

            public IEnumerable<Card> Infos(int iPlayer) =>
                Enumerable.Repeat(_cards[iPlayer], 1);

            public IGameState<Card, Action> AddAction(Action action)
            {
                var actions = _actions
                    .Concat(Enumerable.Repeat(action, 1))
                    .ToArray();
                return new KuhnPokerState(_cards, actions);
            }
        }

        /// Shuffles the given array in place.
        /// From http://rosettacode.org/wiki/Knuth_shuffle#C.23
        static T[] Shuffle<T>(Random rng, T[] array)
        {
            for (int i = 0; i < array.Length; i++)
            {
                int j = rng.Next(i, array.Length); // Don't select from the entire array on subsequent loops
                T temp = array[i]; array[i] = array[j]; array[j] = temp;
            }
            return array;
        }

        [TestMethod]
        public void Run()
        {
            var deck = new Card[] { Card.Jack, Card.Queen, Card.King };
            var rng = new Random(0);
            var numIterations = 100000;
            var delta = 0.03;

            var (expectedGameValues, infoSetMap) =
                CounterFactualRegret.Run(numIterations, 2, () =>
                    {
                        var cards = Shuffle(rng, deck)[0..2];
                        return new KuhnPokerState(cards);
                    });

            // https://en.wikipedia.org/wiki/Kuhn_poker#Optimal_strategy
            Assert.AreEqual(expectedGameValues[0], -1.0 / 18.0, delta);
            var alpha = infoSetMap["J"][1];
            Assert.IsTrue(alpha >= 0.0);
            Assert.IsTrue(alpha <= 1.0 / 3.0);
            Assert.AreEqual(infoSetMap["Q"][0], 1.0, delta);
            Assert.AreEqual(infoSetMap["Qcb"][1], alpha + 1.0 / 3.0, delta);
            Assert.AreEqual(infoSetMap["K"][1], 3.0 * alpha, delta);
        }
    }
}
