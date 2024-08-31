using System;
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
            : GameState<Action>
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

            private string History
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

            public override int CurrentPlayerIdx =>
                _actions.Length % 2;

            public override string Key =>
                $"{_cards[this.CurrentPlayerIdx].ToString()[0]}{this.History}";

            public override double[] TerminalValues
            {
                get
                {
                    int sign;
                    switch (this.History)
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

            public override Action[] LegalActions { get; } =
                new Action[] { Action.Check, Action.Bet };

            public override GameState<Action> AddAction(Action action)
            {
                var actions = _actions
                    .Concat(Enumerable.Repeat(action, 1))
                    .ToArray();
                return new KuhnPokerState(_cards, actions);
            }
        }

        [TestMethod]
        public void Minimize()
        {
            var deck = new Card[] { Card.Jack, Card.Queen, Card.King };
            var rng = new Random(0);
            var numIterations = 100000;
            var delta = 0.03;

            var (expectedGameValues, strategyProfile) =
                CounterFactualRegret.Minimize(numIterations, 2, i =>
                    {
                        rng.Shuffle(deck);
                        var cards = deck[0..2];
                        return new KuhnPokerState(cards);
                    });

            const string path = "Kuhn.strategy";
            strategyProfile.Save(path);
            strategyProfile = StrategyProfile.Load(path);

            // https://en.wikipedia.org/wiki/Kuhn_poker#Optimal_strategy
            var dict = strategyProfile.ToDict();
            Assert.AreEqual(expectedGameValues[0], -1.0 / 18.0, delta);
            var alpha = dict["J"][1];
            Assert.IsTrue(alpha >= 0.0);
            Assert.IsTrue(alpha <= 1.0 / 3.0);
            Assert.AreEqual(dict["Q"][0], 1.0, delta);
            Assert.AreEqual(dict["Qcb"][1], alpha + 1.0 / 3.0, delta);
            Assert.AreEqual(dict["K"][1], 3.0 * alpha, delta);
        }
    }
}
