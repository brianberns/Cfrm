namespace Cfrm

open MathNet.Numerics.LinearAlgebra

/// Represents the set of nodes in a game-tree that are indistinguishable for
/// a given player.
type private InfoSet =
    {
        /// Per-action sum of all regrets computed so far. Positive regret
        /// indicates that we would rather have taken a different action.
        /// Negative regret indicates that we are happy with the action.
        RegretSum : Vector<float>

        /// Per-action sum of all strategies computed so far.
        StrategySum : Vector<float>
    }

module private InfoSet =

    /// Creates a new info set with the given number of legal actions.
    let create numActions =
        {
            RegretSum = DenseVector.zero numActions
            StrategySum = DenseVector.zero numActions
        }

    /// Creates a normalized strategy vector from the given values.
    let private normalize values : Strategy =
        let total = Vector.sum values
        if total > 0.0 then
            values / total   // normalize
        else
            (1.0 / float values.Count)
                |> DenseVector.create values.Count   // use uniform strategy instead

    /// Creates a new strategy for the given info set using the given
    /// reach probability.
    let getStrategy (reachProb : float) infoSet =

        assert(reachProb >= 0.0 && reachProb <= 1.0)

            // compute strategy from current regrets
        let strategy =
            infoSet.RegretSum
                |> Vector.map (max 0.0)
                |> normalize

            // accumulate strategy sum
        let infoSet' =
            { infoSet with
                StrategySum =
                    infoSet.StrategySum + (reachProb * strategy) }

        strategy, infoSet'

    /// Accumulates the given per-action regrets.
    let accumulateRegret regrets infoSet =
        { infoSet with
            RegretSum = infoSet.RegretSum + regrets }

    /// Gets the average strategy for the given info set. This converges on
    /// a Nash equilibrium.
    let getAverageStrategy infoSet =
        infoSet.StrategySum
            |> normalize 
            |> Vector.map (fun x ->
                if x < 0.001 then 0.0 else x)   // eliminate very low probability actions
            |> normalize

/// Maps keys (which typically represent game histories) to
/// known info sets.
type private InfoSetMap = Map<string (*InfoSet.Key*), InfoSet>

module private InfoSetMap =

    /// Obtains an info set for the given key, creating one if
    /// it doesn't already exist.
    let getInfoSet key numActions (infoSetMap : InfoSetMap) =
        match infoSetMap |> Map.tryFind key with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet, infoSetMap
            | None ->
                let infoSet = InfoSet.create numActions
                let infoSetMap' = infoSetMap |> Map.add key infoSet
                infoSet, infoSetMap'
