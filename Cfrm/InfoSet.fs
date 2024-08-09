namespace Cfrm

open MathNet.Numerics.LinearAlgebra

/// Represents the set of nodes in a game tree that are indistinguishable
/// for a given player.
type InfoSet =
    {
        /// Per-action sum of all regrets computed so far. Positive regret
        /// for an action indicates that we would rather take that action.
        /// ("Regret" is a confusing term for this concept.)
        RegretSum : Vector<float>

        /// Per-action sum of all strategies computed so far.
        StrategySum : Vector<float>
    }

module InfoSet =

    /// Creates a new info set with the given number of legal actions.
    let create numActions =
        {
            RegretSum = DenseVector.zero numActions
            StrategySum = DenseVector.zero numActions
        }

    /// Creates a normalized strategy vector from the given cumulative
    /// values.
    let private normalize values =
        assert(Vector.forall (fun x -> x >= 0.0) values)
        let total = Vector.sum values
        if total > 0.0 then
            values / total               // normalize
        else
            (1.0 / float values.Count)   // use uniform strategy when regret is negative
                |> DenseVector.create values.Count

    /// Computes the current strategy for the given info set from its
    /// accumulated positive regrets.
    let getStrategy infoSet =
        infoSet.RegretSum
            |> Vector.map (max 0.0)
            |> normalize

    /// Accumulates the given per-action regrets and strategy.
    let accumulate regrets strategy infoSet =
        { infoSet with
            RegretSum = infoSet.RegretSum + regrets
            StrategySum = infoSet.StrategySum + strategy }

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
type InfoSetMap = Map<string (*InfoSet.Key*), InfoSet>

module InfoSetMap =

    /// Obtains an info set for the given key, creating one if
    /// it doesn't already exist.
    let getInfoSet key numActions (infoSetMap : InfoSetMap) =
        match infoSetMap |> Map.tryFind key with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet
            | None ->
                InfoSet.create numActions

    /// Creates a strategy profile from the given info set map.
    let toStrategyProfile (infoSetMap : InfoSetMap) =
        infoSetMap
            |> Map.map (fun _ infoSet ->
                let strategy =
                    infoSet
                        |> InfoSet.getAverageStrategy
                        |> Vector.toArray
                assert(strategy.Length > 1)
                strategy)
            |> StrategyProfile
