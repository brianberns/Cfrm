// http://modelai.gettysburg.edu/2013/cfr/
// https://github.com/tt293/medium-poker-ai/blob/master/part_7/multiplayer_kuhn_poker_cfr.py

namespace Cfrm

open MathNet.Numerics.LinearAlgebra

type private InfoSet =
    {
        RegretSum : Vector<float>
        StrategySum : Vector<float>
    }

module private InfoSet =

    let create numActions =
        {
            RegretSum = DenseVector.zero numActions
            StrategySum = DenseVector.zero numActions
        }

    let private normalize strategy =
        let total = Vector.sum strategy
        if total > 0.0 then
            strategy / total
        else
            (1.0 / float strategy.Count)
                |> DenseVector.create strategy.Count   // uniform distribution

    let getStrategy (reach : float) infoSet =
        let strategy =
            infoSet.RegretSum
                |> Vector.map (max 0.0)
                |> normalize
        let infoSet =
            { infoSet with
                StrategySum = infoSet.StrategySum + (reach * strategy) }
        strategy, infoSet

    let getAverageStrategy infoSet =
        infoSet.StrategySum
            |> normalize 
            |> Vector.map (fun x ->
                if x < 0.001 then 0.0 else x)   // eliminate very low probability actions
            |> normalize

type private InfoSetMap = Map<string, InfoSet>

module private InfoSetMap =

    let getInfoSet key numActions (infoSetMap : InfoSetMap) =
        match infoSetMap |> Map.tryFind key with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet, infoSetMap
            | None ->
                let infoSet = InfoSet.create numActions
                let infoSetMap = infoSetMap |> Map.add key infoSet
                infoSet, infoSetMap
