namespace Cfrm

open MathNet.Numerics.LinearAlgebra

module CounterFactualRegret =

    /// Computes counterfactual reach probability.
    let private getCounterFactualReachProb (probs : Vector<_>) iPlayer =
        let prod vector = vector |> Vector.fold (*) 1.0
        (prod probs.[0 .. iPlayer-1]) * (prod probs.[iPlayer+1 ..])

    /// Main CFR loop.
    let rec private loop infoSetMap reachProbs (gameState : IGameState<_>) =

        match gameState.TerminalValuesOpt with

                // game is still in progress
            | None ->
                let legalActions = gameState.LegalActions
                match legalActions.Length with
                    | 0 -> failwith "No legal actions"
                    | 1 ->   // trivial case
                        let nextState = gameState.AddAction(legalActions.[0])
                        loop infoSetMap reachProbs nextState
                    | _ -> cfrCore infoSetMap reachProbs gameState legalActions

                // game is over
            | Some values ->
                DenseVector.ofArray values, infoSetMap

    /// Core CFR algorithm.
    and private cfrCore infoSetMap (reachProbs : Vector<_>) gameState legalActions =

            // obtain info set for this game state
        let infoSet, infoSetMap =
            infoSetMap
                |> InfoSetMap.getInfoSet gameState.Key legalActions.Length

            // update strategy for this player in this info set
        let iCurPlayer = gameState.CurrentPlayerIdx
        let strategy, infoSet =
            infoSet |> InfoSet.getStrategy reachProbs.[iCurPlayer]

            // recurse for each legal action
        let counterFactualValues, infoSetMaps =
            legalActions
                |> Seq.indexed
                |> Seq.scan (fun (_, accMap) (iAction, action) ->
                    let nextState = gameState.AddAction(action)
                    let reachProbs =
                        reachProbs
                            |> Vector.mapi (fun iPlayer reach ->
                                if iPlayer = iCurPlayer then
                                    reach * strategy.[iAction]
                                else
                                    reach)
                    loop accMap reachProbs nextState)
                        (DenseVector.ofArray Array.empty, infoSetMap)
                |> Seq.toArray
                |> Array.unzip
        assert(counterFactualValues.Length = legalActions.Length + 1)
        let counterFactualValues =
            counterFactualValues.[1..] |> DenseMatrix.ofRowSeq
        let infoSetMap = infoSetMaps |> Array.last

            // value of current game state is counterfactual values weighted
            // by action probabilities
        let result = strategy * counterFactualValues
        assert(result.Count = reachProbs.Count)

            // accumulate regret
        let infoSet =
            let cfReachProb =
                getCounterFactualReachProb reachProbs iCurPlayer
            let regrets =
                cfReachProb *
                    (counterFactualValues.[0.., iCurPlayer] - result.[iCurPlayer])
            infoSet |> InfoSet.accumulateRegret regrets

        let infoSetMap = infoSetMap |> Map.add gameState.Key infoSet
        result, infoSetMap

    /// Runs CFR minimization for the given number of iterations.
    let minimize numIterations numPlayers getInitialState =

            // accumulate utilties
        let utilities, infoSetMap =
            let initUtils = DenseVector.zero numPlayers
            let iterations = seq { 1 .. numIterations }
            ((initUtils, Map.empty), iterations)
                ||> Seq.fold (fun (accUtils, accMap) _ ->
                    let utils, accMap =
                        getInitialState ()
                            |> loop accMap (DenseVector.create numPlayers 1.0)
                    accUtils + utils, accMap)

            // compute expected game values and equilibrium strategies
        let expectedGameValues = utilities / (float) numIterations
        let strategyProfile =
            infoSetMap
                |> Map.map (fun _ infoSet ->
                    let strategy =
                        infoSet
                            |> InfoSet.getAverageStrategy
                            |> Vector.toArray
                    assert(strategy.Length > 1)
                    strategy)
                |> StrategyProfile
        expectedGameValues.ToArray(), strategyProfile

/// C# support.
[<AbstractClass; Sealed>]
type CounterFactualRegret private () =

    /// Runs CFR minimization for the given number of iterations.
    static member Minimize(numIterations, numPlayers, getInitialState) =
        let getInitialStateF =
            FuncConvert.FromFunc<IGameState<_>>(getInitialState)
        CounterFactualRegret.minimize
            numIterations
            numPlayers
            getInitialStateF
