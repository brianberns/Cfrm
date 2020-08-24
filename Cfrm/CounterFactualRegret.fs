namespace Cfrm

open MathNet.Numerics.LinearAlgebra

module CounterFactualRegret =

    /// Computes counterfactual reach probability.
    let private getCounterFactualReachProb (probs : Vector<_>) iPlayer =
        let prod vector = vector |> Vector.fold (*) 1.0
        (prod probs.[0 .. iPlayer-1]) * (prod probs.[iPlayer+1 ..])

    /// Minimizes regret in the given game state.
    let private cfr numPlayers infoSetMap initialState =

        let rec loop infoSetMap (reachProbs : Vector<_>) (gameState : IGameState<_, _>) =

            match gameState.TerminalValues with

                | null ->

                        // obtain info set for this game state
                    let infoSet, infoSetMap =
                        infoSetMap
                            |> InfoSetMap.getInfoSet
                                gameState.Key
                                gameState.LegalActions.Length

                        // update info set with current player's strategy
                    let iCurPlayer = gameState.CurrentPlayerIdx
                    let strategy, infoSet =
                        infoSet |> InfoSet.getStrategy reachProbs.[iCurPlayer]

                        // recurse for each legal action
                    let counterFactualValues, infoSetMaps =
                        gameState.LegalActions
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
                    assert(counterFactualValues.Length = gameState.LegalActions.Length + 1)
                    let counterFactualValues = counterFactualValues.[1..] |> DenseMatrix.ofRowSeq
                    let infoSetMap = infoSetMaps |> Array.last

                        // value of current game state is counterfactual values weighted by action probabilities
                    let result = strategy * counterFactualValues

                        // accumulate regret
                    let infoSet =
                        let cfReachProb =
                            getCounterFactualReachProb reachProbs iCurPlayer
                        let regrets =
                            cfReachProb * (counterFactualValues.[0.., iCurPlayer] - result.[iCurPlayer])
                        infoSet |> InfoSet.accumulateRegret regrets

                    let infoSetMap = infoSetMap |> Map.add gameState.Key infoSet
                    result, infoSetMap

                    // game is over
                | values ->
                    DenseVector.ofArray values, infoSetMap

        loop
            infoSetMap
            (DenseVector.create numPlayers 1.0)
            initialState

    /// Runs CFR minimization for the given number of iterations.
    let minimize numIterations numPlayers getRandomInitialState =

            // accumulate utilties
        let utilities, infoSetMap =
            let initUtils = DenseVector.zero numPlayers
            let iterations = seq { 1 .. numIterations }
            ((initUtils, Map.empty), iterations)
                ||> Seq.fold (fun (accUtils, accMap) _ ->
                    let utils, accMap =
                        getRandomInitialState () |> cfr numPlayers accMap
                    accUtils + utils, accMap)

            // compute expected game values and equilibrium strategies
        let expectedGameValues = utilities / (float) numIterations
        let strategyProfile =
            infoSetMap
                |> Map.map (fun _ infoSet ->
                    infoSet
                        |> InfoSet.getAverageStrategy
                        |> Vector.toArray)
                |> StrategyProfile
        expectedGameValues.ToArray(), strategyProfile

/// C# support.
[<AbstractClass; Sealed>]
type CounterFactualRegret private () =

    /// Runs CFR minimization for the given number of iterations.
    static member Minimize(numIterations, numPlayers, getRandomInitialState) =
        let func = FuncConvert.FromFunc<IGameState<_, _>>(getRandomInitialState)
        CounterFactualRegret.minimize numIterations numPlayers func
