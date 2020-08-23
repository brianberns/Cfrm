namespace Cfrm

open MathNet.Numerics.LinearAlgebra

type IGameState<'priv, 'action> =
    interface
        abstract member CurrentPlayerIdx : int
        abstract member Infos : int (*iPlayer*) -> seq<'priv>
        abstract member Key : string
        abstract member TerminalValuesOpt : Option<float[]>
        abstract member LegalActions : 'action[]
        abstract member AddAction : 'action -> IGameState<'priv, 'action>
    end

module CounterFactualRegret =

    let private getCounterFactualReach (probs : Vector<_>) iPlayer =
        seq {
            for i = 0 to probs.Count - 1 do
                if i <> iPlayer then
                    yield probs.[i]
        } |> Seq.fold (*) 1.0

    let private minimize numPlayers infoSetMap (initialState : IGameState<_, _>) =

        let rec loop infoSetMap (reaches : Vector<_>) (gameState : IGameState<_, _>) =

            match gameState.TerminalValuesOpt with
                | Some values ->
                    DenseVector.ofArray values, infoSetMap
                | _ ->

                    let infoSet, infoSetMap =
                        infoSetMap
                            |> InfoSetMap.getInfoSet
                                gameState.Key
                                gameState.LegalActions.Length
                    let strategy, infoSet =
                        infoSet
                            |> InfoSet.getStrategy
                                reaches.[gameState.CurrentPlayerIdx]
                    // no need to add the modified info set back into the map yet, because it shouldn't be visited again recursively

                    let counterFactualValues, infoSetMaps =
                        gameState.LegalActions
                            |> Seq.indexed
                            |> Seq.scan (fun (_, accMap) (ia, action) ->
                                let nextState = gameState.AddAction(action)
                                let reaches =
                                    reaches
                                        |> Vector.mapi (fun ip reach ->
                                            if ip = gameState.CurrentPlayerIdx then
                                                reach * strategy.[ia]
                                            else
                                                reach)
                                loop accMap reaches nextState)
                                    (DenseVector.ofArray Array.empty, infoSetMap)
                            |> Seq.toArray
                            |> Array.unzip
                    assert(counterFactualValues.Length = gameState.LegalActions.Length + 1)
                    let counterFactualValues = counterFactualValues.[1..] |> DenseMatrix.ofRowSeq
                    let infoSetMap = infoSetMaps |> Array.last

                    let nodeValues = strategy * counterFactualValues
                    let infoSet =
                        let cfReach = getCounterFactualReach reaches gameState.CurrentPlayerIdx
                        let regretSum =
                            infoSet.RegretSum
                                |> Vector.mapi (fun ia oldRegret ->
                                    let regret = counterFactualValues.[ia, gameState.CurrentPlayerIdx] - nodeValues.[gameState.CurrentPlayerIdx]
                                    oldRegret + (cfReach * regret))
                        { infoSet with RegretSum = regretSum }
                    let infoSetMap = infoSetMap |> Map.add gameState.Key infoSet
                    nodeValues, infoSetMap

        loop
            infoSetMap
            (DenseVector.create numPlayers 1.0)
            initialState

    let run numIterations numPlayers createState =
        let accUtils, infoSetMap =
            ((DenseVector.zero numPlayers, Map.empty), [|1..numIterations|])
                ||> Seq.fold (fun (accUtils, accMap) _ ->
                    let state = createState ()
                    let utils, accMap = minimize numPlayers accMap state
                    accUtils + utils, accMap)
        let expectedGameValues = accUtils / (float) numIterations
        let strategyMap =
            infoSetMap
                |> Map.map (fun _ infoSet ->
                    infoSet
                        |> InfoSet.getAverageStrategy
                        |> Vector.toArray)
        expectedGameValues.ToArray(), strategyMap
