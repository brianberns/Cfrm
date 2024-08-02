namespace Cfrm.Test

open Cfrm

module Program =

    let createGame i =
        let cards =
            match i % 6 with
                | 0 -> [| Card.Jack; Card.Queen |]
                | 1 -> [| Card.Jack; Card.King |]
                | 2 -> [| Card.Queen; Card.Jack |]
                | 3 -> [| Card.Queen; Card.King |]
                | 4 -> [| Card.King; Card.Jack |]
                | 5 -> [| Card.King; Card.Queen |]
                | _ -> failwith "Unexpected"
        KuhnPokerState.Create(cards)

    [<EntryPoint>]
    let main argv =
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize
                100000
                KuhnPoker.numPlayers
                createGame

        printfn "Expected game values:"
        for i = 0 to expectedGameValues.Length - 1 do
            printfn $"Player {i}: {expectedGameValues[i]}"

        printfn ""
        for (KeyValue(key, value)) in strategyProfile.Map do
            printfn $"{key}: {Seq.toList value}"

        0
