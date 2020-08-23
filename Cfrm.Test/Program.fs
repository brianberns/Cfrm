namespace Cfrm.Test

open System.Diagnostics

module Program =

    [<EntryPoint>]
    let main argv =
        let test = KuhnPokerTest()
        let stopwatch = Stopwatch()
        stopwatch.Start()
        test.Run()
        stopwatch.Stop()
        printfn "%A" stopwatch.Elapsed
        0
