namespace Cfrm.Test

open System.Diagnostics

module Program =

    [<EntryPoint>]
    let main argv =
        let test = KuhnPokerTest()
        let stopwatch = Stopwatch()
        stopwatch.Start()
        test.Minimize()
        stopwatch.Stop()
        printfn "%A" stopwatch.Elapsed
        0
