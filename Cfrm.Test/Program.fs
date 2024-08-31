namespace Cfrm.Test

module Program =

    try
        LeducHoldemTest().Minimize()
    with exn ->
        printfn $"{exn.Message}"
