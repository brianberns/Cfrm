namespace Cfrm.Test

open Microsoft.VisualStudio.TestTools

/// Wrapper to resolve ambiguity in VS Assert type.
type Assert =

    static member AreEqual<'t>(expected : 't, actual : 't) =
        UnitTesting.Assert.AreEqual(expected, actual)

    static member AreEqual(expected : float, actual : float, delta : float) =
        UnitTesting.Assert.AreEqual(expected, actual, delta)
