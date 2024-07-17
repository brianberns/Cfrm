namespace Cfrm.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open Cfrm
open Cfrm.Test

type UpDownAction = Up | Down
type LeftRightAction = Left | Right
type CoordinationAction =
    | UpDownAction of UpDownAction
    | LeftRightAction of LeftRightAction

/// https://en.wikipedia.org/wiki/Coordination_game
type CoordinationGame(upDownOpt : Option<UpDownAction>, leftRightOpt : Option<LeftRightAction>) =
    inherit GameState<CoordinationAction>()

    override _.CurrentPlayerIdx =
        match upDownOpt, leftRightOpt with
            | None, None -> 0
            | Some _, None -> 1
            | _ -> failwith "Unexpected"

    override _.Key =
        match upDownOpt, leftRightOpt with
            | None, None -> "0"
            | Some _, None -> "1"
            | Some _, Some _ -> "2"
            | _ -> failwith "Unexpected"

    override _.TerminalValuesOpt =
        match upDownOpt, leftRightOpt with
            | Some Up, Some Left
            | Some Down, Some Right -> Some [| 2.0; 4.0 |]
            | Some Up, Some Right
            | Some Down, Some Left -> Some [| 1.0; 3.0 |]
            | _, None -> None
            | _ -> failwith "Unexpected"

    override _.LegalActions =
        match upDownOpt, leftRightOpt with
            | None, None -> [| UpDownAction Up; UpDownAction Down |]
            | Some _, None -> [| LeftRightAction Left; LeftRightAction Right |]
            | _ -> failwith "Unexpected"

    override _.AddAction(action) =
        match action, upDownOpt, leftRightOpt with
            | UpDownAction upDown, None, None -> CoordinationGame(Some upDown, None)
            | LeftRightAction leftRight, Some _, None -> CoordinationGame(upDownOpt, Some leftRight)
            | _ -> failwith "Unexepcted"

    static member Initial = CoordinationGame(None, None)

[<TestClass>]
type CoordinationGameTest () =

    [<TestMethod>]
    member _.Minimize() =
        let numIterations = 10000
        let expectedGameValues, strategyProfile =
            CounterFactualRegret.minimize numIterations 2 (fun _ -> CoordinationGame.Initial)

        let delta = 0.001
        Assert.AreEqual(2.0, expectedGameValues[0], delta)
        Assert.AreEqual(4.0, expectedGameValues[1], delta)

        let map = strategyProfile.Map
        Assert.AreEqual(2, map.Count)
        Assert.AreEqual(Seq.toList map["0"], Seq.toList map["1"])
