namespace Cfrm.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Cfrm

type UpDownAction = Up | Down
type LeftRightAction = Left | Right
type CoordinationAction =
    | UpDownAction of UpDownAction
    | LeftRightAction of LeftRightAction

/// https://en.wikipedia.org/wiki/Coordination_game
type CoordinationGame(upDownOpt : Option<UpDownAction>, leftRightOpt : Option<LeftRightAction>) =
    inherit GameState<CoordinationAction>()

    override __.CurrentPlayerIdx =
        match upDownOpt, leftRightOpt with
            | None, None -> 0
            | Some _, None -> 1
            | _ -> failwith "Unexpected"

    override __.Key =
        match upDownOpt, leftRightOpt with
            | None, None -> "0"
            | Some _, None -> "1"
            | Some _, Some _ -> "2"
            | _ -> failwith "Unexpected"

    override __.TerminalValuesOpt =
        match upDownOpt, leftRightOpt with
            | Some Up, Some Left
            | Some Down, Some Right -> Some [| 2.0; 4.0|]
            | Some Up, Some Right
            | Some Down, Some Left -> Some [| 1.0; 3.0 |]
            | _, None -> None
            | _ -> failwith "Unexpected"

    override __.LegalActions =
        match upDownOpt, leftRightOpt with
            | None, None -> [| UpDownAction Up; UpDownAction Down |]
            | Some _, None -> [| LeftRightAction Left; LeftRightAction Right |]
            | _ -> failwith "Unexpected"

    override __.AddAction(action) =
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
        printfn "%A" expectedGameValues
        printfn "%A" strategyProfile.Map
