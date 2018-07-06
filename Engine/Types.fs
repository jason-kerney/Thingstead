namespace Thingstead.Engine.Types

open Thingstead.Types

[<AutoOpen>]
module TypeHelpers = 
    let ignoreTest reason (test: Test) = 
        { test with 
            TestMethod = fun _ -> reason |> Ignored |> Failure 
        }    

    let combine (resultB) (resultA) =
        match resultA, resultB with
        | Failure (BeforeFailure before), Failure (AfterFailure after)
        | Failure (AfterFailure after), Failure (BeforeFailure before) ->
            MultiFailure (before |> BeforeFailure, after |> AfterFailure)
            |> Failure
        | Failure (BeforeFailure before), _
        | _, Failure (BeforeFailure before) ->
            before
            |> BeforeFailure
            |> Failure
        | Failure (AfterFailure after), _
        | _, Failure (AfterFailure after) ->
            after
            |> AfterFailure
            |> Failure
        | Failure error, Success _
        | Success _, Failure error ->
            error
            |> Failure
        | _ -> resultA