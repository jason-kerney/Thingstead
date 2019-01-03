namespace Thingstead.Engine.Tests

open Thingstead.Types

module TestingTools = 
    let toString thing  = thing.ToString ()
    
    let teePrint message value = 
        printfn "%s" message
        value

    let teePrintValue message value = 
        teePrint (sprintf "%s %A" message value) value    

    let failTest expected actual = 
        sprintf "expected <%A> got <%A>" expected actual
        |> ExpectationFailure
        |> Failure

    let shouldBeEqualTo expected actual = 
        if expected = actual then Success ()
        else failTest expected actual

    let isSuccess = function
        | Success _ -> true
        | Failure _ -> false

    let isFailure result =
        isSuccess result
        |> not 

    let asTestFailure comment : TestResult =
        comment
        |> GeneralFailure
        |> Failure

    let ignoreTest test =
        {test with
            TestMethod = fun _ -> "Actively Ignored" |> Ignored |> Failure
        }

    let getStepTests = function
        | Initial tests -> tests
        | PreviousSucceeded result
        | PreviousFailed result ->
            result |> List.map fst