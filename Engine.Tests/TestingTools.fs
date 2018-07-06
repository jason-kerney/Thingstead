namespace Thingstead.Engine.Tests

open Thingstead.Types

module TestingTools = 
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

    let isFailure<'a, 'b> = isSuccess<'a, 'b> >> not

    let asTestFailure comment : TestResult =
        comment
        |> GeneralFailure
        |> Failure