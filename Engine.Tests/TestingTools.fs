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
