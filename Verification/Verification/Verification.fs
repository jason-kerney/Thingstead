namespace ThingStead.Verification

open ThingStead.Core.SharedTypes

[<AutoOpen>]
module Standard =
    let andAlso check a b pastResult =
        if pastResult = Success then check a b
        else pastResult

    let expectsNotToBe expected actual = 
        if expected <> actual then Success
        else 
            expected
            |> sprintf "%A expected not to be %A" actual 
            |> ExpectationFailure 
            |> Failure

    let expectsToBe expected actual = 
        if expected = actual then Success
        else  
            expected 
            |> sprintf "%A expected to be %A" actual 
            |> ExpectationFailure 
            |> Failure
