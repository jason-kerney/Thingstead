namespace SolStone.Core

open SolStone.Core.SharedTypes

module Verification =
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
