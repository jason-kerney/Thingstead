namespace SolStone.Assertions

open ThingStead.Framework

[<AutoOpen>]
module Assertions = 
    let isExpectedToBe expected actual = 
        if expected = actual then
            Success
        else
            expected 
            |> sprintf "\"%A\" is not \"%A\"" actual
            |> Expectation
            |> Failure

    let isExpectedToNotBe expected actual =
        if expected = actual then
            expected
            |> sprintf "\"%A\" not \"%A\"" actual
            |> Expectation
            |> Failure

        else
            Success