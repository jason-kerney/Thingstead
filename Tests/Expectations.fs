namespace ThingStead.DomainLanguage
open ThingStead.Framework

module Expectations =
    let withComment message result =
        match result with
        | Success -> result
        | Failure failType ->
            (message, failType)
            |> WithComment
            |> Failure
    let expectsToBe expected actual = 
        if actual = expected then Success
        else
            (sprintf "%A expected to be %A" actual expected)
            |> Expectation
            |> Failure