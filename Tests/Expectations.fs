namespace ThingStead.DomainLanguage
open ThingStead.Framework

module Expectations =
    let withComment message result =
        let asMessageTo failType message =
            (message, failType) |> WithComment

        let failType, ctor = 
            match result with
            | Success -> None, Failure
            | Failure failType ->
                failType |> Some, Failure
            | SetupFailure failType ->
                failType |> Some, SetupFailure
            | _ -> "Unknown" |> General |> Some, Failure

        match failType with
        | None -> result
        | Some failType ->
                message 
                |> asMessageTo failType
                |> ctor

    let expectsToBe expected actual = 
        if actual = expected then Success
        else
            (sprintf "%A expected to be %A" actual expected)
            |> Expectation
            |> Failure