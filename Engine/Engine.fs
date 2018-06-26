namespace Thingstead.Engine

open Thingstead.Types

[<AutoOpen>]
module Executer = 

    let private teePrint message value = 
        printfn "%s" message
        value

    let private teePrintValue message value = 
        teePrint (sprintf "%s %A" message value) value 

    let justExicute tests = 
        let environment = Map.empty<string, string list>

        let results = 
            tests
            |> List.map(fun test ->
                let result = test.Executable environment
                test, result
            )
            |> List.groupBy(fun (_, result) ->
                match result with
                | Success -> "Success"
                | Failure _ -> "Failure"
            )
            |> Map.ofList

        let successes = 
            if results.ContainsKey "Success"
            then
                results.["Success"]
                |> Seq.map (fun (test, _) -> test)
                |> List.ofSeq
            else []

        let failed = 
            if results.ContainsKey "Failure"
            then
                results.["Failure"]
                |> Seq.map (fun (test, Failure failType) ->
                    test, failType
                )
                |> List.ofSeq
            else []

        [{
            Successful = successes
            Failed = failed
        }]
