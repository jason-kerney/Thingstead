namespace Thingstead.Engine.Tests

open Thingstead.Types

module Runner =
    let joinPathToName name path =
        let start = 
            match path with
            | None -> ""
            | Some p -> p

        sprintf "%s %s" start name

    let printFailureMessage name path message =
        printfn "\t###################################"
        printfn "\tTest: %s" (joinPathToName name path)
        printfn "\t%s" message
        printfn "\t###################################"

    let runTest (test : Test) = 
        let printFailure = printFailureMessage (test.Name) (test.Path)

        let join (items: array<string>) = 
            System.String.Join ("\n", items)

        try
            let result = Map.empty<string, string list> |>  test.TestMethod
            match result with
            | Success -> 0
            | Failure failureType ->
                match failureType with
                | ExpectationFailure message ->
                    printFailure message
                | _ -> 
                    printFailure (sprintf "%A" failureType)

                1

        with
        | e -> 
            let message = 
                e.Message.Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun s -> sprintf "\t%s" s)
                |> join
                
            printFailure message
            
            1 

    [<EntryPoint>]
    let main _ =
        let tests = 
            Basic.NeedsToRun.tests

        let failedCount =
            Basic.NeedsToRun.tests
            |> List.sumBy runTest

        let total = tests |> List.length
        let successfulCount = total - failedCount

        printfn ""
        printfn ""
        printfn "------------------------------------------------"
        printfn "Total Tests: %d" total
        printfn "Passing: %d, \tFailed: %d" successfulCount failedCount
        printfn "------------------------------------------------"

        System.Console.ReadKey (true) |> ignore

        failedCount        
        
