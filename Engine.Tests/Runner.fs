namespace Thingstead.Engine.Tests

open Thingstead.Engine.Tests
open Thingstead.Types

  

module Runner =
    let printFailureMessage message =
        printfn "\t###################################"
        printfn "%s" message
        printfn "\t###################################"
    let runTest test = 
        let join (items: array<string>) = 
            System.String.Join ("\n", items)

        try
            let result = test ()
            match result with
            | Success -> 0
            | Failure failureType ->
                match failureType with
                | ExpectationFailure message ->
                    printFailureMessage message
                | _ -> printFailureMessage (sprintf "%A" failureType)

                1

        with
        | e -> 
            let message = 
                e.Message.Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun s -> sprintf "\t%s" s)
                |> join
                
            printFailureMessage message
            
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
        