namespace Thingstead.Engine.Tests

open Thingstead.Engine.Tests

  

module Runner =
    let runTest test = 
        let join (items: array<string>) = 
            System.String.Join ("\n", items)

        try
            test ()
            0
        with
        | e -> 
            let message = 
                e.Message.Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun s -> sprintf "\t%s" s)
                |> join
                
            printfn "\t###################################"
            printfn "%s" message
            printfn "\t###################################"
            
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
        
