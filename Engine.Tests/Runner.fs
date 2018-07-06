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

        // let testMethod = defaultTestExecutor emptyEnvironment

        let result = test |> runTestWith emptyEnvironment defaultTestExecutor

        match result with
        | Ok () -> 0
        | Error failureType ->
            match failureType with
            | ExpectationFailure message ->
                message
                |> printFailure
            | Ignored m -> printfn "\n\tIgnored %s <%s>" (joinPathToName test.Name test.Path) m
            | _ -> 
                failureType
                |> sprintf "%A"
                |> printFailure

            1

    [<EntryPoint>]
    let main _ =
        let tests = 
            DefaultTestExecutor.NeedsToRun.tests
            |> List.append RunTestWith.NeedsToRun.tests

        let failedCount =
            tests
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
        
