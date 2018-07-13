namespace Thingstead.Engine.Tests

open Thingstead.Types

type RunResult =
    | Passing
    | Ignoring
    | Failing

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
        | Success () -> Passing
        | Failure failureType ->
            match failureType with
            | ExpectationFailure message ->
                message
                |> printFailure

                Failing
            | Ignored m -> 
                printfn "\n\tIgnored %s <%s>" (joinPathToName test.Name test.Path) m
                Ignoring
            | _ -> 
                failureType
                |> sprintf "%A"
                |> printFailure

                Failing

    let getValue key (map: Map<RunResult, int>) =
        if (map.ContainsKey key) then map.[key]
        else 0

    let runTests () =
        let tests = 
            DefaultTestExecutor.NeedsToRun.tests
            |> List.append RunTestWith.NeedsToRun.tests
            |> List.append RunTestsStepProccessTests.NeedsToRun.tests
            |> List.append EmptyStep.NeedsToRun.tests

        let resultsCounts =
            tests
            |> List.countBy runTest
            |> Map.ofList
        

        let total = tests |> List.length
        let successfulCount = resultsCounts |> getValue Passing
        let failedCount = resultsCounts |> getValue Failing
        let ignoredCount = resultsCounts |> getValue Ignoring

        printfn ""
        printfn ""
        printfn "------------------------------------------------"
        printfn "Total Tests: %d" total
        printfn "Passing: %d, \tFailed: %d, \tIgnored: %d" successfulCount failedCount ignoredCount
        printfn "------------------------------------------------"


        failedCount        
        
    [<EntryPoint>]
    let main _ = 
        let result = 
            try
                runTests ()
            with
            | e ->
                printfn "%A" e
                87

        System.Console.ReadKey (true) |> ignore
        
        result