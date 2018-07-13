namespace Thingstead.Engine

open Thingstead.Engine.Tests
open Thingstead.Types

module Steps = 
    let private processInput processor input =
        match input with
        | Initial tests -> 
            processor tests
        | PreviousFailed result -> Failure result
        | PreviousSucceeded result ->
            result
            |> List.map fst
            |> processor

    let runStepProccessWith (environment: TestingEnvironment) testRunner (input: StepInput) : EngineResult<(Test * TestResult) list, (Test * TestResult) list> =
        let execute = testRunner environment
        let processTests tests = 
            let results = 
                tests
                |> List.map (fun test -> test, execute test)

            let hasFailures =
                results
                |> List.exists (fun (_, result) -> match result with | Failure _ -> true | Success _ -> false)

            if hasFailures then Failure results
            else Success results

        processInput processTests input

    let runTestsStepProccess (environment: TestingEnvironment) (input: StepInput) : EngineResult<(Test * TestResult) list, (Test * TestResult) list> =
        input |> runStepProccessWith environment runTestWithDefaultExecutor

    let blankStep = 
        {
            Name = "A Blank Step"
            BeforeStep = fun environment -> Success environment
            StepProcess = (fun _ input ->
                processInput (fun tests -> tests |> List.map (fun test -> test, Success ()) |> Success) input
            )
            AfterStep = fun _ -> Success ()
        }

    let basicTestExecutionStep = 
        {blankStep with
            StepProcess = runTestsStepProccess
        }