namespace Thingstead.Engine

open Thingstead.Engine.Tests
open Thingstead.Engine.Types
open Thingstead.Types

module Steps = 
    let runStepProccessWith (environment: TestingEnvironment) testRunner (input: StepInput) : EngineResult<(Test * TestResult) list, (Test * TestResult) list> =
        match input with
        | Initial tests -> 
            let results = 
                tests
                |> List.map (fun test -> test, testRunner emptyEnvironment test)

            let hasFailures =
                results
                |> List.exists (fun (_, result) -> match result with | Failure _ -> true | Success _ -> false)

            if hasFailures then Failure results
            else Success results
        | PreviousFailed result -> Failure result

    let runTestsStepProccess (environment: TestingEnvironment) (input: StepInput) : EngineResult<(Test * TestResult) list, (Test * TestResult) list> =
        input |> runStepProccessWith environment runTestWithDefaultExecutor