namespace Thingstead.Engine

open Thingstead.Engine.Tests
open Thingstead.Engine.Types
open Thingstead.Types

module Steps = 
    let runTestsStepProccess (environment: TestingEnvironment) (input: StepInput) : EngineResult<(Test * TestResult) list, (Test * TestResult) list> =
        match input with
        | Initial tests -> 
            let results = 
                tests
                |> List.map (fun test -> test, test.TestMethod emptyEnvironment)

            let hasFailures =
                results
                |> List.exists (fun (_, result) -> match result with | Failure _ -> true | Success _ -> false)

            if hasFailures then Failure results
            else Success results