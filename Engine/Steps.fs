namespace Thingstead.Engine

open Thingstead.Engine.Tests
open Thingstead.Engine.Types
open Thingstead.Types

module Steps = 
    let runTestsStepProccess (environment: TestingEnvironment) (input: StepInput) : EngineResult<(Test * TestResult) list, (Test * TestResult) list> =
        match input with
        | Initial (test::_ as tests) -> 
            tests
            |> List.map (fun _ -> test, Success ())
            |> Success

        | _ -> Failure []