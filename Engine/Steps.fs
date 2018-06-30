namespace Thingstead.Engine

open Thingstead.Types

module Steps = 
    let baseStep =
        {
            BeforeStep = fun env _ -> Ok env
            Executor = fun env testMethod -> testMethod env
            AfterStep = fun _ _ -> Ok ()
        }

    let private executeTests testRunner (environment: Environment) (testMethod: Environment -> TestResult) =
        try
            testRunner environment testMethod
        with
        | e -> e |> ExceptionFailure |> Failure

    let private runTest environment (step: Step) (test: Test) =
        let preResult = test.Before environment

        match preResult with
        | Ok resultEnv -> 
            test.TestMethod
            |> executeTests (step.Executor) resultEnv
        | Error result -> 
            result |> BeforeFailure |> Failure

    let runStep (tests : Test list) environment (step : Step) =
        let testExecutor = runTest environment step
        tests
        |> List.map (fun t -> t, t |> testExecutor)
        