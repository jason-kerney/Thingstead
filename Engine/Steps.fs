namespace Thingstead.Engine

open Thingstead.Types

module Steps = 
    let baseStep =
        {
            BeforeStep = fun env _ -> Ok env
            Executor = fun env testMethod -> testMethod env
            AfterStep = fun _ _ -> Ok ()
        }

    let private handleUnsafeTestAction unsafeAction (environment: Environment) exceptionHandler =
        try
            unsafeAction environment
        with
        | e -> e |> exceptionHandler

    let private executeTests testRunner (environment: Environment) (testMethod: Environment -> TestResult) =
        let runner = fun env -> testRunner env testMethod
        handleUnsafeTestAction runner environment (ExceptionFailure >> Failure)

    let private runAsBookend (environment: Environment) f =
        handleUnsafeTestAction f environment (PrePostExceptionFailure >> Error)

    let private runTest environment (step: Step) (test: Test) =
        let preResult = test.Before |> runAsBookend environment
        let after = fun environment result () ->
            let afterResult = test.After |> runAsBookend environment

            match afterResult with
            | Ok () -> result
            | Error postError ->
                postError
                |> AfterFailure
                |> Failure
        
        let result =
            match preResult with
            | Ok resultEnv -> 
                test.TestMethod
                |> executeTests (step.Executor) resultEnv
                |> after resultEnv
            | Error result -> 
                result 
                |> BeforeFailure 
                |> Failure
                |> after environment


        result ()

    let runStep (tests : Test list) environment (step : Step) =
        let testExecutor = runTest environment step
        tests
        |> List.map (fun t -> t, t |> testExecutor)
        