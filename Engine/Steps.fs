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
        handleUnsafeTestAction runner environment (ExceptionFailure >> Error)

    let private runAsBookend (environment: Environment) f =
        handleUnsafeTestAction f environment (PrePostExceptionFailure >> Error)

    let private runTest environment (step: Step) (test: Test) =
        let preResult = test.Before |> runAsBookend environment
        
        let after = fun environment result () ->
            let afterResult = test.After |> runAsBookend environment

            match result, afterResult with
            | _, Ok () -> result
            | Error (BeforeFailure reason), Error postError ->
                let afterFailure =
                    postError
                    |> AfterFailure

                let beforeFailure = 
                    reason
                    |> BeforeFailure

                (beforeFailure, afterFailure)
                |> MultiFailure
                |> Error

            | _, Error postError ->
                postError
                |> AfterFailure
                |> Error

        
        let result =
            match preResult with
            | Ok resultEnv -> 
                test.TestMethod
                |> executeTests (step.Executor) resultEnv
                |> after resultEnv
            | Error result -> 
                result 
                |> BeforeFailure 
                |> Error
                |> after environment


        result ()

    let runStep (tests : Test list) environment (step : Step) =
        let testExecutor = runTest environment step
        tests
        |> List.map (fun t -> t, t |> testExecutor)
        