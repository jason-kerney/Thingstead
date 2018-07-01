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

    let bookEndProcess (environment: Environment) before (action: Environment -> TestResult) after = 
        let doIt (prevResult: Result<Environment, FailureType>) =
            match prevResult with
            | Ok env ->
                match action env with
                | Ok _ -> Ok env
                | Error result -> Error result
            | _ -> prevResult
            
        let doAfter prevResult =
            let a env = 
                match after |> runAsBookend env with
                | Ok _ -> Ok ()
                | Error error ->
                    error
                    |> AfterFailure
                    |> Error
                    
            match prevResult with
            | Ok env ->
                a env
            | Error error ->
                a environment
                |> combine (Error error)

        let doBefore env =
            match before |> runAsBookend env with
            | Ok env -> Ok env
            | Error error -> 
                error
                |> BeforeFailure
                |> Error

        environment
        |> doBefore
        |> doIt
        |> doAfter


    let private runTest environment (step: Step) (test: Test) =
        let testFunction = (fun env -> 
            test.TestMethod |> executeTests (step.Executor) env
        )
        
        bookEndProcess environment (test.Before) testFunction (test.After)

    let runStep (tests : Test list) environment (step : Step) =
        let testExecutor = runTest environment step
        tests
        |> List.map (fun t -> t, t |> testExecutor)
        