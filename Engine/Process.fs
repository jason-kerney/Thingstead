namespace Thingstead.Engine

open Thingstead.Types
open Thingstead.Engine.Types

module internal Process = 

    let handleUnsafeTestAction unsafeAction (environment: TestingEnvironment) exceptionHandler =
        try
            unsafeAction environment
        with
        | e -> e |> exceptionHandler

    let runAsBookend (environment: TestingEnvironment) f =
        handleUnsafeTestAction f environment (PrePostExceptionFailure >> Failure)

    let bookEndProcess (environment: TestingEnvironment) before action after = 
        let doBefore env =
            match before |> runAsBookend env with
            | Success env -> Success env
            | Failure error -> 
                error
                |> BeforeFailure
                |> Failure

        let doIt (prevResult: EngineResult<TestingEnvironment, FailureType>) =
            match prevResult with
            | Success env ->
                match action env with
                | Success _ -> Success env
                | Failure result -> Failure result
            | _ -> prevResult
            
        let doAfter prevResult =
            let a env = 
                let newEnvironment = env.Add ("Result", obj(prevResult))
                match after |> runAsBookend env with
                | Success _ -> Success ()
                | Failure error ->
                    error
                    |> AfterFailure
                    |> Failure
                    
            match prevResult with
            | Success env ->
                a env
            | Failure error ->
                a environment
                |> combine (Failure error)

        environment
        |> doBefore
        |> doIt
        |> doAfter