namespace Thingstead.Engine

open Thingstead.Types

module Steps = 
    let baseStep =
        {
            BeforeStep = fun env _ -> Ok env
            Executor = fun env testMethod -> testMethod env
            AfterStep = fun _ _ -> Ok ()
        }

    let private runTest testRunner (environment: Environment) (testMethod: Environment -> TestResult) =
        try
            testRunner environment testMethod
        with
        | e -> e |> ExceptionFailure |> Failure        

    let runStep (tests : Test list) environment (step : Step) =
        let testRunner = runTest (step.Executor) environment
        tests
        |> List.map (fun t -> t, t.TestMethod |> testRunner)
        