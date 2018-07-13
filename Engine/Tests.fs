namespace Thingstead.Engine

open Thingstead.Types
open Thingstead.Engine.Process

module Tests = 
    let defaultTestExecutor (env: TestingEnvironment) testMethod : TestResult =
        testMethod env

    let private executeTests testRunner (environment: TestingEnvironment) (testMethod: TestingEnvironment -> TestResult) =
        let runner = fun env -> testRunner env testMethod
        handleUnsafeTestAction runner environment (ExceptionFailure >> Failure)

    let runTestWith environment executor (test: Test) =
        let testFunction = (fun env -> 
            test.TestMethod |> executeTests executor env
        )
        
        bookEndProcess environment (test.Before) testFunction (test.After)
        
    let runTestWithDefaultExecutor environment test =
        test |> runTestWith environment defaultTestExecutor