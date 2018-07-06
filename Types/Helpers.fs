namespace Thingstead.Types

[<AutoOpen>]
module Helpers = 
    let emptyEnvironment : Environment = Map.empty<string, string list>

    let successFulTest : TestResult = Success ()
    
    let testTemplate = 
            {
                Name = "[need a name for this test]"
                Path = None
                TestMethod = fun _ ->
                    "Not Yet Implimented"
                    |> Ignored
                    |> Failure
                Before = fun env -> Success env
                After = fun _ -> Success ()
            }

    let applyToTemplate template testMethod name = 
        { template with
            Name = name
            TestMethod = testMethod
        }

    let ``Not Yet Implimented`` : TestResult = 
        "Not Yet Implimented"
        |> Ignored
        |> Failure 

    let Not_Yet_Implimented = ``Not Yet Implimented``     

    let withFailComment comment result =
        match result with
        | Failure f -> FailureWithComment (f, comment) |> Failure
        | r -> r

    let withFailMessage message = withFailComment message

    let stage = 
        {
            BeforeStage = (fun env _ -> Success env)
            Steps = []
            AfterStage = (fun _ _ -> Success ())
            Filter = (fun input -> 
                    match input with
                    | Tests tests -> tests
                    | Results (tests, _) -> tests
                )
        }

    let pipeline = 
        {
            Name = None
            Tests = []
            BeforePipeline = fun env _ -> Success env
            Stages = []
            AfterPipeline = fun _ _ -> Success ()
        }