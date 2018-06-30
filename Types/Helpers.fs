namespace Thingstead.Types

[<AutoOpen>]
module Helpers = 
    let emptyEnvironment : Environment = Map.empty<string, string list>
    let testTemplate = 
            {
                Name = "[need a name for this test]"
                Path = None
                TestMethod = fun _ ->
                    "Not Yet Implimented"
                    |> Ignored
                    |> Failure
                Before = fun env -> Ok env
                After = fun _ -> Ok ()
            }

    let applyToTemplate template testMethod name = 
        { template with
            Name = name
            TestMethod = testMethod
        }

    let ``Not Yet Implimented`` = 
        "Not Yet Implimented"
        |> Ignored
        |> Failure

    let Not_Yet_Implimented = ``Not Yet Implimented``     

    let withComment comment result =
        match result with
        | Failure f -> FailureWithComment (f, comment) |> Failure
        | r -> r

    let withMessage message = withComment message    

    let stage = 
        {
            BeforeStage = (fun env _ -> Ok env)
            Steps = []
            AfterStage = (fun _ _ -> Ok ())
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
            BeforePipeline = fun env _ -> Ok env
            Stages = []
            AfterPipeline = fun _ _ -> Ok ()
        }