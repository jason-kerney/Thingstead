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

    let ``Not Yet Implimented`` = 
        "Not Yet Implimented"
        |> Ignored
        |> Failure

    let Not_Yet_Implimented = ``Not Yet Implimented``     

    let withComment comment failure = 
        FailureWithComment (failure, comment)

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