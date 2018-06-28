namespace Thingstead.Types

[<AutoOpen>]
module Helpers = 
    let testTemplate = 
            {
                Name = "[need a name for this test]"
                Path = None
                Executable = fun _ ->
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

    let step = 
        {
            BeforeStep = fun env _ -> Ok env
            Executor = fun testMethod env -> testMethod env
            AfterStep = fun _ _ -> Ok ()
        }

    let pipeline = 
        {
            Name = None
            Tests = []
            BeforePipeline = fun env _ -> Ok env
            Stages = []
            AfterPipeline = fun _ _ -> Ok ()
        }