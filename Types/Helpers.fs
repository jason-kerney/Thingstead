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

    let combine (resultB) (resultA) =
        match resultA, resultB with
        | Failure (BeforeFailure before), Failure (AfterFailure after)
        | Failure (AfterFailure after), Failure (BeforeFailure before) ->
            MultiFailure (before |> BeforeFailure, after |> AfterFailure)
            |> Failure
        | Failure (BeforeFailure before), _
        | _, Failure (BeforeFailure before) ->
            before
            |> BeforeFailure
            |> Failure
        | Failure (AfterFailure after), _
        | _, Failure (AfterFailure after) ->
            after
            |> AfterFailure
            |> Failure
        | Failure error, Success _
        | Success _, Failure error ->
            error
            |> Failure
        | _ -> resultA

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