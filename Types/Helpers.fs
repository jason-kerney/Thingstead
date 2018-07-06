namespace Thingstead.Types

[<AutoOpen>]
module Helpers = 
    let emptyEnvironment : TestingEnvironment = Map.empty<string, string list>

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
