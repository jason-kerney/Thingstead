namespace Thingstead.Types

[<AutoOpen>]
module Helpers = 
    let testTemplate = 
            {
                Name = "[need a name for this test]"
                Path = None
                Executable = fun _ ->
                    Ignored "Not Yet Implimented" |> Failure
                Before = None
                After = None
            }

    let ``Not Yet Implimented`` = 
        "Not Yet Implimented"
        |> Ignored
        |> Failure

    let Not_Yet_Implimented = ``Not Yet Implimented``     

    let withComment comment failure = 
        FailureWithComment (failure, comment)       