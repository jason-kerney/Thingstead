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