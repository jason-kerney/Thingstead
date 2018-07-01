namespace Thingstead.Engine.Types

open Thingstead.Types

[<AutoOpen>]
module TypeHelpers = 
    let ignoreTest reason (test: Test) = 
        { test with 
            TestMethod = fun _ -> reason |> Ignored |> Failure 
        }