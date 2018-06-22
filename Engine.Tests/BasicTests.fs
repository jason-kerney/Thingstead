namespace Thingstead.Engine.Tests.Basic

open Thingstead.Engine
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module ``A test engine when running tests should`` =

    let ``run a test and return its result`` () =
        let test = 
            {
                Name = "run a test and return its result"
                Path = Some "A test engine when running tests should"
                Before = None
                Executable =  fun _ -> Success
                After = None 
            }

        [test] 
        |> justExicute
        |> shoulbBeEqualToResultsOf [{Successful = [test]; Failed = []}]



module NeedsToRun = 
    let tests = 
        [
            ``A test engine when running tests should``.``run a test and return its result``
        ]
