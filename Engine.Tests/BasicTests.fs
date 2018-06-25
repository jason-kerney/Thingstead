namespace Thingstead.Engine.Tests.Basic

open Thingstead.Engine
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module ``Thingstead Test Engine`` =

    module ``justExicute should`` = 
        let ``run a test and return its result`` =
            {
                Name = "run a test and return its result"
                Path = Some "Thingstead Test Engine justExicute should"
                Before = None
                Executable = fun _ ->                
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
                    
                After = None
            }


module NeedsToRun = 
    let tests = 
        [
            ``Thingstead Test Engine``.``justExicute should``.``run a test and return its result``
        ]
