namespace Thingstead.Engine.Tests.Basic

open Thingstead.Engine
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module ``Thingstead Test Engine`` =

    module ``'justExicute' should`` = 
        let path = Some "Thingstead Test Engine 'justExicute' should"

        let template = 
            { testTemplate with
                Path = path 
            }

        let ``run a test and return its result`` =
            {template with
                Name = "run a test and return its result"
                Executable = fun _ ->                
                    let test = 
                        { template with
                            Name = "run a test and return its result"
                            Executable =  fun _ -> Success
                        }

                    [test] 
                    |> justExicute
                    |> shoulbBeEqualToResultsOf [{Successful = [test]; Failed = []}]
            }
            

        let ``run a failing test and return its result`` = 
            { template with
                Name = "run a failing test and return its result"
                Executable = fun _ ->
                    let failure = 
                        "Something does not equal"
                        |> ExpectationFailure

                    let test = 
                        { template with
                            Name = "run a test and return its result"
                            Executable =  fun _ -> 
                                failure
                                |> Failure
                        }

                    [test] 
                    |> justExicute
                    |> shoulbBeEqualToResultsOf [{Successful = []; Failed = [(test, failure)]}]
            }

module NeedsToRun = 
    let tests = 
        [
            ``Thingstead Test Engine``.``'justExicute' should``.``run a test and return its result``
            ``Thingstead Test Engine``.``'justExicute' should``.``run a failing test and return its result``
        ]
