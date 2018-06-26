namespace Thingstead.Engine.Tests.Basic

open Thingstead.Engine
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module NeedsToRun = 
        let path = Some "Thingstead Test Engine 'justExicute' should"

        let template = 
            { testTemplate with
                Path = path 
            }

        let tests = 
            [
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

                {template with
                    Name = "run a successfultest and a failing test and give results"
                    Executable = (fun _ -> 
                        let passingTest = 
                            { template with
                                Name = "run a test and return its result"
                                Executable =  fun _ -> Success
                            }

                        let failure = 
                            "Something does not equal"
                            |> ExpectationFailure

                        let failingTest = 
                            { template with
                                Name = "run a test and return its result"
                                Executable =  fun _ -> 
                                    failure
                                    |> Failure
                            }

                        [failingTest; passingTest]
                        |> justExicute
                        |> shoulbBeEqualToResultsOf 
                            [{Successful = [passingTest]; Failed = [(failingTest, failure)]}]
                    )
                }
            ]
