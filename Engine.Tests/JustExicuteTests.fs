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

                {template with
                    Name = "run the before, before executing the test"
                    Executable = (fun _ ->
                        let mutable wasCalled = false

                        let test = 
                            {template with
                                Before = Some (fun env -> 
                                    wasCalled <- true
                                    Ok env
                                )
                                Executable = (fun _ ->
                                    wasCalled
                                    |> shouldBeEqualTo true
                                )
                            }

                        [test]
                        |> justExicute
                        |> (fun (result::_) ->
                            match result with
                            | { Successful = []; Failed = failures } ->
                                match failures with
                                | [] -> failwith "No results given"
                                | (_, failure)::_ ->
                                    failure
                                    |> withComment "Before was not called"
                                    |> Failure
                            | { Successful = successes; Failed = _ } ->
                                match successes with
                                | [] -> failwith "No results given"
                                | _ -> Success
                        )
                    )
                }
            ]
