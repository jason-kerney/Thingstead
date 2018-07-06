namespace Thingstead.Engine.Tests.RunTestWith

open Thingstead.Engine.Tests
open Thingstead.Engine.Steps
open Thingstead.Engine.Tests.TestingTools
open Thingstead.Engine.Types
open Thingstead.Types

module NeedsToRun = 
        let private path = Some "Thingstead Test Engine 'runStep' should"

        let private template = 
            { testTemplate with
                Path = path 
            }

        let testedWith = applyToTemplate template

        let tests = 
            [
                // Running Tests

                "Pass a given test to the provided step"
                |> testedWith (fun _ -> 
                        let mutable result = 
                            "No Result Collected"
                            |> GeneralFailure
                            |> Error

                        let test = 
                            {testTemplate with
                                TestMethod = fun _ -> (Ok ())
                            }

                        let executor = 
                            (fun _ givenTestMethod ->
                                let expectedTestMethod = test.TestMethod.ToString ()
                                let resultTestMethod = givenTestMethod.ToString ()

                                result <-
                                    expectedTestMethod
                                    |> shouldBeEqualTo resultTestMethod

                                result
                            )

                        runTestWith emptyEnvironment executor test
                        |> ignore

                        result
                    )

                "Pass a given environment to the provided step"
                |> testedWith (fun _ -> 
                        let mutable result = 
                            "No Result Collected"
                            |> GeneralFailure
                            |> Error

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let step = 
                            (fun givenEnvironment _ ->
                                result <-
                                    givenEnvironment
                                    |> shouldBeEqualTo testEnvironment

                                result
                            )

                        runTestWith testEnvironment step template
                        |> ignore

                        result
                    )
            ]
            |> List.append [
                // Handling the Before Tests

                "Runs the Before of each test, just before running that test method"
                |> testedWith (fun _ ->
                        let mutable beforeA = false

                        let buildTest (wasCalled: bool ref) name = 
                            { testTemplate with
                                Name = name
                                Before = (fun env -> 
                                    wasCalled := true
                                    Ok env
                                )
                                TestMethod = (fun _ ->
                                    !wasCalled
                                    |> shouldBeEqualTo true
                                )
                            }


                        let tests = buildTest (ref beforeA) "Test A"

                        runTestWith emptyEnvironment (baseStep.Executor) tests
                        |> (fun (result) -> result = (Ok ()))
                        |> shouldBeEqualTo true
                        |> withFailMessage "Did not call the before on the tests"
                    )

                "Passes the environment given to it to the Before"
                |> testedWith (fun _ ->
                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let mutable result =
                            "No results collected"
                            |> GeneralFailure
                            |> Error

                        let test = 
                            { testTemplate with
                                Before = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment
                                    
                                    Ok env
                                )
                            }

                        runTestWith testEnvironment (baseStep.Executor) test
                        |> ignore

                        result
                        |> shouldBeEqualTo (Ok ())
                    )

                "Passes the environment returned from the Before to the test"
                |> testedWith (fun _ ->
                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let test = 
                            { testTemplate with
                                Before = (fun _ -> Ok testEnvironment)
                                TestMethod = (fun env ->
                                    env
                                    |> shouldBeEqualTo testEnvironment
                                )
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                    )

                "Not call the test if the before fails"
                |> testedWith (fun _ ->
                        let mutable called = false
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = (fun _ ->
                                    called <- true
                                    "This should never happen"
                                    |> GeneralFailure
                                    |> Error
                                )
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> ignore

                        called
                        |> shouldBeEqualTo false
                        |> withFailComment "Test method should not have been called"
                    )

                "return a Before Error if the before fails"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> (Ok ())
                            }

                        runTestWith emptyEnvironment baseStep.Executor test
                        |> shouldBeEqualTo ("Before Failed" |> PrePostSimpleFailure |> BeforeFailure |> Error)
                    )

                "return a Before Error if the before throws an exception"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                Before = (fun _ -> failwith "Before BOOM")
                                TestMethod = fun _ -> (Ok ())
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> fun (result) -> 
                            match result with
                            | Error (BeforeFailure (PrePostExceptionFailure e)) ->
                                e.Message
                                |> sprintf "PrePostExceptionFailure <%s>"
                                |> GeneralFailure
                                |> Error
                            | result -> result

                        |> shouldBeEqualTo ("PrePostExceptionFailure <Before BOOM>" |> GeneralFailure |> Error)
                    )
            ]
            |> List.append [
                // Handling the After test

                "Runs the After of each test, just before running that test method"
                |> testedWith (fun _ ->
                        let mutable afterA = false
                        let mutable resultA = false

                        let buildTest (markCalled: unit -> unit) (getCalled : unit -> bool) (markResult : bool -> unit) name = 
                            { testTemplate with
                                Name = name
                                After = (fun _ -> 

                                    markResult (getCalled ())
                                    Ok ()
                                )
                                TestMethod = (fun _ ->
                                    markCalled ()
                                    (Ok ())
                                )
                            }

                        let test = 
                            buildTest 
                                (fun () -> afterA <- true) 
                                (fun () -> afterA) 
                                (fun called -> resultA <- called) 
                                "Test A"

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> ignore

                        resultA
                        |> shouldBeEqualTo true
                        |> withFailComment "the After was not called"
                    )

                "Passes the environment given to it to the After"
                |> testedWith (fun _ ->
                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let mutable result =
                            "No results collected"
                            |> GeneralFailure
                            |> Error

                        let test = 
                            { testTemplate with
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment
                                    
                                    Ok ()
                                )
                            }

                        runTestWith testEnvironment (baseStep.Executor) test
                        |> ignore

                        result
                        |> shouldBeEqualTo (Ok ())
                    )

                "Passes the environment returned from the Before to the After"
                |> testedWith (fun _ ->
                        let mutable result = 
                            "No Results collect"
                            |> GeneralFailure
                            |> Error
                            
                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let test = 
                            { testTemplate with
                                Before = (fun _ -> Ok testEnvironment)
                                TestMethod = (fun _ -> (Ok ()))
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment

                                    Ok ()
                                )
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> ignore

                        result
                    )

                "Calls the after even if the before fails"
                |> testedWith (fun _ ->
                        let mutable called = false
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> (Ok ())
                                After = (fun _ ->
                                    called <- true
                                    Ok ()
                                )
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> ignore

                        called
                        |> shouldBeEqualTo true
                        |> withFailComment "Test After should have been called"
                    )

                "Calls the after with intial environment if before fails"
                |> testedWith (fun _ ->
                        let mutable result = 
                            "No results collected"
                            |> GeneralFailure
                            |> Error

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> (Ok ())
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment

                                    Ok ()
                                )
                            }

                        runTestWith testEnvironment (baseStep.Executor) test
                        |> ignore

                        result
                    )

                "return an After Error if the After fails"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                After = fun _ -> "After Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> (Ok ())
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> shouldBeEqualTo 
                            (
                                "After Failed" 
                                |> PrePostSimpleFailure 
                                |> AfterFailure 
                                |> Error
                            )
                    )

                "return an After Error if the After throws an exception"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                After = (fun _ -> failwith "After BOOM")
                                TestMethod = fun _ -> (Ok ())
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> fun (result) -> 
                            match result with
                            | Error (AfterFailure (PrePostExceptionFailure e)) ->
                                e.Message
                                |> sprintf "PrePostExceptionFailure <%s>"
                                |> GeneralFailure
                                |> Error
                            | result -> result
                        |> shouldBeEqualTo ("PrePostExceptionFailure <After BOOM>" |> GeneralFailure |> Error)
                    )
            ]
            |> List.append [
                "if both Before and After Fail, they are both returned"
                |> testedWith (fun _ -> 
                        let beforeFailure = "Bad Before" |> PrePostSimpleFailure
                        let afterFailure = "After is dead" |> PrePostSimpleFailure

                        let expectedFailure = 
                            MultiFailure ((beforeFailure |> BeforeFailure), (afterFailure |> AfterFailure))
                            |> Error
                            
                        let test = 
                            { testTemplate with
                                Before = fun _ -> beforeFailure |> Error
                                After = fun _ -> afterFailure |> Error
                            }

                        runTestWith emptyEnvironment (baseStep.Executor) test
                        |> shouldBeEqualTo expectedFailure
                    )
            ]
