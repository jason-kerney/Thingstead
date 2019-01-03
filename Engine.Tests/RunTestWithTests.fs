namespace Thingstead.Engine.Tests.RunTestWith

open Thingstead.Engine.Tests
open Thingstead.Engine.Tests.TestingTools
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
                            |> Failure

                        let test = 
                            {testTemplate with
                                TestMethod = fun _ -> (Success ())
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
                            |> Failure

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
                                    Success env
                                )
                                TestMethod = (fun _ ->
                                    !wasCalled
                                    |> shouldBeEqualTo true
                                )
                            }


                        let tests = buildTest (ref beforeA) "Test A"

                        runTestWith emptyEnvironment defaultTestExecutor tests
                        |> (fun (result) -> result = (Success ()))
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
                            |> Failure

                        let test = 
                            { testTemplate with
                                Before = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment
                                    
                                    Success env
                                )
                            }

                        runTestWith testEnvironment defaultTestExecutor test
                        |> ignore

                        result
                        |> shouldBeEqualTo (Success ())
                    )

                "Passes the environment returned from the Before to the test"
                |> testedWith (fun _ ->
                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let test = 
                            { testTemplate with
                                Before = (fun _ -> Success testEnvironment)
                                TestMethod = (fun env ->
                                    env
                                    |> shouldBeEqualTo testEnvironment
                                )
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                    )

                "Not call the test if the before fails"
                |> testedWith (fun _ ->
                        let mutable called = false
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Failure
                                TestMethod = (fun _ ->
                                    called <- true
                                    "This should never happen"
                                    |> GeneralFailure
                                    |> Failure
                                )
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> ignore

                        called
                        |> shouldBeEqualTo false
                        |> withFailComment "Test method should not have been called"
                    )

                "return a Before Failure if the before fails"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Failure
                                TestMethod = fun _ -> (Success ())
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> shouldBeEqualTo ("Before Failed" |> PrePostSimpleFailure |> BeforeFailure |> Failure)
                    )

                "return a Before Failure if the before throws an exception"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                Before = (fun _ -> failwith "Before BOOM")
                                TestMethod = fun _ -> (Success ())
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> fun (result) -> 
                            match result with
                            | Failure (BeforeFailure (PrePostExceptionFailure e)) ->
                                e.Message
                                |> sprintf "PrePostExceptionFailure <%s>"
                                |> GeneralFailure
                                |> Failure
                            | result -> result

                        |> shouldBeEqualTo ("PrePostExceptionFailure <Before BOOM>" |> GeneralFailure |> Failure)
                    )
            ]
            |> List.append [
                // Handling the After test

                "Runs the After of each test, just after running that test method"
                |> testedWith (fun _ ->
                        let mutable afterA = false
                        let mutable resultA = false

                        let buildTest (markCalled: unit -> unit) (getCalled : unit -> bool) (markResult : bool -> unit) name = 
                            { testTemplate with
                                Name = name
                                After = (fun _ -> 
                                    markResult (getCalled ())
                                    Success ()
                                )
                                TestMethod = (fun _ ->
                                    markCalled ()
                                    (Success ())
                                )
                            }

                        let test = 
                            buildTest 
                                (fun () -> afterA <- true) 
                                (fun () -> afterA) 
                                (fun called -> resultA <- called) 
                                "Test A"

                        runTestWith emptyEnvironment defaultTestExecutor test
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
                            |> Failure

                        let test = 
                            { testTemplate with
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment
                                    
                                    Success ()
                                )
                            }

                        runTestWith testEnvironment defaultTestExecutor test
                        |> ignore

                        result
                        |> shouldBeEqualTo (Success ())
                    )

                "Passes the environment returned from the Before to the After"
                |> testedWith (fun _ ->
                        let mutable result = 
                            "No Results collect"
                            |> GeneralFailure
                            |> Failure
                            
                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let test = 
                            { testTemplate with
                                Before = (fun _ -> Success testEnvironment)
                                TestMethod = (fun _ -> (Success ()))
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment

                                    Success ()
                                )
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> ignore

                        result
                    )

                "Calls the after even if the before fails"
                |> testedWith (fun _ ->
                        let mutable called = false
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Failure
                                TestMethod = fun _ -> (Success ())
                                After = (fun _ ->
                                    called <- true
                                    Success ()
                                )
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> ignore

                        called
                        |> shouldBeEqualTo true
                        |> withFailComment "Test After should have been called"
                    )
                    
                "Calls the after with result in the environment when before fails"
                |> testedWith (fun _ ->
                        let mutable gotCorrectResult = false
                        let expectedFailure : EngineResult<TestingEnvironment, PrePostFailureType> = "Before Failed" |> PrePostSimpleFailure |> Failure
                        let test = 
                            { testTemplate with
                                Before = fun _ -> expectedFailure
                                TestMethod = fun _ -> (Success ())
                                After = (fun environment ->
                                    if environment |> Map.containsKey "Result"
                                    then
                                        let result = environment.["Result"]
                                        match result with
                                        | :? EngineResult<TestingEnvironment, PrePostFailureType> ->
                                            let testingResult = result :?> EngineResult<TestingEnvironment, PrePostFailureType>
                                            
                                            if testingResult = expectedFailure
                                            then 
                                                gotCorrectResult <- true
                                        | _ -> gotCorrectResult <- false
                                            
                                    Success ()
                                )
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> ignore

                        gotCorrectResult
                        |> shouldBeEqualTo true
                        |> withFailComment "Test After should have been called"
                    )

                "Calls the after with intial environment if before fails"
                |> testedWith (fun _ ->
                        let mutable result = 
                            "No results collected"
                            |> GeneralFailure
                            |> Failure

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Failure
                                TestMethod = fun _ -> (Success ())
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment

                                    Success ()
                                )
                            }

                        runTestWith testEnvironment defaultTestExecutor test
                        |> ignore

                        result
                    )

                "return an After Failure if the After fails"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                After = fun _ -> "After Failed" |> PrePostSimpleFailure |> Failure
                                TestMethod = fun _ -> (Success ())
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> shouldBeEqualTo 
                            (
                                "After Failed" 
                                |> PrePostSimpleFailure 
                                |> AfterFailure 
                                |> Failure
                            )
                    )

                "return an After Failure if the After throws an exception"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                After = (fun _ -> failwith "After BOOM")
                                TestMethod = fun _ -> (Success ())
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> fun (result) -> 
                            match result with
                            | Failure (AfterFailure (PrePostExceptionFailure e)) ->
                                e.Message
                                |> sprintf "PrePostExceptionFailure <%s>"
                                |> GeneralFailure
                                |> Failure
                            | result -> result
                        |> shouldBeEqualTo ("PrePostExceptionFailure <After BOOM>" |> GeneralFailure |> Failure)
                    )
            ]
            |> List.append [
                "if both Before and After Fail, they are both returned"
                |> testedWith (fun _ -> 
                        let beforeFailure = "Bad Before" |> PrePostSimpleFailure
                        let afterFailure = "After is dead" |> PrePostSimpleFailure

                        let expectedFailure = 
                            MultiFailure ((beforeFailure |> BeforeFailure), (afterFailure |> AfterFailure))
                            |> Failure
                            
                        let test = 
                            { testTemplate with
                                Before = fun _ -> beforeFailure |> Failure
                                After = fun _ -> afterFailure |> Failure
                            }

                        runTestWith emptyEnvironment defaultTestExecutor test
                        |> shouldBeEqualTo expectedFailure
                    )
            ]
