namespace Thingstead.Engine.Tests.RunStep

open Thingstead.Engine.Steps
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools
open Thingstead.Engine.Types

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
                                TestMethod = fun _ -> Success
                            }

                        let step = 
                            { baseStep with
                                Executor = (fun _ givenTestMethod ->
                                    let expectedTestMethod = test.TestMethod.ToString ()
                                    let resultTestMethod = givenTestMethod.ToString ()

                                    result <-
                                        expectedTestMethod
                                        |> shouldBeEqualTo resultTestMethod

                                    result
                                )
                            }

                        runStep [test] emptyEnvironment step
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
                            {baseStep with
                                Executor = (fun givenEnvironment _ ->
                                    result <-
                                        givenEnvironment
                                        |> shouldBeEqualTo testEnvironment

                                    result
                                )
                            }

                        runStep [template] testEnvironment step
                        |> ignore

                        result
                    )

                "Run all tests with the provided step"
                |> testedWith (fun _ ->
                        let mutable count = 0

                        let buildTestMethod result : Environment -> TestResult = 
                            (fun _ -> 
                                count <- count + 1
                                result
                            )

                        let successfulTestMethod = buildTestMethod Success
                        let failureTestMethod = buildTestMethod ("This is a failure" |> GeneralFailure |> Failure)

                        let tests = [
                            { testTemplate with
                                Name = "Test 1"
                                TestMethod = failureTestMethod
                            }
                            { testTemplate with
                                Name = "Test 2"
                                TestMethod = successfulTestMethod
                            }
                            { testTemplate with
                                Name = "Test 3"
                                TestMethod = successfulTestMethod
                            }
                        ]

                        runStep tests emptyEnvironment baseStep
                        |> ignore

                        count
                        |> shouldBeEqualTo tests.Length
                    )

                "Run all tests, even with exceptions, with the provided step"
                |> testedWith (fun _ ->
                        let mutable count = 0

                        let buildTestMethod getResult : Environment -> TestResult = 
                            (fun _ -> 
                                count <- count + 1
                                getResult ()
                            )

                        let successfulTestMethod = buildTestMethod (fun () -> Success)
                        let failureTestMethod = buildTestMethod (fun () -> failwith "This is an exception")

                        let tests = [
                            { testTemplate with
                                Name = "Test 1"
                                TestMethod = failureTestMethod
                            }
                            { testTemplate with
                                Name = "Test 2"
                                TestMethod = successfulTestMethod
                            }
                            { testTemplate with
                                Name = "Test 3"
                                TestMethod = successfulTestMethod
                            }
                        ]

                        runStep tests emptyEnvironment baseStep
                        |> ignore

                        count
                        |> shouldBeEqualTo tests.Length
                    )

                "Run all tests with the provided step and return the results"
                |> testedWith (fun _ ->
                        let buildTestMethod getResult : Environment -> TestResult = 
                            (fun _ -> 
                                getResult ()
                            )

                        let successfulTestMethod = buildTestMethod (fun () -> Success)
                        let throwingTestMethod = buildTestMethod (fun () -> failwith "This is an exception")
                        let failingTestMethod = buildTestMethod (fun () -> "This is a failure" |> GeneralFailure |> Failure)

                        let tests = [
                            { testTemplate with
                                Name = "Test 1"
                                TestMethod = throwingTestMethod
                            }
                            { testTemplate with
                                Name = "Test 2"
                                TestMethod = successfulTestMethod
                            }
                            { testTemplate with
                                Name = "Test 3"
                                TestMethod = failingTestMethod
                            }
                        ]

                        let results = 
                            runStep tests emptyEnvironment baseStep
                            |> List.map (fun (test, result) ->
                                let result = 
                                    match result with
                                    | Failure (ExceptionFailure e) ->
                                        e.Message
                                        |> sprintf "ExceptionFailure <%s>"
                                        |> GeneralFailure
                                        |> Failure
                                    | other -> other

                                test.Name, result
                            )

                        results
                        |> shouldBeEqualTo [
                            "Test 1", "ExceptionFailure <This is an exception>" |> GeneralFailure |> Failure
                            "Test 2", Success
                            "Test 3", "This is a failure" |> GeneralFailure |> Failure
                        ]
                    )
            ]
            |> List.append [
                // Handling the Before Tests

                "Runs the Before of each test, just before running that test method"
                |> testedWith (fun _ ->
                        let mutable beforeA = false
                        let mutable beforeB = false
                        let mutable beforeC = false

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


                        let tests = [
                            buildTest (ref beforeA) "Test A"
                            buildTest (ref beforeB) "Test B"
                            buildTest (ref beforeC) "Test C"
                        ]

                        runStep tests emptyEnvironment baseStep
                        |> List.map (fun (_, result) -> result = Success)
                        |> List.reduce (&&)
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
                                    
                                    Ok env
                                )
                            }

                        runStep [test] testEnvironment baseStep
                        |> ignore

                        result
                        |> shouldBeEqualTo Success
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

                        runStep [test] emptyEnvironment baseStep
                        |> List.head
                        |> fun (_, result) -> result
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
                                    |> Failure
                                )
                            }

                        runStep [test] emptyEnvironment baseStep
                        |> ignore

                        called
                        |> shouldBeEqualTo false
                        |> withFailComment "Test method should not have been called"
                    )

                "return a Before Failure if the before fails"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> Success
                            }

                        runStep [test] emptyEnvironment baseStep
                        |> List.head
                        |> fun (_, result) -> result
                        |> shouldBeEqualTo ("Before Failed" |> PrePostSimpleFailure |> BeforeFailure |> Failure)
                    )

                "return a Before failure if the before throws an exception"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                Before = (fun _ -> failwith "Before BOOM")
                                TestMethod = fun _ -> Success
                            }

                        runStep [test] emptyEnvironment baseStep
                        |> List.head
                        |> fun (_, result) -> 
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

                "Runs the After of each test, just before running that test method"
                |> testedWith (fun _ ->
                        let mutable afterA = false
                        let mutable resultA = false
                        let mutable afterB = false
                        let mutable resultB = false
                        let mutable afterC = false
                        let mutable resultC = false

                        let buildTest (markCalled: unit -> unit) (getCalled : unit -> bool) (markResult : bool -> unit) name = 
                            { testTemplate with
                                Name = name
                                After = (fun _ -> 

                                    markResult (getCalled ())
                                    Ok ()
                                )
                                TestMethod = (fun _ ->
                                    markCalled ()
                                    Success
                                )
                            }

                        let tests = [
                            buildTest (fun () -> afterA <- true) (fun () -> afterA) (fun called -> resultA <- called) "Test A"
                            buildTest (fun () -> afterB <- true) (fun () -> afterB) (fun called -> resultB <- called) "Test B"
                            buildTest (fun () -> afterC <- true) (fun () -> afterC) (fun called -> resultC <- called) "Test C"
                        ]

                        runStep tests emptyEnvironment baseStep
                        |> ignore

                        resultA
                        |> (&&) resultB
                        |> (&&) resultC
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
                                    
                                    Ok ()
                                )
                            }

                        runStep [test] testEnvironment baseStep
                        |> ignore

                        result
                        |> shouldBeEqualTo Success
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
                                Before = (fun _ -> Ok testEnvironment)
                                TestMethod = (fun _ -> Success)
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment

                                    Ok ()
                                )
                            }

                        runStep [test] emptyEnvironment baseStep
                        |> ignore

                        result
                    )

                "Calls the after even if the before fails"
                |> testedWith (fun _ ->
                        let mutable called = false
                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> Success
                                After = (fun _ ->
                                    called <- true
                                    Ok ()
                                )
                            }

                        runStep [test] emptyEnvironment baseStep
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
                            |> Failure

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        let test = 
                            { testTemplate with
                                Before = fun _ -> "Before Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> Success
                                After = (fun env ->
                                    result <-
                                        env
                                        |> shouldBeEqualTo testEnvironment

                                    Ok ()
                                )
                            }

                        runStep [test] testEnvironment baseStep
                        |> ignore

                        result
                    )

                "return an After Failure if the After fails"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                After = fun _ -> "After Failed" |> PrePostSimpleFailure |> Error
                                TestMethod = fun _ -> Success
                            }

                        runStep [test] emptyEnvironment baseStep
                        |> List.head
                        |> fun (_, result) -> result
                        |> shouldBeEqualTo ("After Failed" |> PrePostSimpleFailure |> AfterFailure |> Failure)
                    )

                "return an After failure if the After throws an exception"
                |> testedWith (fun _ ->
                        let test = 
                            { testTemplate with
                                After = (fun _ -> failwith "After BOOM")
                                TestMethod = fun _ -> Success
                            }

                        runStep [test] emptyEnvironment baseStep
                        |> List.head
                        |> fun (_, result) -> 
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
                                Before = fun _ -> beforeFailure |> Error
                                After = fun _ -> afterFailure |> Error
                            }

                        runStep [test] emptyEnvironment baseStep
                        |> List.head
                        |> snd
                        |> shouldBeEqualTo expectedFailure
                    )
            ]
