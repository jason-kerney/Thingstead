namespace Thingstead.Engine.Tests.RunTestsStepProccessTests

open Thingstead.Engine.Steps
open Thingstead.Engine.Tests.TestingTools
open Thingstead.Types

module NeedsToRun = 
        let private path = Some "Thingstead Test Engine 'runTestsStepProccess' should"

        let private template = 
            { testTemplate with
                Path = path 
            }

        let private testedWith = applyToTemplate template

        let tests = 
            [
                // runStepProccessWith
                "Gives the tests to the test runner" 
                |> testedWith 
                    (fun _ -> 
                        let mutable result : string list = [] 

                        let tests = 
                            [
                                { testTemplate with
                                    Name = "1"
                                    TestMethod = fun _ -> successFulTest
                                }

                                { testTemplate with
                                    Name = "2"
                                    TestMethod = fun _ -> successFulTest
                                }
                            ]

                        let input = 
                            Initial tests

                        let expected = tests |> List.map toString |> List.sort

                        let runner (_: TestingEnvironment) test = 
                                result <- (test |> toString) :: result
                                Success ()

                        runStepProccessWith emptyEnvironment runner input
                        |> ignore

                        result
                        |> List.sort
                        |> shouldBeEqualTo expected
                    )

                "Passes the environment to the test runner"
                |> testedWith
                    (fun _ ->
                        let mutable result = 
                            "No Results collected"
                            |> asTestFailure

                        let testEnvironment =
                            emptyEnvironment.Add ("setup", ["this"; "is";"the";"environment"])

                        let tests = 
                            [
                                { testTemplate with
                                    Name = "1"
                                }

                                { testTemplate with
                                    Name = "2"
                                }
                            ]

                        let input = 
                            Initial tests

                        let runner (environment: TestingEnvironment) _ = 
                                result <- 
                                    environment
                                    |> shouldBeEqualTo testEnvironment

                                result

                        runStepProccessWith testEnvironment runner input
                        |> ignore

                        result
                    )
            ] 
            |> List.append [
                // runTestsStepProccess
                "Run a single passing test and return its result"
                |> testedWith 
                    (fun _ ->
                        let input = 
                            Initial [
                                { testTemplate with
                                    TestMethod = fun _ -> successFulTest
                                }
                            ]

                        let result = runTestsStepProccess emptyEnvironment input

                        match result with
                        | Success tests ->
                            tests
                            |> List.head
                            |> snd<Test, TestResult>
                            |> isSuccess
                            |> shouldBeEqualTo true
                            |> withFailComment "Test was not successful"
                        | _ -> "Tests failed" |> asTestFailure
                    )

                "Run a single passing test and return the test as part of the result"
                |> testedWith 
                    (fun _ ->
                        let test = 
                            { testTemplate with
                                TestMethod = fun _ -> successFulTest
                            }

                        let expected = test.ToString ()

                        let input = 
                            Initial [
                                test
                            ]

                        let result = runTestsStepProccess emptyEnvironment input

                        match result with
                        | Success tests ->
                            tests
                            |> List.head
                            |> fst<Test, TestResult>
                            |> fun tst -> tst.ToString ()
                            |> shouldBeEqualTo expected
                            |> withFailComment "Test was not returned"
                        | _ -> "Tests failed" |> asTestFailure
                    )

                "Run a multiple successful tests and return the results"
                |> testedWith 
                    (fun _ ->
                        let tests = [
                            { testTemplate with
                                Name = "1"
                                TestMethod = fun _ -> successFulTest
                            }

                            { testTemplate with
                                Name = "2"
                                TestMethod = fun _ -> successFulTest
                            }

                            { testTemplate with
                                Name = "3"
                                TestMethod = fun _ -> successFulTest
                            }
                        ]

                        let input = 
                            Initial tests

                        let result = runTestsStepProccess emptyEnvironment input

                        match result with
                        | Success tests ->
                            tests
                            |> List.map (fun result -> result |> snd<Test, TestResult> |> isSuccess)
                            |> shouldBeEqualTo [true; true; true]
                            |> withFailComment "unexpected test failure"
                        | _ -> "Tests failed" |> asTestFailure
                    )

                "Run a multiple successful tests and returns the tests as part of the result"
                |> testedWith 
                    (fun _ ->
                        let tests = [
                            { testTemplate with
                                Name = "1"
                                TestMethod = fun _ -> successFulTest
                            }

                            { testTemplate with
                                Name = "2"
                                TestMethod = fun _ -> successFulTest
                            }

                            { testTemplate with
                                Name = "3"
                                TestMethod = fun _ -> successFulTest
                            }
                        ]

                        let input = 
                            Initial tests

                        let expected = tests |> List.map (fun test -> test.ToString ()) |> List.sort

                        let result = runTestsStepProccess emptyEnvironment input

                        match result with
                        | Success tests ->
                            tests
                            |> List.map (fun result -> result |> fst<Test, TestResult> |> fun test -> test.ToString ())
                            |> List.sort
                            |> shouldBeEqualTo expected
                            |> withFailComment "did not recieve expected tests"
                        | _ -> "Tests failed" |> asTestFailure
                    )

                "Run multiple tests and returns the tests the results"
                |> testedWith 
                    (fun _ ->
                        let tests = [
                            { testTemplate with
                                Name = "1"
                                TestMethod = fun _ -> "Test 1 Failed" |> asTestFailure
                            }

                            { testTemplate with
                                Name = "2"
                                TestMethod = fun _ -> successFulTest
                            }

                            { testTemplate with
                                Name = "3"
                                TestMethod = fun _ -> "Test 3 Failed" |> asTestFailure
                            }
                        ]

                        let input = 
                            Initial tests

                        let expected = 
                            tests 
                            |> List.map (fun test -> (test, test.TestMethod emptyEnvironment).ToString ())
                            |> List.sort

                        let result = runTestsStepProccess emptyEnvironment input

                        match result with
                        | Success _ ->
                            "Process should not have passed" |> asTestFailure
                        | Failure results ->
                            results
                            |> List.map (fun result -> result.ToString ())
                            |> List.sort
                            |> shouldBeEqualTo expected
                    )

                "Not run when give a failed input"
                |> testedWith (fun _ -> 
                        let mutable called = false

                        let test = 
                            { testTemplate with
                                TestMethod = (fun _ ->
                                    called <- true
                                    "Should not have been called"
                                    |> asTestFailure
                                )
                            }

                        let input = PreviousFailed [test, successFulTest]

                        runTestsStepProccess emptyEnvironment input
                        |> ignore

                        called
                        |> shouldBeEqualTo false
                        |> withFailComment "Test method should not have executed"
                    )

                "Run a multiple successful tests and return the results if input comes from successful previous"
                |> testedWith 
                    (fun _ ->
                        let tests = [
                            { testTemplate with
                                Name = "1"
                                TestMethod = fun _ -> successFulTest
                            }

                            { testTemplate with
                                Name = "2"
                                TestMethod = fun _ -> successFulTest
                            }

                            { testTemplate with
                                Name = "3"
                                TestMethod = fun _ -> successFulTest
                            }
                        ]

                        let input = 
                            PreviousSucceeded (tests |> List.map (fun test -> test, "something went wrong but its okay" |> asTestFailure))

                        let result = runTestsStepProccess emptyEnvironment input

                        match result with
                        | Success tests ->
                            tests
                            |> List.map (fun result -> result |> snd<Test, TestResult> |> isSuccess)
                            |> shouldBeEqualTo [true; true; true]
                            |> withFailComment "unexpected test failure"
                        | _ -> "Tests failed" |> asTestFailure
                    )

            ]