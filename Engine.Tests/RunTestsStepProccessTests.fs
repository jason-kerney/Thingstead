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
            ]