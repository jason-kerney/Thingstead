namespace Thingstead.Engine.Tests.Basic

open Thingstead.Engine.Steps
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module NeedsToRun = 
        let private pathString = "Thingstead Test Engine 'Steps' should"
        let private path = Some pathString
        let private baseStepPath = Some (sprintf "%s %s" pathString "Provide a base step")

        let private template = 
            { testTemplate with
                Path = path 
            }
        let private baseStepTestTemplate = 
            { testTemplate with
                Path = baseStepPath
            }        

        let private basePathTests = 
            [
                { baseStepTestTemplate with
                    Name = "that runs successfull test and returns the result"
                    TestMethod = (fun _ -> 
                        let { Executor = executor; BeforeStep = _; AfterStep = _ } = baseStep

                        executor emptyEnvironment (fun _ -> Success)
                        |> shouldBeEqualTo Success
                    )
                }

                { baseStepTestTemplate with
                    Name = "that runs a failed test and returns the result"
                    TestMethod = (fun _ -> 
                        let expected = 
                            "this is a test failure"
                            |> GeneralFailure
                            |> Failure

                        let { Executor = executor; BeforeStep = _; AfterStep = _ } = baseStep

                        executor emptyEnvironment (fun _ -> expected)
                        |> shouldBeEqualTo expected
                    )
                }

                { baseStepTestTemplate with
                    Name = "whos before step method is a pass through"
                    TestMethod = (fun _ -> 
                        let { Executor = _; BeforeStep = beforeStep; AfterStep = _ } = baseStep

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        beforeStep testEnvironment testTemplate
                        |> shouldBeEqualTo (Ok testEnvironment)
                    )
                }

                { baseStepTestTemplate with
                    Name = "whos after step method is a pass through"
                    TestMethod = (fun _ -> 
                        let { Executor = _; BeforeStep = _; AfterStep = afterStep } = baseStep

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        afterStep testEnvironment testTemplate
                        |> shouldBeEqualTo (Ok ())
                    )
                }
            ]

        let tests = 
            basePathTests
            |> List.append [
                { template with
                    Name = "Pass a given test to the provided step"
                    TestMethod = (fun _ -> 
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
                }

                { template with
                    Name = "Pass a given environment to the provided step"
                    TestMethod = (fun _ -> 
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
                }

                { template with
                    Name = "Run all tests with the provided step"
                    TestMethod = (fun _ ->
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
                }
            ]
