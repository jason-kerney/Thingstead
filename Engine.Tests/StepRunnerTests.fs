namespace Thingstead.Engine.Tests.StepRunner

open Thingstead.Engine.Steps
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module NeedsToRun = 
        let private path = Some "Thingstead Test Engine 'stepRunner' should"

        let private template = 
            { testTemplate with
                Path = path
            }

        let private testedWith = applyToTemplate template

        let private createPassingTests count =
            seq { for i in 1..count do 
                    yield { testTemplate with 
                                Name = sprintf "Passing %d %d" i (System.DateTime.Now.Ticks)
                                TestMethod = fun _ -> successFulTest
                          }
                }
            |> List.ofSeq

        let private createFailingTests count =
            seq { for i in 1..count do 
                    let t = (System.DateTime.Now.Ticks)
                    yield { testTemplate with 
                                Name = sprintf "Failing %d %d" i t
                                TestMethod = (fun _ -> 
                                    sprintf "%d %d Failed" i t
                                    |> asTestFailure
                                )
                          }
                }
            |> List.ofSeq

        let tests : Test list = 
            [
                // stepRunner: Step list -> Test list -> (Step * EngineResult<(Test * TestResult) list, (Test * TestResult) list>) list
                // stepRunner
                //  Runs each step with all tests until one fails then stops.
                //  When running a step, it runs the before, then the proess, then the after
                "Run each passing step and return the results"
                |> testedWith
                    (fun _ ->
                        let tests =
                            createPassingTests 3

                        let steps = [
                            { basicTestExecutionStep with
                                Name = "Step 1"
                            }
                            { basicTestExecutionStep with
                                Name = "Step 2"
                            }
                        ]

                        ``Not Yet Implimented``
                    )
            ]