namespace Thingstead.Engine.Tests.BaseSteps

open Thingstead.Engine.Steps
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module NeedsToRun = 
        let private baseStepPath = Some "Thingstead Test Engine 'BaseStep' should"

        let private template = 
            { testTemplate with
                Path = baseStepPath
            }

        let testedWith = applyToTemplate template
        
        let tests = 
            [
                "that runs successfull test and returns the result"
                |> testedWith (fun _ -> 
                        let { Executor = executor; BeforeStep = _; AfterStep = _ } = baseStep

                        executor emptyEnvironment (fun _ -> Success)
                        |> shouldBeEqualTo Success
                    )

                "that runs a failed test and returns the result"
                |> testedWith (fun _ -> 
                        let expected = 
                            "this is a test failure"
                            |> GeneralFailure
                            |> Failure

                        let { Executor = executor; BeforeStep = _; AfterStep = _ } = baseStep

                        executor emptyEnvironment (fun _ -> expected)
                        |> shouldBeEqualTo expected
                    )

                "whos before step method is a pass through"
                |> testedWith (fun _ -> 
                        let { Executor = _; BeforeStep = beforeStep; AfterStep = _ } = baseStep

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        beforeStep testEnvironment testTemplate
                        |> shouldBeEqualTo (Ok testEnvironment)
                    )
                
                "whos after step method is a pass through"
                |> testedWith (fun _ -> 
                        let { Executor = _; BeforeStep = _; AfterStep = afterStep } = baseStep

                        let testEnvironment = 
                            emptyEnvironment.Add ("Hello", ["World"; "this"; "is"; "an"; "evironment"])

                        afterStep testEnvironment testTemplate
                        |> shouldBeEqualTo (Ok ())
                    )
            ]
            
