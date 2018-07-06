namespace Thingstead.Engine.Tests.DefaultTestExecutor

open Thingstead.Engine.Tests
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module NeedsToRun = 
        let private baseStepPath = Some "Thingstead Test Engine 'defaultTestExecutor' should"

        let private template = 
            { testTemplate with
                Path = baseStepPath
            }

        let testedWith = applyToTemplate template
        
        let tests = 
            [
                "that runs successfull test and returns the result"
                |> testedWith (fun _ -> 
                        defaultTestExecutor emptyEnvironment (fun _ -> (Success ()))
                        |> shouldBeEqualTo (Success ())
                    )

                "that runs a failed test and returns the result"
                |> testedWith (fun _ -> 
                        let expected = 
                            "this is a test failure"
                            |> GeneralFailure
                            |> Failure

                        defaultTestExecutor emptyEnvironment (fun _ -> expected)
                        |> shouldBeEqualTo expected
                    )
            ]
            
