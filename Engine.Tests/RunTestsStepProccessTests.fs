namespace Thingstead.Engine.Tests.RunTestsStepProccessTests

open Thingstead.Engine.Tests
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
                {template with
                    Name = "Run a single passing test and return its result"
                    TestMethod = 
                        (fun _ ->
                            Success ()
                        )
                }
            ]