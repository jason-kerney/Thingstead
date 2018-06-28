namespace Thingstead.Engine.Tests.Basic

open Thingstead.Engine
open Thingstead.Types
open Thingstead.Engine.Tests.TestingTools

module NeedsToRun = 
        let path = Some "Thingstead Test Engine 'justExicute' should"

        let template = 
            { testTemplate with
                Path = path 
            }

        let tests = 
            [
            ]
