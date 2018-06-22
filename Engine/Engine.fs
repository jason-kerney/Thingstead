namespace Thingstead.Engine

open Thingstead.Types

[<AutoOpen>]
module Executer = 
    let justExicute tests = 
        [{
            Successful = tests
            Failed = []
        }]
