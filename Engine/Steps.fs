namespace Thingstead.Engine

open Thingstead.Types
open System

module Steps = 
    let baseStep =
        {
            BeforeStep = fun env _ -> Ok env
            Executor = fun env testMethod -> testMethod env
            AfterStep = fun _ _ -> Ok ()
        }

    let runStep (tests : Test list) environment (step : Step) =
        let testRunner = step.Executor environment
        tests
        |> List.map (fun t -> t.TestMethod |> testRunner)
        