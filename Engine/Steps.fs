namespace Thingstead.Engine

open Thingstead.Types

module Steps = 
    let baseStep =
        {
            BeforeStep = fun env _ -> Ok env
            Executor = fun env testMethod -> testMethod env
            AfterStep = fun _ _ -> Ok ()
        }