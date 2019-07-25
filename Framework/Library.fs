namespace ThingStead.Framework

[<AutoOpen>]
module Decisions = 
    let resultRailroad handleFailure f a prior = 
        match prior with
        | Success ->
            try
                f a
            with
            | ex -> ex |> Exception |> handleFailure
        | _ -> prior
    
    let railroad f a prior = resultRailroad Failure f a prior

    let setupRailroad f a prior = resultRailroad SetupFailure f a prior

    let tearDownRailroad f a prior = resultRailroad TeardownFailure f a prior

module Execution = 
    let perform { TestName = name; Function = test } =
        let name = sprintf "%s" name
        name, (test ())
        
