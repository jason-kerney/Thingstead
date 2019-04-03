namespace ThingStead.Framework

[<AutoOpen>]
module Decisions = 
    let railroad f a prior =
        match prior with
        | Success ->
            try
                f a
            with
            | ex -> ex |> Exception |> Failure
        | _ -> prior