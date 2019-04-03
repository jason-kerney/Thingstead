namespace ThingStead.Engines.SolStone

[<AutoOpen>]
module Decisions = 
    let pipeline f a prior =
        match prior with
        | Success ->
            try
                f a
            with
            | ex -> ex |> Exception |> Failure
        | _ -> prior
