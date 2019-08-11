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
    let randomize (getIndex: int -> int) (items: 'a List) = 
        let rec walk index (arr:'a array) =
            if (arr.Length) <= index then
                arr |> Array.toList
            else
                let swap = getIndex (arr.Length)
                let h = arr.[index]
                let w = arr.[swap]
                Array.set arr index w
                Array.set arr swap h
                walk (index + 1) arr

        walk 0 (items |> List.toArray)
        
    let perform { TestName = name; Function = test } =
        let name = sprintf "%s" name
        name, (test ())
        
