namespace Thingstead.Tests

open System
open ``Registry Should``

module Program = 
    [<EntryPoint>]
    let main _argv =
        let tests = 
            ``behave as defined``

        tests
            |> List.map (fun (name, test) ->  name , test "hello")
            |> List.iter (fun (name, result) -> printfn "%s: %A" name result)

        printfn "Done"
        0 // return an integer exit code
