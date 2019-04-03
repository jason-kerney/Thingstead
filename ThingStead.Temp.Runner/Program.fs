namespace TempRunner
// Learn more about F# at http://fsharp.org

module Program = 
    open Utils
    open ThingStead.Engines.SolStone

    let tests = [
        (
            "Success Calls function", 
            fun _ -> 
            (
                let mutable a = 0
                let call x = 
                    a <- x
                    Success

                Success |> pipeline call 2 |> ignore

                if a = 2 then Success
                else "Function was not called" |> General |> Failure
            )
        )
        (
            "Failure prevents function call",
            fun _ ->
            (
                let mutable a = 0
                let call x =
                    a <- x
                    Success

                "This is a failure"
                |> General |> Failure |> pipeline call 2 |> ignore

                if a = 0 then Success
                else "Function was called" |> General |> Failure
            )
        )
        (
            "Exception is not thrown out",
            fun _ -> 
            (
                let call x =
                    failwith "Bang"

                try
                    Success |> pipeline call 2 |> ignore
                    Success
                with
                | ex -> ex |> Exception |> Failure 
            )
        )
    ]

    [<EntryPoint>]
    let main argv =
        let failed = 
            tests
            |> List.map (
                fun (name, tst) -> 
                    name, (tst ())
            )
            |> List.filter (
                fun (_, result) -> result <> Success
            )

        failed        
        |> List.iter (
            fun (name, result) ->
                printfn "%s" name
                printfn "%A" result
        )

        printfn "%d tests run" (tests |> List.length)
        printfn "%d tests failed" (failed |> List.length)

        0 // return an integer exit code
