namespace TempRunner

open ThingStead.Framework
open ThingStead.DomainLanguage.Expectations

type Test<'a> = {
    Name : string
    Function : 'a -> Results
}

module Program = 
    open Utils

    let tests = [
        "Pipeline should: ",
        [
            {
                Name = "Success Calls function" 
                Function =
                    fun _ -> 
                    (
                        let mutable a = 0
                        let call x = 
                            a <- x
                            Success

                        Success |> pipeline call 2 |> ignore

                        a |> expectsToBe 2 |> withComment "Function was not called"
                    )
            }
            {
                Name = "Failure prevents function call"
                Function = 
                    fun _ ->
                    (
                        let mutable a = 0
                        let call x =
                            a <- x
                            Success

                        "This is a failure"
                        |> General |> Failure |> pipeline call 2 |> ignore

                        a |> expectsToBe 0 |> withComment "Function was called"
                    )
            }
            {
                Name = "Exception is not thrown out"
                Function =
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
            }
        ];
        "expectsToBe Should:",
        [
            {
                Name = "succeed when comparing 1 to 1"
                Function = fun _ -> 1 |> expectsToBe 1
            }
            {
                Name = "Return an expectation failure when comparing 1 to 2"
                Function = 
                    (fun _ -> 
                        match (1 |> expectsToBe 2) with
                        | Failure (Expectation "1 expected to be 2") ->
                            Success
                        | a ->
                            (sprintf "%A expected to be %A" a ("1 expected to be 2" |> Expectation |> Failure))
                            |> Expectation |> Failure
                    )
            }
            {
                Name = "Allow a comment to be added to a failure"
                Function = 
                    (fun _ ->
                        let result = 2 |> expectsToBe 1 |> withComment "This is a failure"

                        let expected =
                            ("This is a failure", "2 expected to be 1" |> Expectation) 
                            |> WithComment
                            |> Failure

                        result |> expectsToBe expected
                    )
            }
            {
                Name = "Comment is not added if expectation is met"
                Function = 
                    (fun _ ->
                        let result = "Hello" |> expectsToBe "Hello" |> withComment "This is a failure"

                        result |> expectsToBe Success
                    )
            }
        ];
    ]

    [<EntryPoint>]
    let main argv =
        let failed = 
            tests
            |> List.collect (
                fun (suite, tests) ->
                    tests 
                    |> List.map (
                        fun { Name = name; Function = tst} ->
                            suite + name, (tst ())
                    )
            )
            |> List.filter (
                fun (_, result) ->
                    result <> Success
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
