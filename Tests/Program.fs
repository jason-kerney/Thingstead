namespace TempRunner

open TempRunner.Utils
open ThingStead.Framework
open ThingStead.DomainLanguage.Expectations

type Test<'a> = {
    Name : string
    Function : 'a -> Results
}

module Program = 
    open Utils

    let tests = 
        [
            "Railroad should:",
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

                            Success |> railroad call 2 |> ignore

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
                            |> General |> Failure |> railroad call 2 |> ignore

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
                                Success |> railroad call 2 |> ignore
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
            "Setup railroad should:",
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

                            Success |> setupRailroad call 2 |> ignore

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
                            |> General |> Failure |> setupRailroad call 2 |> ignore

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
                                Success |> setupRailroad call 2 |> ignore
                                Success
                            with
                            | ex -> ex |> Exception |> SetupFailure 
                        )
                }
            ]
        ] |> List.collect (
            fun (suite, tests) ->
                tests
                |> List.map(
                    fun { Name = name; Function = test} ->
                        (sprintf "%s %s" suite name), test
                )
        )

    [<EntryPoint>]
    let main argv =
        let failed = 
            tests
            |> List.map (fun(name, test)->
                name, (test ())
            ) 
            |> List.filter (
                fun (_, result) ->
                    result <> Success
            )

        let report = 
            failed        
            |> List.map (
                fun (name, result) ->
                    sprintf "%s\n%A" name result
            )
            |> join

        if 0 < report.Length then
            printfn "%s\n" report

        let failedCount = failed |> List.length
        printfn "%d tests run" (tests |> List.length)
        printfn "%d tests failed" failedCount

        failedCount