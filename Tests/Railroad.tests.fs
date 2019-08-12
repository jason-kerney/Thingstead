namespace Tests.Framework

open Tests.Utils
open ThingStead.Framework
open ThingStead.DomainLanguage.Expectations


module Railroad = 
    let tests = 
        "Railroad should" |> asTests
            [
                {
                    TestName = "Success Calls function" 
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
                    TestName = "Failure prevents function call"
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
                    TestName = "Exception is not thrown out"
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
                {
                    TestName = "Success from method is returned"
                    Function = 
                        (fun _ ->
                            let call x =
                                Success

                            let result = railroad call 2
                            Success |> result |> expectsToBe Success
                        )
                }
                {
                    TestName = "Failure from method is returned"
                    Function = 
                        (fun _ ->
                            let call x =
                                "This is a failure"
                                |> General
                                |> Failure

                            let result = railroad call 2
                            Success |> result |> expectsToBe ("This is a failure" |> generalFailure)
                        )
                }
            ]
