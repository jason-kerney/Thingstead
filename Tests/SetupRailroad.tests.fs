namespace Tests.Framework

open Tests.Utils
open ThingStead.DomainLanguage.Expectations
open ThingStead.Framework

module SetupRailroad =
    let tests =
        "Setup railroad should" |> asTests
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

                            Success |> setupRailroad call 2 |> ignore

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
                            |> General |> Failure |> setupRailroad call 2 |> ignore

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
                                Success |> setupRailroad call 2 |> ignore
                                Success
                            with
                            | ex -> (ex |> Exception |> SetupFailure) 
                        )
                }
                {
                    TestName = "Success from method is returned"
                    Function = 
                        (fun _ ->
                            let call x =
                                Success

                            let result =
                                Success |> setupRailroad call 3

                            result |> expectsToBe Success
                        )
                }
                {
                    TestName = "Failuer from method is returned"
                    Function = 
                        (fun _ ->
                            let call x =
                                "this is a failure"
                                |> General
                                |> Failure

                            let result =
                                Success |> setupRailroad call 3

                            result |> expectsToBe ("this is a failure" |> generalFailure)
                        )
                }
            ]

