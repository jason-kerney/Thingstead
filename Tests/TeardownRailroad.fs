namespace Tests.Framework

open Tests.Utils
open ThingStead.DomainLanguage.Expectations
open ThingStead.Framework

module TeardownRailroad =
    let tests = 
        "Teardown railroad should:" |> asTests
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

                            Success |> tearDownRailroad call 2 |> ignore

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
                            |> General |> Failure |> tearDownRailroad call 2 |> ignore

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
                                Success |> tearDownRailroad call 2 |> ignore
                                Success
                            with
                            | ex -> ex |> Exception |> TeardownFailure 
                        )
                }
            ]