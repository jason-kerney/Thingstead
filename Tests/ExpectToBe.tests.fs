namespace Tests.Expectations

open Tests.Utils
open ThingStead.Framework
open ThingStead.DomainLanguage.Expectations


module ExpectToBe = 
    let tests = 
        "expectsToBe Should" |> asTests
            [
                {
                    TestName = "succeed when comparing 1 to 1"
                    Function = fun _ -> 1 |> expectsToBe 1
                }
                {
                    TestName = "Return an expectation failure when comparing 1 to 2"
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
                    TestName = "Allow a comment to be added to a failure"
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
                    TestName = "Comment is not added if expectation is met"
                    Function = 
                        (fun _ ->
                            let result = "Hello" |> expectsToBe "Hello" |> withComment "This is a failure"

                            result |> expectsToBe Success
                        )
                }
                //{
                //    TestName = "This will fail"
                //    Function =
                //        (fun _ ->
                //            true |> expectsToBe false
                //        )
                //}
            ]
