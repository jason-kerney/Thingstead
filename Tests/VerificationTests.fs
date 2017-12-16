namespace ThingStead.Tests.Core
open ThingStead.Core.SharedTypes
open ThingStead.Core.Verification
open ThingStead.TestBuilder.Scripting

module VerificationTests = 
    let tests =
        product "ThingStead" (
            suite "Verification" (
                feature "expectsToBe" [
                    "should return Success if given \"5 |> expectsToBe 5\""
                        |> testedWith (fun _ ->
                            let result = 5 |> expectsToBe 5
                            if result = Success then Success
                            else 
                                result 
                                |> sprintf "Success <> %A"
                                |> ExpectationFailure
                                |> Failure
                        )

                    "should return Failure if given \"\\\"Hello\" |> expectsToBe \\\"Bob\\\"\""
                        |> testedWith (fun _ ->
                            let result = "Hello" |> expectsToBe "Bob"
                            if result = ("\"Hello\" expected to be \"Bob\"" |> ExpectationFailure |> Failure)
                            then Success
                            elif result = Success
                            then  "\"Hello\" was stated as being \"Bob\"" |> ExpectationFailure |> Failure
                            else
                                result 
                                |> sprintf "%A was not the expected failure" 
                                |> ExpectationFailure 
                                |> Failure
                        )
                ] 
                |> alsoWith feature "expectsNotToBe" [
                    "should return Success if given \"3 |> expectsNotToBe 5\""
                        |> testedWith (fun _ ->
                            let result = 3 |> expectsNotToBe 5
                            if result = Success then Success
                            else result |> sprintf "Success <> %A" |> ExpectationFailure |> Failure
                        )

                    "should return an expectation failure if given \"2 |> expectsNotToBe 2\""
                        |> testedWith (fun _ ->
                            let expected = ("2 expected not to be 2" |> ExpectationFailure |> Failure)
                            let result = 2 |> expectsNotToBe 2

                            if result = expected
                            then Success
                            else result |> sprintf "%A <> %A" expected |> ExpectationFailure |> Failure
                        )
                ]
                |> alsoWith feature "andAlso" [
                    "should return Success when Success is checked with a Success"
                        |> testedWith (fun _ ->
                            let result = Success |> andAlso expectsToBe "H" "H"
                            
                            result |> expectsToBe Success
                        )
                    
                    "should return the failure if given a failure as the last argument"
                        |> testedWith (fun _ ->
                            let failure = "Some Failure" |> ExpectationFailure |> Failure
                            let result = failure |> andAlso expectsToBe 5 5

                            result |> expectsToBe failure
                        )

                    "should return the failure if the check results in a failure and the last item is a Success"
                        |> testedWith (fun _ ->
                            let expectedFailure = expectsToBe "bob" "Tom"
                            let result = Success |> andAlso expectsToBe "bob" "Tom"
                            
                            result |> expectsToBe expectedFailure
                        )
                ]
            )
        )