open SolStone.TestRunner.Default.Framework
open SolStone.Core.SharedTypes
open SolStone.Core.Verification
open SolStone.Reporters.Console.Reporter
open SolStone.TestBuilder.Scripting

[<EntryPoint>]
let main _argv =
    product "SolStone" (
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
            |> andNext feature "expectsNotToBe" [
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
            |> andNext feature "andThen" [
                "should combine 2 test lists"
                    |> testedWith (fun _ ->
                        let testA = {blankTest with TestName = "Hello A"}
                        let testB = {blankTest with TestName = "Hello B"}
                        [testA]
                        |> andThen [testB]
                        |> List.map (fun test -> test.TestName)
                        |> expectsToBe [testB.TestName; testA.TestName]
                    )
            ]
        )
    ) 
    |> executer
    |> report
    |> getFailCount