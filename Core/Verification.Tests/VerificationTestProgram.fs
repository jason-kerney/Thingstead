open SolStone.TestRunner.Default.Framework
open SolStone.Core.SharedTypes
open SolStone.Reporters.Console.Reporter
open SolStone.TestBuilder.Scripting

let expectsToBe = SolStone.Core.Verification.expectsToBe

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
        )
    ) 
    |> executer
    |> report
    |> getFailCount