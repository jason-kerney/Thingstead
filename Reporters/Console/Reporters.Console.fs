namespace Reporters.Console
open SolStone.SharedTypes
open System

module Reporter = 
    let getLines (input : string) = 
        input.Split ([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries);

    let joinWith (seperator: string) (values : string seq) = 
        String.Join (seperator, values)

    let joinAsLines : string seq -> string = joinWith "\n"

    let modifyLinesWith modification input =
        input
            |> getLines
            |> Array.map modification
            |> joinAsLines

    let prependWith before input = 
        sprintf "%s%s" before input

    let trim (input : string) =
        input.Trim ()    

    let prependLinesWith before input =
        input |> modifyLinesWith (prependWith before)

    let indentBy amount value = 
        let amount = if amount >= 0 then amount else 0
        let tab = if amount > 0 then "\t" else ""        
        let indentation = String.replicate amount tab
        value |> prependWith indentation

    let indentLinesBy amount (value : string) =
        value |> modifyLinesWith (indentBy amount)

    let report (testResults : TestExecutionReport) =
        let failedTests = testResults.Failures
        let failedTestCount = testResults |>  getFailCount
        let testCount = testResults |> getTestCount


        let printDetailedFailure i (test: Test, failure: FailureType) = 
            printfn "%3d: %s" i test.TestName
            let summary = 
                failure 
                |> sprintf "%A"
                |> modifyLinesWith trim
                |> prependLinesWith "#  " 
                |> indentLinesBy 2

            printfn "%s\n" summary

        let printFailure i (test: Test, failure: FailureType) =
            printf "%3d: %s" i test.TestName
            let failureMessage = 
                failure
                |> sprintf "%A"
                |> getLines
                |> Array.map trim
                |> joinWith " "

            printfn " -- %s" failureMessage

        failedTests |> List.iteri printDetailedFailure
        printfn "\n\n"
        failedTests |> List.iteri printFailure
        printfn "\n\n"
        printfn "%d of %d Failed" failedTestCount testCount
        if (failedTestCount = 0) then
            printfn "Good Job!"