namespace SolStone.Reporters.Console
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

    
    let changeColorTo color fn =
        let orig = Console.ForegroundColor
        try
            Console.ForegroundColor <- color
            fn ()
        finally
            Console.ForegroundColor <- orig

    let printInColor color value =
        (fun () -> printf "%s" value)
        |> changeColorTo color

    let printHeader value =
        value |> sprintf "%s\n" |> printInColor ConsoleColor.Yellow

    let report (testResults : TestExecutionReport) =
        let failedTests = testResults.Failures
        let failedTestCount = testResults |>  getFailCount
        let testCount = testResults |> getTestCount

        let printTestTitle i (test: Test) =
            printfn "%3d: %s" (i + 1) (test |> getTestName)

        let printDetailedFailure i (test: Test, failure: FailureType) = 
            printTestTitle i test
            let printDetails () =
                let summary = 
                    failure 
                    |> sprintf "%A"
                    |> modifyLinesWith trim
                    |> prependLinesWith "#  " 
                    |> indentLinesBy 1

                printfn "%s\n" summary

            printDetails |> changeColorTo (ConsoleColor.Red)

        let printFailure i (test: Test, _failure: FailureType) =
            printTestTitle i test

        if failedTestCount > 0 then
            let reportHeaderColor = ConsoleColor.Cyan
            "-- Failed Test Details --\n\n" |> printInColor reportHeaderColor
            failedTests |> List.iteri printDetailedFailure
            
            "\n\n--Failed Test Summary --\n\n" |> printInColor reportHeaderColor
            failedTests |> List.iteri printFailure
            
            printfn "\n"

        let notificationColor = if failedTestCount > 0 then ConsoleColor.Red else ConsoleColor.Green
        sprintf "%d of %d Failed\n\n" failedTestCount testCount
            |> printInColor notificationColor

        testResults 
