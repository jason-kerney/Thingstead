namespace SolStone.TestBuilder.Scripting.Tests
open SolStone.SharedTypes
open SolStone.TestBuilder.Scripting
open System


module Support =

    let pause () = 
        printfn "\n\nPress any key to continue"
        Console.ReadKey true |> ignore

    let joinWith (seperator: string) (values : string seq) = 
        String.Join (seperator, values)

    let joinAsLines : string seq -> string = joinWith "\n"

    let indent amount (value : string) =
        let tab = if amount > 0 then "\t" else ""
        let amount = if amount >= 0 then amount else 0
        value.Split ([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s -> sprintf "%s%s" (String.replicate amount tab) s )
            |> joinAsLines
        

    let test name fn =
        printf "%s: " name
        try
            fn ()
        with
        | e -> printfn "Failure: %s" e.Message

    type TestSummary = 
        {
            ContainerPath: string list
            Name: string
            Result: TestResult option
        }    

    let blankSummary = { ContainerPath = []; Name = ""; Result = None }

    let asSummary test = 
        { blankSummary with
            ContainerPath = test.TestContainerPath
            Name = test.TestName
            Result = Some (test.TestFunction ())
        }

    let successfullResult () = Success

    let getFailureNames (report: TestExecutionReport) = 
        report.Failures
        |> List.map 
            (fun (test, failure) ->
                let failureText = failure |> sprintf "%A" |> indent 1
                let testName = test |> getTestName
                sprintf "\n%s:\n%s" testName failureText
            )

    let reportFailures = getFailureNames >> List.iter (printfn "%s")
