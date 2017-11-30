// Learn more about F# at http://fsharp.org

open System
open SolStone.SharedTypes
open TestBuilder.Scripting
open SolStone.TestRunner.Default.Framework

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

let getTestName test =
    let path = test.TestContainerPath |> joinWith " "
    sprintf "%s %s" path (test.TestName)

let expectsToBe a b =
    if a = b then Success
    else Failure (ExpectationFailure (sprintf "%A <> %A" a b))

let expectsToNotBe a b =
    if a = b then Failure (ExpectationFailure (sprintf "%A = %A" a b))
    else Success

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

[<EntryPoint>]
let main _argv =
    let ``Creates a test once given all the parts`` =
        {emptyTest with
            TestName = "creates a test once given all the parts"
            TestContainerPath = ["Scripting"; "a tests"]
            TestFunction = (fun () ->
                let name = "My Test"
                let result =
                    name
                    |> testedWith (fun () -> Success)
                    |> asSummary

                let expected = { blankSummary with Name=name; Result = Some Success }
                expectsToBe expected result
            )
        }

    let result = 
        [
            ``Creates a test once given all the parts``
        ] |> executer

    let failedCount = result.Failures |> List.length    

    result.Failures
        |> List.iter 
            (fun (test, failure) ->
                let failureText = failure |> sprintf "%A" |> indent 1
                let testName = test |> getTestName
                printfn "\n%s:\n%s" testName failureText
            )

    printfn "\n%d out of %d failed" (failedCount) (result.TotalTests)
    
    if failedCount = 0 && result.TotalTests > 0 then
        printfn "All Good!"

    0 // return an integer exit code
