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
            TestContainerPath = ["Scripting"; "a tests"]
            TestName = "creates a test once given all the parts"
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

    let ``Scripting appends suite name to a single test`` =
         {emptyTest with
                TestContainerPath = ["Scripting"; "suite"]
                TestName = "appends suite name to a single test"
                TestFunction =
                    (fun () ->
                        let path = 
                            "Suite" 
                            |> suite
                                [{blankTest with
                                    TestContainerPath = ["contains a test"]
                                }]
                            |> List.map (fun test -> test.TestContainerPath)
                            |> List.head

                        let expected = ["Suite"; "contains a test"]
                        path |> expectsToBe expected
                    )
         }

    let ``Scripting appends suite to all tests`` =
        {emptyTest with
            TestContainerPath = ["Scripting"; "suite"]
            TestName = "appents suite to all tests"
            TestFunction = 
                (fun () ->
                    let paths = 
                        "Suite"
                        |> suite 
                            [
                                {blankTest with
                                    TestContainerPath = ["does some thing"]
                                }
                                blankTest
                            ]
                        |> List.map (fun test -> test.TestContainerPath)                        

                    let expected = [["Suite"; "does some thing"]; ["Suite"]]
                    paths |> expectsToBe expected                        
                )
        }

    let ``Scripting suite does not fail when given no tests`` =
        {blankTest with
            TestContainerPath = ["Scripting"; "suite"]
            TestName = "does not fail when given no tests"
            TestFunction = 
                (fun () ->
                    let paths = 
                        "Suite" |> suite [] |> List.map asSummary

                    let expected : TestSummary list = []

                    paths |> expectsToBe expected                    
                )
        }

    let result = 
        [
            ``Creates a test once given all the parts``
            ``Scripting appends suite name to a single test``
            ``Scripting appends suite to all tests``
            ``Scripting suite does not fail when given no tests``
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
