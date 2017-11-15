// Learn more about F# at http://fsharp.org

open System
open SolStone.TestRunner.Default.Framework
open SolStone.SharedTypes

let pause () = 
    printfn "\n\nPress any key to continue"
    Console.ReadKey true |> ignore

let verify a b = 
    if a = b then printfn "Success"
    else printfn "Failure: %A <> %A" a b
    

let test name fn =
    printf "%s: " name
    try
        fn ()
    with
    | e -> printfn "Failure: %s" e.Message

let createTest name fn = 
    {
        TestContainerPath = []
        TestName = name
        TestFunction = fn
    }

let createSuccessfullTest name =
    createTest name (fun () -> Success)

let createFailingTest name failure =
    createTest name (fun () -> Failure failure)

[<EntryPoint>]
let main _argv =
    test "true is true" (fun () ->
            verify true true
        )

    test "Shows a successful test as being successfull" 
            (fun () ->
                let testCase = createSuccessfullTest "A passing test"
                let result = executer [testCase] |> fun result -> result.Successes |> List.head
            
                let expected : string = testCase.TestName
                let actual : string = result.TestName

                verify actual expected
            )

    test "Shows a failed test as failing" 
            (fun () ->
                let failure = GeneralFailure "Bad Test"
                let testCase = createFailingTest "A passing test" failure

                let result = executer [testCase] |> fun result -> result.Failures |> List.head
                let expected = testCase.TestName, failure
                let actual = 
                    match result with
                    | test, testResult ->
                        test.TestName, testResult

                verify actual expected
            )

    pause ()
    0 // return an integer exit code
