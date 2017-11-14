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

[<EntryPoint>]
let main _argv =
    test "true is true" (fun () ->
            verify true true
        )

    test "Shows a successful test as being successfull" 
            (fun () ->
                let testCase = {
                        TestContainerPath = []
                        TestName = "A passing test"
                        TestFunction = (fun () -> Success)
                    }

                let result = executer [testCase] |> fun result -> result.Successes |> List.head
            
                let expected : string = testCase.TestName
                let actual : string = result.TestName

                verify actual expected
            )

    test "Shows a failed test as failing" 
            (fun () ->
                let failure = GeneralFailure "Bad Test"
                let testCase = {
                        TestContainerPath = []
                        TestName = "A passing test"
                        TestFunction = (fun () -> Failure (failure))
                    }

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
