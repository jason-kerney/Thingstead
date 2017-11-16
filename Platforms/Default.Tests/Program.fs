// Learn more about F# at http://fsharp.org

open System
open SolStone.TestRunner.Default.Framework
open SolStone.SharedTypes

let pause () = 
    printfn "\n\nPress any key to continue"
    Console.ReadKey true |> ignore    

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

let getTestName test =
    test.TestName

let expectsToBe a b =
    if a = b then Success
    else Failure (ExpectationFailure (sprintf "%A <> %A" a b))

let expectsToNotBe a b =
    if a = b then Failure (ExpectationFailure (sprintf "%A = %A" a b))
    else Success

let andAlso check a b pastResult =
    if pastResult = Success then check a b
    else pastResult

let printResult result =
    match result with
    | Success -> printfn "%A" result
    | Failure failure -> printfn "Failed: %A" failure

let verify a b = 
    a |> expectsToBe b |> printResult

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

    test "Multiple tests run in random order"
        (fun () ->
            let testCase1 = createSuccessfullTest "A"
            let testCase2 = createSuccessfullTest "B"
            let testCase3 = createSuccessfullTest "C"

            let resultSeedA, resultA = 45   |> executerWithSeed [testCase1; testCase2; testCase3] |> fun result -> result.Seed, result.Successes |> List.map getTestName
            let resultSeedB, resultB = 1889 |> executerWithSeed [testCase1; testCase2; testCase3] |> fun result -> result.Seed, result.Successes |> List.map getTestName
            let _, resultC = 45   |> executerWithSeed [testCase1; testCase2; testCase3] |> fun result -> result.Seed, result.Successes |> List.map getTestName

            resultA 
                |> expectsToNotBe resultB
            |> andAlso 
                expectsToBe resultA resultC
            |> andAlso
                expectsToBe resultSeedA 45
            |> andAlso
                expectsToBe resultSeedB 1889
            |> printResult
        )        

    // pause ()
    0 // return an integer exit code
