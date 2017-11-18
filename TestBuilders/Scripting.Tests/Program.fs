// Learn more about F# at http://fsharp.org

open System
open SolStone.SharedTypes
open TestBuilder.Scripting

let pause () = 
    printfn "\n\nPress any key to continue"
    Console.ReadKey true |> ignore    

let test name fn =
    printf "%s: " name
    try
        fn ()
    with
    | e -> printfn "Failure: %s" e.Message

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

type SuiteSummary =
    | Summaries of string * SuiteSummary list
    | TestSummaries of TestSummary list

let asSuiteSummary suite =
    let rec asSuiteSummary suite =
        match suite with
        | TestSuite (name, suites) ->
            Summaries (name, suites |> List.map asSuiteSummary)
        | Tests tests ->
            tests |> List.map asSummary |> TestSummaries

    suite |> asSuiteSummary        

[<EntryPoint>]
let main _argv =
    (*
        "FizzBuzz returns \"1\" when given 1"
            |> testedWith
                (fun _ -> 
                    1 
                    |> fizzBuzz 
                    |> expectsToBe "1"
                )
    *)

    test "Creates a test once given all the parts" 
        (fun () ->
            let name = "My Test"
            let result =
                name
                |> testedWith (fun () -> Success)
                |> asSummary

            verify result {blankSummary with Name=name; Result = Some Success}
        )

    test "Groups tests and creates paths"
        (fun () ->
            let testSuite =
                grouping "Some Related Tests"
                    [
                        Tests ([ "A passing Test" |> testedWith (fun () -> Success) ])
                    ]

            verify (testSuite |> asSuiteSummary) 
                   (Summaries 
                        (
                            "Some Related Tests",
                            [
                                TestSummaries 
                                    [
                                        {
                                            ContainerPath = [];
                                            Name = "A passing Test";
                                            Result = Some Success;
                                        }
                                    ]
                            ]
                        )
                    )
        )        

    pause ()
    0 // return an integer exit code
