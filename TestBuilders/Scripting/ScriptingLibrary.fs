namespace SolStone.TestBuilder.Scripting
open SolStone.Core.SharedTypes
open SolStone.Core.SharedTypes
open SolStone.Core.SharedTypes.Support
open SolStone.Core.SharedTypes
open SolStone.Core.SharedTypes
open SolStone.Core.SharedTypes.Support
open System.Data
open System.Runtime.InteropServices.ComTypes
open SolStone.Core.SharedTypes

type TestSetup<'a> = 
    {
        SetupName : string
        SetupFunction: unit -> Result<'a, FailureType>
    }

[<AutoOpen>]
module Framework =
    let trim (value : string) = value.Trim ()

    let testedWith testFn name =
        {emptyTest with
            TestName = name
            TestFunction = testFn
        }

    let private alterPath name tests =
        tests
        |> List.map
            (fun test ->
                { test with TestContainerPath = name::(test.TestContainerPath) }
            )

    let buildPath typeName name = 
        {
            PathName = name;
            PathType = typeName
        }
    let private namedAlterPath indicator = 
        let typeName = if (indicator |> trim) |> Seq.isEmpty then None else Some indicator
        buildPath typeName >> alterPath

    let suite = namedAlterPath "Suite"

    let asSuite = suite
    let feature = namedAlterPath "Feature"
    let describe = namedAlterPath "Described"
    let subFeature = feature
    let product = namedAlterPath "Product"
    let groupedBy = namedAlterPath "Group"
    let featured = feature

    let asExpectationFailure = ExpectationFailure >> Failure
    let asIgnored = Ignored >> Failure

    let also : Test list -> Test list -> Test list = List.append
    
    let alsoWith (fn : string -> Test list -> Test list) (groupTitle : string) (tests : Test list) =
        also (
            fn groupTitle tests
        )

    let setup name fn = 
        {
            SetupName = name
            SetupFunction = fn
        }

    let testedBy<'a> (testFunction : 'a -> TestResult) (tearDown : Result<'a, FailureType> * TestResult -> Result<TestResult, FailureType>) (setup : TestSetup<'a>) =
        let runTestWithTearDown testFunction result =
            let results = 
                match result with
                | Ok data -> 
                    let testResult = 
                        try
                            testFunction data
                        with
                        | e ->
                            e |> ExceptionFailure |> Failure

                    (Ok data), testResult
                | Error failureType -> 
                    let setupFailure = failureType |> SetupFailure
                    Error (setupFailure), setupFailure |> Failure

            try
                match tearDown results with
                | Ok testingResult -> testingResult
                | Error errorResult -> 
                    errorResult |> TearDownFailure |> Failure
            with
            | e -> e |> ExceptionFailure |> TearDownFailure |> Failure

                

        let test () = 
            let r = 
                try
                    setup.SetupFunction ()
                with
                | e -> 
                    e |> ExceptionFailure |> SetupFailure |> Error

            runTestWithTearDown testFunction r

        {blankTest with
            TestName = setup.SetupName
            TestFunction = test
        }

    let fin (_setupResult : 'a, testResult : TestResult) : Result<TestResult, FailureType> = Ok testResult

    let notYetImplemented name = 
        {blankTest with
            TestName = name
            TestFunction = (fun _ -> "Not implemented" |> Ignored |> Failure)
        }

    let ``Not Yet Implemented`` = notYetImplemented
