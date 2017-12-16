namespace ThingStead.TestBuilder.Scripting
open ThingStead.Core.SharedTypes

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

    let testedBy<'a> (testFunction : 'a -> TestResult) (teardown : Result<'a, FailureType> * TestResult -> Result<unit, FailureType>) (setup : TestSetup<'a>) =
        let runTestWithTeardown testFunction result =
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
                match teardown results with
                | Ok _ -> results |> snd
                | Error errorResult -> 
                    errorResult |> TeardownFailure |> Failure
            with
            | e -> e |> ExceptionFailure |> TeardownFailure |> Failure

                

        let test () = 
            let r = 
                try
                    setup.SetupFunction ()
                with
                | e -> 
                    e |> ExceptionFailure |> SetupFailure |> Error

            runTestWithTeardown testFunction r

        {blankTest with
            TestName = setup.SetupName
            TestFunction = test
        }

    let fin (_setupResult : 'a, _testResult : TestResult) : Result<unit, FailureType> = Ok ()

    let notYetImplemented name = 
        {blankTest with
            TestName = name
            TestFunction = (fun _ -> "Not implemented" |> Ignored |> Failure)
        }

    let ``Not Yet Implemented`` = notYetImplemented
