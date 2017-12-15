namespace SolStone.TestBuilder.Scripting
open SolStone.Core.SharedTypes
open SolStone.Core.SharedTypes
open SolStone.Core.SharedTypes.Support
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

    let testedBy<'a> (testFunction : 'a -> TestResult) (_tearDown : 'a -> TestResult) (setup : TestSetup<'a>) =
        let setupPassThrough fn result =
            match result with
            | Ok data -> fn data
            | Error failureType -> failureType |> SetupFailure |> Failure

        let test = setupPassThrough testFunction
        
        {blankTest with
            TestName = setup.SetupName
            TestFunction = setup.SetupFunction >> test
        }

    let fin _ = Success
