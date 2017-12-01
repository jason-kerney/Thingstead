namespace TestBuilder.Scripting
open SolStone.SharedTypes

[<AutoOpen>]
module Framework =

    let testedWith testFn name =
        {emptyTest with
            TestName = name
            TestFunction = testFn
        }

    let suite name tests =
        tests
        |> List.map
            (fun test ->
                { test with TestContainerPath = name::(test.TestContainerPath) }
            )

    let asSuite = suite
    let feature = suite
    let describe = suite
    let subFeature = suite
    let product = suite
    let groupedBy = suite
    let featured = suite

    let asExpectationFailure = ExpectationFailure >> Failure
    let asIgnored = Ignored >> Failure
    
    let andThen : Test list -> Test list -> Test list = List.append