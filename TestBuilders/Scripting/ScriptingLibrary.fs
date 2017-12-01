namespace TestBuilder.Scripting
open SolStone.SharedTypes

[<AutoOpen>]
module Framework =

    let testedWith testFn name =
        {emptyTest with
            TestName = name
            TestFunction = testFn
        }

    let suite tests name =
        tests
        |> List.map
            (fun test ->
                { test with TestContainerPath = name::(test.TestContainerPath) }
            )

    let asSuite = suite
    let describe = suite
    let feature = suite
    let subFeature = suite
    let product = suite
    
    let asExpectationFailure = ExpectationFailure >> Failure
    let asIgnored = Ignored >> Failure
    
    let andThen : Test list -> Test list -> Test list = List.append