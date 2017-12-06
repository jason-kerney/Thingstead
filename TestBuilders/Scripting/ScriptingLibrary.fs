namespace SolStone.TestBuilder.Scripting
open SolStone.SharedTypes

[<AutoOpen>]
module Framework =

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

    let private namedAlterPath indicator = 
        sprintf "%s \"%s\"" indicator >> alterPath

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
    
    let andThen : Test list -> Test list -> Test list = List.append
    let expectsToBe a b =
        if a = b then Success
        else Failure (ExpectationFailure (sprintf "%A <> %A" a b))

    let expectsToNotBe a b =
        if a = b then Failure (ExpectationFailure (sprintf "%A = %A" a b))
        else Success

    let andAlso check a b pastResult =
        if pastResult = Success then check a b
        else pastResult