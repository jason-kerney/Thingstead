namespace SolStone.TestBuilder.Scripting
open SolStone.Core.SharedTypes

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
    
    let andNext (fn : string -> Test list -> Test list) (groupTitle : string) (tests : Test list) =
        List.append (
            fn groupTitle tests
        )

    let andAlso check a b pastResult =
        if pastResult = Success then check a b
        else pastResult