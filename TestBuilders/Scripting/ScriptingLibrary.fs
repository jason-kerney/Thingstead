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

    let describe = suite
    let feature = suite
    let product = suite        
