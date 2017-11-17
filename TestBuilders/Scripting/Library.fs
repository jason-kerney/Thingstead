namespace TestBuilder.Scripting
open SolStone.SharedTypes

module Framework =
    let testedWith testFn name =
        {emptyTest with
            TestName = name
            TestFunction = testFn
        }