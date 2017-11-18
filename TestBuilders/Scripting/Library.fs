namespace TestBuilder.Scripting
open SolStone.SharedTypes

[<AutoOpen>]
module Framework =
    type SuiteTree =
        | TestSuite of (string * SuiteTree)
        | Tests of Test list

    let testedWith testFn name =
        {emptyTest with
            TestName = name
            TestFunction = testFn
        }

    let grouping name suite =
        TestSuite (name, suite)