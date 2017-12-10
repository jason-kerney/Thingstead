namespace SolStone.Tests.TestRunners
open SolStone.Core.SharedTypes

module Support =
    let createTest name fn = 
        {
            TestContainerPath = []
            TestName = name
            TestFunction = fn
        }

    let createSuccessfullTest name =
        createTest name (fun () -> Success)

    let createFailingTest name failure =
        createTest name (fun () -> Failure failure)

    let getSimpleTestName test =
        test.TestName