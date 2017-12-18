namespace ThingStead.Tests
open ThingStead.Core.SharedTypes

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

    let successfullResult () = Success

    type TestSummary = 
        {
            ContainerPath: PathInformation list
            Name: string
            Result: TestResult option
        }    

    let blankSummary = { ContainerPath = []; Name = ""; Result = None }

    let asSummary test = 
        { blankSummary with
            ContainerPath = test.TestContainerPath
            Name = test.TestName
            Result = Some (test.TestFunction ())
        }

    let asPath name = 
        {
            PathName = name
            PathType = None
        }

    let buildTestName testName fn =
        fn [{blankTest with TestName = testName}]
        |> List.head
        |> getTestName

    let getFirstFailure (testReport : TestExecutionReport) = 
        testReport
        |> getFailures
        |> List.head