namespace SolStone.Core.SharedTypes
open System
open System.Xml.XPath
open System.Reflection

//type IndeterminateInfo =
//    {
//        OriginalFailure : FailureType
//        NewFailure: FailureType
//    }
//and 
type FailureType =
    | GeneralFailure of string
    | ExpectationFailure of string
    | ExceptionFailure of Exception
    | Ignored of String
    | StandardNotMet of String
    | SetupFailure of FailureType
    | TeardownFailure of FailureType
//   | IndeterminateFailure of IndeterminateInfo

type TestResult =
    | Success
    | Failure of FailureType

//type TestContext =
//    {
//        ContainerPath              : string list;
//        TestName                   : string;
//        CanonicalizedName          : string;
//        GoldStandardPath           : string;
//        Reporters                  : (unit -> IApprovalFailureReporter) List;
//    }

type PathInformation = 
    {
        PathName : string
        PathType : string option
    }

type Test =
    {
        TestContainerPath : PathInformation list
        TestName : string
        TestFunction : unit -> TestResult
    }

type TestExecutionReport =
    {
        Seed: int option
        TotalTests: int
        Failures: (Test * FailureType) list
        Successes: Test list
    }

type RandomSeed = int
type TestExecutor = Test list -> TestExecutionReport
type TestExecutorWithSeed = Test list -> RandomSeed -> TestExecutionReport

type Header = string
type TestReporter = Header -> TestExecutionReport -> TestExecutionReport

[<AutoOpen>]
module Support =
    let emptyTest = { TestContainerPath = []; TestName = "Empty Test"; TestFunction = fun _ -> "Not Implemented" |> Ignored |> Failure }
    let blankTest = emptyTest
    let startingReport = { Seed = None ; TotalTests = 0 ; Failures = []; Successes = [] }

    let private joinWith (seperator: string) (values : string seq) = 
        String.Join (seperator, values)

    let getOptionalString = function
        | None -> ""
        | Some value -> value

    let getPathName { PathName = name; PathType = typeName } =
        if typeName = None then
            name
        else
            let typeName = typeName |> getOptionalString
            sprintf "%s %A" typeName name
                  

    let getTestName {TestContainerPath = containerPath; TestName = testName} =
        let path = 
            containerPath
            |> List.map getPathName
            |> joinWith " "

        sprintf "%s %s" path testName

    let getFailCount (report : TestExecutionReport) = 
        report.Failures |> List.length

    let getSuccessCount (report : TestExecutionReport) =
        report.Successes |> List.length

    let getTestCount (report : TestExecutionReport) =
        (report |> getFailCount) + (report |> getSuccessCount)

    let getPath (test : Test) = test.TestContainerPath

    let getFailures (results : TestExecutionReport) =
        results.Failures

    let addFailure test result (report : TestExecutionReport) =
        match result with
        | Success -> report
        | Failure failure ->
            { report with Failures = (test, failure) :: report.Failures; TotalTests = report.TotalTests + 1  }

    let addSuccess test result (report : TestExecutionReport) =
        match result with
        | Failure _ -> report
        | Success ->
            { report with Successes = test :: report.Successes; TotalTests = report.TotalTests + 1 }