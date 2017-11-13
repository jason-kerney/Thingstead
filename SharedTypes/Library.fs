namespace SolStone.SharedTypes
open System

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

type Test =
    {
        TestContainerPath : string list
        TestName : string
        TestFunction : unit -> TestResult
    }

type TestExecutionReport =
    {
        Failures: (Test * FailureType) list
        Successes: Test list
    }