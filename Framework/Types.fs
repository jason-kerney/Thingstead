namespace ThingStead.Framework
open System

type FailureType =
    | WithComment of string * FailureType
    | General of string
    | Exception of exn
    | Expectation of string

type Result =
    | Success
    | Failure of FailureType
    | SetupFailure of FailureType
    | TeardownFailure of FailureType
    | CustomFailure of FailureType

type Environment = unit

type Test =
    {
        TestName : string
        Function: Environment -> Result
    }

type TestGroup =
    {
        GroupName : string
        Tags: string list
        Tests: Test list
    }

type TestSuite = TestGroup

[<AutoOpen>]
module TypeHelpers =
    let generalFailure message =
        message
        |> General
        |> Failure