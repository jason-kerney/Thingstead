namespace ThingStead.Framework
open System

type FailureTypes =
    | WithComment of string * FailureTypes
    | General of string
    | Exception of exn
    | Expectation of string

type Results =
    | Success
    | Failure of FailureTypes
    | SetupFailure of FailureTypes
    | TeardownFailure of FailureTypes
    | CustomFailure of FailureTypes

type Environment = unit

type Test =
    {
        TestName : string
        Function: Environment -> Results
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