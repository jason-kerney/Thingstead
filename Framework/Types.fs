namespace ThingStead.Framework

type FailureTypes =
    | WithComment of string * FailureTypes
    | General of string
    | Exception of System.Exception
    | Expectation of string

type Results =
    | Success
    | Failure of FailureTypes
    | SetupFailure of FailureTypes
    | TeardownFailure of FailureTypes
    | CustomFailure of FailureTypes