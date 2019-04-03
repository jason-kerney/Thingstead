namespace ThingStead.Framework

type FailureTypes =
    | General of string
    | Exception of System.Exception
    | Expectation of string
    | WithComment of string * FailureTypes

type Results =
    | Success
    | Failure of FailureTypes