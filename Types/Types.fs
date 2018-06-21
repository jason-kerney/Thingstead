namespace Thingstead.Types

type FailureType =
    | Intermittent
    | ExpectationFailure of string
    | ExceptionFailure of System.Exception
    | FailureWithComment of FailureType * string
    | Ignored of string

type TestResult = 
    | Success
    | Failure of FailureType

type PrePostFailureType =
    | PrePostExceptionFailure of System.Exception
    | PrePostFailure of obj
    | PrePostSimpleFailure

type Environment =  Map<string, string list>

type Test = 
    {
        Name: string
        Path: string option
        Before: (Environment -> Result<Environment, PrePostFailureType>) option
        Executable: Environment -> TestResult
        After: (Environment -> Result<Environment, PrePostFailureType>) option
    }
