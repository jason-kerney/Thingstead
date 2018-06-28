namespace Thingstead.Types

type PrePostFailureType =
    | PrePostExceptionFailure of System.Exception
    | PrePostFailure of obj
    | PrePostSimpleFailure

type EquatableObject<'T> (item:'T) = 
    member __.Item
        with get () =
            item
    override __.GetHashCode () =
        (item :> obj).GetHashCode ()

    override __.Equals (other) =
        match other with
        | :? EquatableObject<'T> as thing ->
                System.Object.Equals(item, thing.Item)
        | _ -> false                    

type FailureType =
    | BeforFailure of PrePostFailureType
    | AfterFailure of PrePostFailureType
    | FailureWithComment of FailureType * string
    | Intermittent
    | ExceptionFailure of System.Exception
    | ExpectationFailure of string
    | GeneralFailure of string
    | Ignored of string

type TestResult = 
    | Success
    | Failure of FailureType

type Environment =  Map<string, string list>

type Test = 
    {
        Name: string
        Path: string option
        Before: (Environment -> Result<Environment, PrePostFailureType>) option
        Executable: Environment -> TestResult
        After: (Environment -> Result<unit, PrePostFailureType>) option
    }
    

type ExecutionResults = 
    {
        Successful: Test list
        Failed: (Test * FailureType) list
    }