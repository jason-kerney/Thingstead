namespace Thingstead.Types

type PrePostFailureType =
    | PrePostExceptionFailure of System.Exception
    | PrePostFailure of obj
    | PrePostSimpleFailure of string

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
    | BeforeFailure of PrePostFailureType
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
        Before: Environment -> Result<Environment, PrePostFailureType>
        TestMethod: Environment -> TestResult
        After: Environment -> Result<unit, PrePostFailureType>
    }
    
type ExecutionResults = 
    {
        Successful: Test list
        Failed: (Test * FailureType) list
    }

type Step = 
    {
        BeforeStep: Environment -> Test -> Result<Environment, PrePostFailureType>
        Executor: Environment -> (Environment -> TestResult) -> TestResult
        AfterStep: Environment -> Test -> Result<unit, PrePostFailureType>
    }
    
type StageInput =
    | Tests of Test list
    | Results of (Test list) * ExecutionResults

type Stage = 
    {
        Filter: StageInput -> Test list
        BeforeStage: Environment -> Test list -> Result<Environment, PrePostFailureType>
        Steps: Step list
        AfterStage: Environment -> Test list -> Result<unit, PrePostFailureType>
    }

type Pipeline =
    {
        Name: string option
        Tests: Test list
        BeforePipeline: Environment -> Test list -> Result<Environment, PrePostFailureType>
        Stages: Stage list
        AfterPipeline: Environment -> Test list -> Result<unit, PrePostFailureType>
    }