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
    | MultiFailure of FailureType * FailureType
    | FailureWithComment of FailureType * string
    | Intermittent
    | ExceptionFailure of System.Exception
    | ExpectationFailure of string
    | GeneralFailure of string
    | Ignored of string

type EngineResult<'SuccessType, 'FailureType> =
    | Success of 'SuccessType
    | Failure of 'FailureType

type TestResult = EngineResult<unit, FailureType>

type TestingEnvironment =  Map<string, obj>

type Test = 
    {
        Name: string
        Path: string option
        Before: TestingEnvironment -> EngineResult<TestingEnvironment, PrePostFailureType>
        TestMethod: TestingEnvironment -> TestResult
        After: TestingEnvironment -> EngineResult<unit, PrePostFailureType>
    }
    
type ExecutionResults = 
    {
        Successful: Test list
        Failed: (Test * FailureType) list
    }

type StepInput = 
    | Initial of Test list
    | PreviousFailed of (Test * TestResult) list
    | PreviousSucceeded of (Test * TestResult) list

type Step = 
    {
        Name : string
        BeforeStep: TestingEnvironment -> EngineResult<TestingEnvironment, PrePostFailureType>
        StepProcess: TestingEnvironment -> StepInput -> EngineResult<(Test * TestResult) list, (Test * TestResult) list>
        AfterStep: TestingEnvironment -> EngineResult<unit, PrePostFailureType>
    }

(* Sketch of intent

type StageInput =
    | Tests of Test list
    | Results of (Test list) * ExecutionResults

type Stage = 
    {
        Filter: StageInput -> Test list
        BeforeStage: TestingEnvironment -> Test list -> EngineResult<TestingEnvironment, PrePostFailureType>
        Steps: Step list
        AfterStage: TestingEnvironment -> Test list -> EngineResult<unit, PrePostFailureType>
    }

type Pipeline =
    {
        Name: string option
        Tests: Test list
        BeforePipeline: TestingEnvironment -> Test list -> EngineResult<TestingEnvironment, PrePostFailureType>
        Stages: Stage list
        AfterPipeline: TestingEnvironment -> Test list -> EngineResult<unit, PrePostFailureType>
    }

// *)