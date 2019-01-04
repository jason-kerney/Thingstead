namespace ThingStead.Framework

type TestFailureModes =
    | SetupFailure of TestFailureModes
    | DoFailure of TestFailureModes
    | General of string
    | Expectation of string
    | Exception of System.Exception
    | Complex of TestFailureModes * TestFailureModes
    | CleanUpFailure of TestFailureModes


type TestResult =
    | Success
    | Failure of TestFailureModes

type TestingResults = {
    SetupResults : obj option
    DoResults: obj option
    VerifyResults: TestResult option
    ExecutionResult : TestResult
    CleanUpResults : TestResult option
}

type Context = {
    Seed : int64
    TestName : string
    FullName : string
}

type Setup<'precoditions> = Context -> 'precoditions
type Do<'precoditions, 'result> = 'precoditions -> Context -> 'result
type Verify<'result> = 'result -> Context -> TestResult
type CleanUp<'precoditions, 'result> = 'precoditions option -> 'result option -> TestResult option -> Context -> TestResult

type testExecutable = Context -> TestingResults

type TestSteps<'precoditions, 'result> = {
    Setup : Setup<'precoditions>
    Do : Do<'precoditions, 'result>
    Verify : Verify<'result>
    CleanUp: CleanUp<'precoditions, 'result>
}

type Test = {
    Name: string
    Executable : testExecutable
}
and TestTree =
    | Tests of Test list
    | Suite of TestSuite
and TestSuite = {
    Name : string
    Sub: TestTree
}