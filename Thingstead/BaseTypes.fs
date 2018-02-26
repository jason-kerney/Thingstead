namespace Thingstead

// Test Case => (transformer) => Executable Test Case => (Executer) => Test Result

type TestResult = 
    | Success
    | Failure of string

type TestCaseExecution = unit -> TestResult

type ExecutableTestBlob = {
    TestName: string
    TestCase: TestCaseExecution
}