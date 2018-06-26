namespace Thingstead.Engine.Tests

open Thingstead.Types

module TestingTools = 
    let teePrint message value = 
        printfn "%s" message
        value

    let teePrintValue message value = 
        teePrint (sprintf "%s %A" message value) value    

    type EquatableTest = 
        {
            EquatableName: string
            EquatablePath: string option
            EquatableBefore: EquatableObject<(Environment -> Result<Environment, PrePostFailureType>) option>
            EquatableExecutable: EquatableObject<Environment -> TestResult>
            EquatableAfter: EquatableObject<(Environment -> Result<unit, PrePostFailureType>) option>
        }

    type  EquatableExecutionResults = 
        {
            EquatablSuccessful: EquatableTest list
            EquatablFailed: (EquatableTest * FailureType) list
        }  

    let toEquatableTest (test: Test) =
        {
            EquatableName = test.Name
            EquatablePath = test.Path
            EquatableBefore = EquatableObject (test.Before)
            EquatableExecutable = EquatableObject (test.Executable)
            EquatableAfter = EquatableObject (test.After)
        }

    let toEquatableExecutionResult (result: ExecutionResults) = 
        {
            EquatablSuccessful = result.Successful |> List.map toEquatableTest
            EquatablFailed = result.Failed |> List.map (fun (test, testResult) -> (test |> toEquatableTest, testResult))
        }

    let testIsEqualTo expected actual = 
        let a = expected |> toEquatableTest
        let b = actual |> toEquatableTest

        a.EquatableName = b.EquatableName
        && a.EquatablePath = b.EquatablePath
        && a.EquatableBefore.Equals (b.EquatableBefore)
        && a.EquatableExecutable.Equals (b.EquatableExecutable)
        && a.EquatableAfter.Equals (b.EquatableAfter)

    let resultIsEqualTo expected actual = 
        let successfulMatch = 
            (
                expected.Successful.Length = 0
                && actual.Successful.Length = 0
            )
            || (
                expected.Successful.Length = actual.Successful.Length
                && (
                    expected.Successful 
                    |> List.zip (actual.Successful)
                    |> List.map (fun (expected, actual) -> actual |> testIsEqualTo expected)
                    |> List.reduce (&&)
                )
            )

        let failedMatch =
            (
                expected.Failed.Length = 0
                && actual.Failed.Length = 0
            )
            || (
                expected.Failed.Length = actual.Failed.Length
                && (
                    expected.Failed
                    |> List.zip (actual.Failed)
                    |> List.map (fun ((expectTest, expectedResult), (actualTest, actualResult)) ->
                        actualResult = expectedResult
                        && (actualTest |> testIsEqualTo expectTest)
                    )
                    |> List.reduce (&&)
                )
            )

        successfulMatch && failedMatch

    let failTest expected actual = 
        sprintf "expected <%A> got <%A>" expected actual
        |> ExpectationFailure
        |> Failure

    let shouldBeEqualTo expected actual = 
        if expected = actual then Success
        else failTest expected actual

    let shouldBeEqualToTestOf (expected: Test) (actual: Test) =
        
        if actual |> testIsEqualTo expected 
        then Success
        else failTest expected actual

    let shoulbBeEqualToResultOf expected actual =
        if actual |> resultIsEqualTo expected
        then Success
        else failTest expected actual

    let shoulbBeEqualToResultsOf expected actual = 
        let result =
            expected
            |> List.zip actual
            |> List.map (fun (e, a) -> a |> resultIsEqualTo e)
            |> List.reduce (&&)

        if result
        then Success
        else failTest expected actual