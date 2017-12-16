namespace ThingStead.Tests.TestRunners
open ThingStead.TestBuilder.Scripting
open ThingStead.TestRunner.Default.Framework
open ThingStead.Core.SharedTypes
open ThingStead.Core.Verification
open System
        
module DefaultRunner =
    open ThingStead.Tests.Support

    let tests = 
        product "ThingStead" (
            suite "Default Test Execution" [
                "Shows a successful test as being successfull"
                    |> testedWith (fun _ ->
                        let testCase = createSuccessfullTest "A passing test"
                        let result = executer [testCase] |> fun result -> result.Successes |> List.head
                    
                        let expected : string = testCase.TestName
                        let actual : string = result.TestName

                        actual |> expectsToBe expected
                    )

                "Shows a failed test as failing"
                    |> testedWith (fun _ ->
                        let failure = GeneralFailure "Bad Test"
                        let testCase = createFailingTest "A passing test" failure

                        let result = executer [testCase] |> getFirstFailure
                        let expected = testCase.TestName, failure
                        let actual = 
                            match result with
                            | test, testResult ->
                                test.TestName, testResult

                        actual |> expectsToBe expected
                    )

                "Multiple tests run in random order"
                    |> testedWith (fun _ ->
                        let testCase1 = createSuccessfullTest "A"
                        let testCase2 = createSuccessfullTest "B"
                        let testCase3 = createSuccessfullTest "C"

                        let resultSeedA, resultA = 45   |> executerWithSeed [testCase1; testCase2; testCase3] |> fun result -> result.Seed, result.Successes |> List.map getSimpleTestName
                        let resultSeedB, resultB = 1889 |> executerWithSeed [testCase1; testCase2; testCase3] |> fun result -> result.Seed, result.Successes |> List.map getSimpleTestName
                        let _, resultC = 45   |> executerWithSeed [testCase1; testCase2; testCase3] |> fun result -> result.Seed, result.Successes |> List.map getSimpleTestName

                        resultA 
                            |> expectsNotToBe resultB
                            |> andAlso 
                                expectsToBe resultA resultC
                            |> andAlso
                                expectsToBe resultSeedA (Some 45)
                            |> andAlso
                                expectsToBe resultSeedB (Some 1889)
                    )
                "A test function that throws an error returns with an exception Failure instead"
                    |> testedWith (fun _ ->
                        let e = ApplicationException "This is an exeption"
                        let expected = e :> Exception |> ExceptionFailure
                        let test = 
                            { blankTest with
                                TestFunction = fun _ -> raise e
                            }
                        
                        let result = [test] |> executer |> getFirstFailure |> snd
                        result |> expectsToBe expected
                    )
            ]
        )