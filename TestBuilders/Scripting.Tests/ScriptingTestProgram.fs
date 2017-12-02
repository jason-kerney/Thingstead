namespace SolStone.TestBuilder.Scripting.Tests

open SolStone.TestBuilder.Scripting.Tests.Support
open SolStone.SharedTypes
open TestBuilder.Scripting
open SolStone.TestRunner.Default.Framework
open System.ComponentModel.Design.Serialization

module Program =
    let tests = 
        asSuite "Scripting" (
            feature "a test" [
                "creates a test once given all the parts"
                    |> testedWith (fun () ->
                        let name = "My Test"
                        let result =
                            name
                            |> testedWith successfullResult
                            |> asSummary

                        let expected = { blankSummary with Name=name; Result = Some Success }
                        expectsToBe expected result
                    )
            ]
            |> andThen (
                feature "suite" [
                    "appends suite name to a single test"
                        |> testedWith (fun () ->
                            let path = 
                                suite "Suite" [
                                    {blankTest with
                                        TestContainerPath = ["contains a test"]
                                    }
                                ]
                                |> List.map (fun test -> test.TestContainerPath)
                                |> List.head

                            let expected = ["Suite \"Suite\""; "contains a test"]
                            path |> expectsToBe expected
                        )
                    "appents suite to all tests"
                        |> testedWith (fun () ->
                            let paths = 
                                suite "Suite" [
                                    {blankTest with
                                        TestContainerPath = ["does some thing"]
                                    }
                                    blankTest
                                ]
                                |> List.map (fun test -> test.TestContainerPath)                        

                            let expected = [["Suite \"Suite\""; "does some thing"]; ["Suite \"Suite\""]]
                            paths |> expectsToBe expected                        
                        )
                    "does not fail when given no tests"
                        |> testedWith (fun () ->
                            let paths = 
                                suite "Suite" [] |> List.map asSummary

                            let expected : TestSummary list = []

                            paths |> expectsToBe expected
                        )
                ]
            )
            |> andThen [
                "tests can be concatinated with \"andThen\""
                    |> testedWith (fun () ->
                    let tests = 
                        [
                            "Test A"
                            |> testedWith successfullResult
                        ] 
                        |> andThen [
                            "Test B"
                            |> testedWith (fun () -> "Did not work" |> asExpectationFailure)
                        ]
                        |> List.map asSummary
                        

                    let expected = 
                        [
                            {blankSummary with
                                Name = "Test B"
                                Result = Some ("Did not work" |> asExpectationFailure)
                            }
                            {blankSummary with
                                Name = "Test A"
                                Result = Some Success
                            }
                        ]

                    tests |> expectsToBe expected
                )
            ]
            |> andThen (
                let buildTestName testName fn =
                    fn [{blankTest with TestName = testName}]
                    |> List.head
                    |> getTestName

                feature "Named Groups" [
                    "method asSuite works the same as suite"
                        |> testedWith (fun () ->
                            let testName = 
                                asSuite "suite" |> buildTestName "test name"

                            testName |> expectsToBe "Suite \"suite\" test name"
                        )
                    "method feature appends feature"
                        |> testedWith (fun () ->
                            let testName =
                                feature "feature" |> buildTestName "some test"

                            testName |> expectsToBe "Feature \"feature\" some test"
                        )
                    "method describe appends Described"
                        |> testedWith (fun () ->
                            let testName =
                                describe "describe" |> buildTestName "a test"
                            testName |> expectsToBe "Described \"describe\" a test"
                        )
                    "method subFeature works the same as feature"
                        |> testedWith (fun () ->
                            let testName =
                                subFeature "sub-feature" |> buildTestName "test"

                            testName |> expectsToBe "Feature \"sub-feature\" test"
                        )
                    "method product appends Product"
                        |> testedWith (fun () ->
                            let testName = 
                                product "product" |> buildTestName "test"

                            testName |> expectsToBe "Product \"product\" test"
                        )
                    "method groupedBy appends Group"
                        |> testedWith (fun () -> 
                            let testName =
                                groupedBy "grouped by" |> buildTestName "test"

                            testName |> expectsToBe "Group \"grouped by\" test"
                        )
                    "method featured works like feature"
                        |> testedWith (fun () -> 
                            let testName =
                                featured "featured" |> buildTestName "test"

                            testName |> expectsToBe "Feature \"featured\" test"
                        )
                ]
            )
        )
        
    tests 
        |> List.map getTestName
        |> List.sort 
        |> List.iteri (fun i name -> printfn "%3d: %s" (i + 1) name)
    
    printfn "\n\n\n\n\n\n\n"

    [<EntryPoint>]
    let main _argv =
        let result = tests |> executer

        let failedCount = result |> getFailCount

        result |> reportFailures

        printfn "\n%d out of %d failed" (failedCount) (result.TotalTests)
        
        if failedCount = 0 && result.TotalTests > 0 then
            printfn "All Good!"

        0 // return an integer exit code
