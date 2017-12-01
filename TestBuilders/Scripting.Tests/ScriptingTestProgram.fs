namespace SolStone.TestBuilder.Scripting.Tests

open SolStone.TestBuilder.Scripting.Tests.Support
open SolStone.SharedTypes
open TestBuilder.Scripting
open SolStone.TestRunner.Default.Framework

module Program =
    let defineSuite name = 
        fun test -> suite test name

    let scripting = defineSuite "Scripting"

    let testFeature = 
        scripting (
            "a test"
            |> feature [
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
        )

    let suiteFeature = 
        scripting (
            "suite"
            |> feature [
                "appends suite name to a single test"
                |> testedWith (fun () ->
                    let path = 
                        "Suite" 
                        |> suite
                            [{blankTest with
                                TestContainerPath = ["contains a test"]
                            }]
                        |> List.map (fun test -> test.TestContainerPath)
                        |> List.head

                    let expected = ["Suite"; "contains a test"]
                    path |> expectsToBe expected
                )
            ]
        )

    let tsts = 
        testFeature
        |> andThen suiteFeature

    let ``Scripting appends suite to all tests`` =
        {emptyTest with
            TestContainerPath = ["Scripting"; "suite"]
            TestName = "appents suite to all tests"
            TestFunction = 
                (fun () ->
                    let paths = 
                        "Suite"
                        |> suite 
                            [
                                {blankTest with
                                    TestContainerPath = ["does some thing"]
                                }
                                blankTest
                            ]
                        |> List.map (fun test -> test.TestContainerPath)                        

                    let expected = [["Suite"; "does some thing"]; ["Suite"]]
                    paths |> expectsToBe expected                        
                )
        }

    let ``Scripting suite does not fail when given no tests`` =
        {blankTest with
            TestContainerPath = ["Scripting"; "suite"]
            TestName = "does not fail when given no tests"
            TestFunction = 
                (fun () ->
                    let paths = 
                        "Suite" |> suite [] |> List.map asSummary

                    let expected : TestSummary list = []

                    paths |> expectsToBe expected                    
                )
        }

    let ``Scripting tests can be concatinated with "andThen"`` =
        {blankTest with
            TestContainerPath = ["Scripting";]
            TestName = "tests can be concatinated with \"andThen\""
            TestFunction = 
                (fun () ->
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
        }

    [<EntryPoint>]
    let main _argv =
        let tests = 
            [
                ``Scripting appends suite to all tests``
                ``Scripting suite does not fail when given no tests``
                ``Scripting tests can be concatinated with "andThen"``
            ]
            
        let result = tests |> andThen tsts |> executer

        let failedCount = result |> getFailCount

        result |> reportFailures

        printfn "\n%d out of %d failed" (failedCount) (result.TotalTests)
        
        if failedCount = 0 && result.TotalTests > 0 then
            printfn "All Good!"

        0 // return an integer exit code
