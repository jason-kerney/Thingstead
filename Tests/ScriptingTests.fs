namespace SolStone.Tests.TestBuilders
open SolStone.Core.SharedTypes
open SolStone.TestBuilder.Scripting
open SolStone.Core.Verification
open SolStone.Tests.Support
open SolStone.TestBuilder.Scripting.Framework
open SolStone.TestBuilder.Scripting.Framework

module Scripting =
    let tests = 
        product "SolStone"(
            suite "Scripting" (
                feature "a test" [
                    "creates a test once given all the parts"
                        |> testedWith (fun _ ->
                            let name = "My Test"
                            let result =
                                name
                                |> testedWith successfullResult
                                |> asSummary

                            let expected = { blankSummary with Name=name; Result = Some Success }
                            expectsToBe expected result
                        )
                ]
                |> alsoWith feature "suite" [
                    "appends suite name to a single test"
                        |> testedWith (fun _ ->
                            let path = 
                                suite "Suite" [
                                    {blankTest with
                                        TestContainerPath = ["contains a test" |> asPath]
                                    }
                                ]
                                |> List.map getPath
                                |> List.head
                                |> List.map getPathName

                            let expected = ["Suite \"Suite\""; "contains a test"]
                            path |> expectsToBe expected
                        )
                    "appents suite to all tests"
                        |> testedWith (fun _ ->
                            let paths = 
                                suite "Suite" [
                                    {blankTest with
                                        TestContainerPath = ["does some thing" |> asPath]
                                    }
                                    blankTest
                                ]
                                |> List.map (fun test -> test.TestContainerPath)                      

                            let expected = [[{ PathName = "Suite"; PathType = Some "Suite" }; "does some thing" |> asPath]; [{ PathName = "Suite"; PathType = Some "Suite" }]]
                            paths |> expectsToBe expected                        
                        )
                    "does not fail when given no tests"
                        |> testedWith (fun _ ->
                            let paths = 
                                suite "Suite" [] |> List.map asSummary

                            let expected : TestSummary list = []

                            paths |> expectsToBe expected
                        )
                ]
                |> alsoWith feature "setup - testedBy" [
                    "creates a test with the correct name"
                        |> testedWith (fun _ ->
                            let expectedTestName = "my test name"
                            let test = 
                                setup expectedTestName (fun _ ->
                                    Ok ()
                                )
                                |> testedBy (fun _ -> Success) fin
                            
                            test.TestName |> expectsToBe expectedTestName
                        )
                    "calls the test function if setup succeeds"
                        |> testedWith (fun _ ->
                            let mutable called = false
                            let test =
                                setup "A test" (fun _ ->
                                    Ok ()
                                )
                                |> testedBy (fun _ ->
                                    called <- true
                                    Success
                                ) fin

                            test.TestFunction () |> ignore
                            called |> expectsToBe true
                        )
                    "calls the test with data created in the setup"
                        |> testedWith (fun _ -> 
                            let mutable actual = 0
                            let expected = 5
                            let test =
                                setup "Some test" (fun _ ->
                                    Ok expected
                                )
                                |> testedBy (fun context ->
                                    actual <- context
                                    Success
                                ) fin

                            test.TestFunction () |> ignore
                            actual |> expectsToBe expected
                        )
                    "does not call test if setup fails"
                        |> testedWith (fun _ ->
                            let mutable called = false
                            let test = 
                                setup "a test with a failing setup" (fun _ ->
                                    Error ("Failed" |> GeneralFailure)
                                )
                                |> testedBy (fun _ -> 
                                    called <- true
                                    Success
                                ) fin

                            test.TestFunction () |> ignore
                            called |> expectsToBe false
                        )
                    "returns a failure genreated during setup"
                        |> testedWith (fun _ ->
                            let test = 
                                setup "a test with a failing setup" (fun _ ->
                                    Error ("Failed" |> GeneralFailure)
                                )
                                |> testedBy (fun _ -> Success) fin

                            let result = test.TestFunction ()
                            result |> expectsToBe ("Failed" |> GeneralFailure |> SetupFailure |> Failure)
                        )
                ]
                |> also [
                    "tests can be concatinated with \"also\""
                        |> testedWith (fun _ ->
                        let tests = 
                            [
                                "Test A"
                                |> testedWith successfullResult
                            ] 
                            |> also [
                                "Test B"
                                |> testedWith (fun _ -> "Did not work" |> asExpectationFailure)
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
                |> alsoWith feature "Named Groups" [
                    "method asSuite works the same as suite"
                        |> testedWith (fun _ ->
                            let testName = 
                                asSuite "suite" |> buildTestName "test name"

                            testName |> expectsToBe "Suite \"suite\" test name"
                        )
                    "method feature appends feature"
                        |> testedWith (fun _ ->
                            let testName =
                                feature "feature" |> buildTestName "some test"

                            testName |> expectsToBe "Feature \"feature\" some test"
                        )
                    "method describe appends Described"
                        |> testedWith (fun _ ->
                            let testName =
                                describe "describe" |> buildTestName "a test"
                            testName |> expectsToBe "Described \"describe\" a test"
                        )
                    "method subFeature works the same as feature"
                        |> testedWith (fun _ ->
                            let testName =
                                subFeature "sub-feature" |> buildTestName "test"

                            testName |> expectsToBe "Feature \"sub-feature\" test"
                        )
                    "method product appends Product"
                        |> testedWith (fun _ ->
                            let testName = 
                                product "product" |> buildTestName "test"

                            testName |> expectsToBe "Product \"product\" test"
                        )
                    "method groupedBy appends Group"
                        |> testedWith (fun _ -> 
                            let testName =
                                groupedBy "grouped by" |> buildTestName "test"

                            testName |> expectsToBe "Group \"grouped by\" test"
                        )
                    "method featured works like feature"
                        |> testedWith (fun _ -> 
                            let testName =
                                featured "featured" |> buildTestName "test"

                            testName |> expectsToBe "Feature \"featured\" test"
                        )
                ]
            )
        )
