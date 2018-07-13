namespace Thingstead.Engine.Tests.EmptyStep

open Thingstead.Engine.Tests
open Thingstead.Engine.Steps
open Thingstead.Types
open Thingstead.Engine.Tests
open Thingstead.Engine.Tests.TestingTools
open Thingstead.Engine

module NeedsToRun = 
        let private path = Some "Thingstead Test Engine 'EmptyStep' should"

        let private template = 
            { testTemplate with
                Path = path
            }

        let private testedWith = applyToTemplate template

        let tests : Test list = 
            [
                "Has the name 'A Blank Step'"
                |> testedWith 
                    (fun _ ->
                        blankStep.Name
                        |> shouldBeEqualTo "A Blank Step"
                    )

                "Return a success for the process"
                |> testedWith 
                    (fun _ ->
                        let tests = [
                            {testTemplate with
                                Name = "A failing test"
                                TestMethod = fun _ -> "Bad Method" |> asTestFailure
                            }
                        ]

                        let getResults (result: EngineResult<(Test * TestResult) list, (Test * TestResult) list>) = 
                            match result with
                            | Success results ->
                                results
                                |> List.map (fun (test, result) -> test.Name, result)
                                |> Success
                            | Failure results ->
                                results
                                |> List.map (fun (test, result) -> test.Name, result)
                                |> Failure

                        tests
                        |> Initial
                        |> blankStep.StepProcess emptyEnvironment
                        |> getResults
                        |> shouldBeEqualTo ([("A failing test", Success ())] |> Success)
                    )

                "Not run any of the tests"
                |> testedWith
                    (fun _ ->
                        let mutable hasRun = false;
                        let tests = [
                            {testTemplate with
                                TestMethod = fun _ -> hasRun <- true; successFulTest
                            }
                            {testTemplate with
                                TestMethod = fun _ -> hasRun <- true; successFulTest
                            }
                            {testTemplate with
                                TestMethod = fun _ -> hasRun <- true; successFulTest
                            }
                        ]

                        tests 
                        |> Initial
                        |> blankStep.StepProcess emptyEnvironment
                        |> ignore

                        hasRun
                        |> shouldBeEqualTo false
                        |> withFailComment "Should not have run any of the tests"
                    )

                "Return the environment passed to the 'BeforeStep' function"
                |> testedWith
                    (fun _ ->
                        let environment = emptyEnvironment.Add ("thing", ["this"; "is"; "my"; "thing"])

                        environment 
                        |> blankStep.BeforeStep
                        |> shouldBeEqualTo (environment |> Success)
                    )

                "Returns success from the 'AfterStep' function"
                |> testedWith
                    (fun _ ->
                        let environment = emptyEnvironment.Add ("thing", ["this"; "is"; "my"; "thing"])

                        environment 
                        |> blankStep.AfterStep
                        |> shouldBeEqualTo (Success ())
                    )
            ] 