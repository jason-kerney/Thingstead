namespace Tests

open Tests.Utils
open ThingStead.Framework
open ThingStead.Framework.Execution

module Program = 
    open Utils

    let tests = 
        [
            Expectations.ExpectToBe.tests
            Framework.Railroad.tests
            Framework.SetupRailroad.tests
            Framework.TeardownRailroad.tests
            Framework.Execution.Randomizer.tests
        ]

    [<EntryPoint>]
    let main _argv =
        let run (tests: TestGroup list) =
            let rand = System.Random()
            let getNext max =
                rand.Next max

            let flatten tests = 
                tests
                |> List.collect (fun { GroupName = groupName; Tags = _; Tests = ts } ->
                    ts |> List.map (fun test -> groupName, test)
                )

            let executeEach tests =
                tests
                |> List.map (fun (groupName : string, test) ->
                    groupName, (perform test), test
                )
            
            let groupItems tests =
                tests
                |> List.groupBy (fun (groupName : string, _, _) ->
                    groupName
                )
                |> List.map (fun (groupName, results) ->
                    let outPut =
                        results 
                        |> List.map (fun (_, (name : string, result), test) -> (name, result, test))
                        |> List.filter (fun (_, result, _) -> result <> Success)
                    groupName, outPut
                )

            tests
            |> flatten
            |> randomize getNext
            |> executeEach
            |> groupItems

        let results = run tests

        let failed = 
            results
            |> List.filter (fun (_, results) -> results |> List.isEmpty |> not)

        let report = 
            failed
            |> List.map (
                fun (group_name, results) ->
                    let reportedResults = 
                        results
                        |> List.mapi (fun index (name, result, _test) ->
                            sprintf "\t%d: %s %A\n" (index + 1) name result
                        )
                        |> join
                    
                    sprintf "%s:\n%s" group_name reportedResults
            )
            |> join

        if 0 < report.Length then
            printfn "\n\n%s\n" report

        let countPartsBy getParts items =
            let numberGetter = getParts >> List.length
            items |> List.sumBy numberGetter

        let failedCount = failed |> (countPartsBy (fun (_, results) -> results))
        let runCount = 
            tests |> (countPartsBy(fun { GroupName = _; Tags = _; Tests = tests } -> tests))

        printfn "%d tests run" runCount
        printfn "%d tests failed" failedCount

        failedCount