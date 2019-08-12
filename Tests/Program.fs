namespace Tests

open Tests.Utils
open ThingStead.Framework
open ThingStead.Framework.Execution

module Program = 
    open Utils

    let tests = 
        {
            TestGroups =
                [
                    Expectations.ExpectToBe.tests
                    Framework.Railroad.tests
                    Framework.SetupRailroad.tests
                    Framework.TeardownRailroad.tests
                    Framework.Execution.Randomizer.tests
                ]
        }

    [<EntryPoint>]
    let main _argv =
        let results = run tests

        let report = 
            results.Failures
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

        let failedCount = results.Failures |> (countPartsBy (fun (_, results) -> results))
        let runCount = 
            tests.TestGroups |> (countPartsBy(fun { GroupName = _; Tags = _; Tests = tests } -> tests))

        printfn "%d tests run" runCount
        printfn "%d tests failed" failedCount
        printfn "\tSeeded With: %d" results.Seed

        failedCount