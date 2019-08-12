namespace Tests

open ThingStead.Framework.Execution
open Tests.Utils

module ReportBuilder =
    
    let reportOn results =
        let failedCount = results.Failures |> (countPartsBy (fun (_, results) -> results))
        let runCount = results.Results |> (countPartsBy(fun (_, results) -> results))

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

        let report =
            if 0 < report.Length then
                sprintf "\n\n%s\n\n" report
            else
                report

        let report = sprintf "%s%d tests run\n" report runCount
        let report = sprintf "%s%d tests failed\n" report failedCount
        let report = sprintf "%s\tSeeded With: %d\n" report results.Seed

        printf "%s" report
