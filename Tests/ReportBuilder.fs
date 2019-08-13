namespace Tests

open ThingStead.Framework.Execution
open Tests.Utils

module ReportBuilder =
    
    let reportOn results =
        let getTestResults { GroupName = _; TestResults = results } = results
        let failedCount = results.Failures |> (countPartsBy getTestResults)
        let runCount = results.Results |> (countPartsBy getTestResults)

        let report = 
            results.Failures
            |> List.map (
                fun { GroupName = group_name; TestResults = results } ->
                    let reportedResults = 
                        results
                        |> List.mapi (fun index { TestName = name; Result = result; Test = _ } ->
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

        let timeElapsed = double(results.TimeElapsedMilliseconds) / 1000.0
        
        printf "%s" report
        printfn "%d tests run" runCount
        printfn "%d tests failed" failedCount
        printfn "\tSeeded With: %d" results.Seed
        printfn "\n\nElapsed Time: %f Seconds" timeElapsed
