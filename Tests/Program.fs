namespace Tests

open Tests.Utils
open ThingStead.Framework.Execution
   
module Program = 
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
        let failedCount = results.Failures |> (countPartsBy (fun (_, results) -> results))

        let report = ReportBuilder.reportOn results
        printf "%s" report

        failedCount