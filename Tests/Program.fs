namespace ThingStead.Tests
open ThingStead.Core.SharedTypes.Support
open ThingStead.Reporters.Console.Reporter
open ThingStead.TestBuilder.Scripting
open ThingStead.TestRunner.Default.Framework

module Program =
    [<EntryPoint>]
    let main _argv =
        [
            TestRunners.DefaultRunner.tests
            TestBuilders.Scripting.tests
            Core.VerificationTests.tests
        ] 
        |> List.concat
        |> product "Thingstead"
        |> executer
        |> report "ThingStead Tests"
        |> getFailCount



