namespace ThingStead.Tests
open ThingStead.TestRunner.Default.Framework
open ThingStead.Reporters.Console.Reporter
open ThingStead.Core.SharedTypes.Support

module Program =
    [<EntryPoint>]
    let main _argv =
        [
            TestRunners.DefaultRunner.tests
            TestBuilders.Scripting.tests
            Core.VerificationTests.tests
        ] 
        |> List.concat
        |> executer
        |> report "ThingStead Tests"
        |> getFailCount



