namespace SolStone.Tests
open SolStone.TestRunner.Default.Framework
open SolStone.Reporters.Console.Reporter
open SolStone.Core.SharedTypes.Support

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
        |> report "SolStone Tests"
        |> getFailCount



