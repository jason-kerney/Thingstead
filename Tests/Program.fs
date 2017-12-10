namespace SolStone.Tests
// Learn more about F# at http://fsharp.org

module Program =
    [<EntryPoint>]
    let main _argv =
        [
            TestRunners.DefaultRunner.run ()
            TestBuilders.Scripting.run ()
        ] |> List.sum
