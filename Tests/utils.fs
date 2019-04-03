namespace TempRunner
open System

module Utils =
    let pause _ =
        Console.ReadKey true |> ignore

    let joinWith (seperator: string) (items : 'a seq) =
        String.Join(seperator, items)

    let join (items : 'a seq) = joinWith "\n" items