namespace TempRunner
open System

module Utils =
    let pause _ =
        Console.ReadKey true |> ignore