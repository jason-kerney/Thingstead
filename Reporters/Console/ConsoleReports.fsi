namespace SolStone.Reporters.Console
open SolStone.Core.SharedTypes
open System

module Reporter = 
    val report : TestReporter
    val changeColorTo : ConsoleColor -> (unit -> 'a) ->'a
    val printInColor : ConsoleColor -> string -> unit
    val printHeader : string -> unit