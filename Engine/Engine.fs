namespace Thingstead.Engine

open Thingstead.Types

[<AutoOpen>]
module Executer = 

    let private teePrint message value = 
        printfn "%s" message
        value

    let private teePrintValue message value = 
        teePrint (sprintf "%s %A" message value) value 

    ()