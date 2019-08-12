namespace Tests

open System
open ThingStead.Framework

module Utils =
    let pause _ =
        Console.ReadKey true |> ignore

    let joinWith (seperator: string) (items : 'a seq) =
        String.Join(seperator, items)

    let join (items : 'a seq) = joinWith "\n" items

    let asTests templates (suiteName : string) =
        let suiteName = 
            if suiteName.Trim().Length > 0 then suiteName.Trim()
            else ""
    
        {
            GroupName = suiteName
            Tags = []
            Tests = templates
        }

    let countPartsBy getParts items =
        let numberGetter = getParts >> List.length
        items |> List.sumBy numberGetter