namespace ThingStead.Tests

open ThingStead.Framework
open SolStone.Assertions
open System


module Program = 
    let pause _ =
        System.Console.ReadKey true |> ignore

    let trim (s : string) =
        let a = s
        a.Trim ()

    let join (items : string list) =
        String.Join ('\n', items)

    let indentLines (s : string) =
        s.Split ([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map (fun line -> line |> sprintf "\t\t%s")
        |> join

    [<EntryPoint>]
    let main _argv =

        let tests = [
            "true is expected to be true", (fun () -> 
                true |> isExpectedToBe true
            )
            "False is expected to be true produces Failure (Expectation(false is not true))", (fun () -> 
                false |> isExpectedToBe true |> isExpectedToBe (Failure (Expectation "\"false\" is not \"true\""))
            )
            "False is expected to not be true", (fun () -> 
                false |> isExpectedToNotBe true |> isExpectedToBe Success
            )
            "True is expected to Not be true produces Failure (Expectation(true is true))", (fun () -> 
                true |> isExpectedToNotBe true |> isExpectedToBe (Failure (Expectation "\"true\" not \"true\""))
            )
        ]

        let failed = 
            tests 
            |> List.map (fun (name, test) -> name, test ())
            |> List.filter (fun (_, result) -> result |> (=) Success |> not )
            |> List.map (fun (name, result) -> name, (result |> sprintf "%A" |> indentLines))

        let failedCount = failed.Length

        if 0 < failedCount then
            printfn "---Failures---"
            printfn "--------------"
        else
            printfn "All Good!"

        failed
            |> List.iter (fun (name, result) -> printfn "\t* %s\n%s" name result)

        pause ()

        failedCount