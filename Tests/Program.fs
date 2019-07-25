namespace TempRunner

open TempRunner.Utils
open ThingStead.Framework
open ThingStead.DomainLanguage.Expectations
open ThingStead.Framework.Execution

type TestTemplate = {
    Name : string
    Function : Environment -> Results
}

module Program = 
    open Utils

    let randomize (getIndex: int -> int) (items: 'a List) = 
        let rec walk index (arr:'a array) =
            if (arr.Length) <= index then
                arr |> Array.toList
            else
                let swap = getIndex (arr.Length)
                let h = arr.[index]
                let w = arr.[swap]
                Array.set arr index w
                Array.set arr swap h
                walk (index + 1) arr

        walk 0 (items |> List.toArray)

    let asTests templates (suiteName : string) =
        let suiteName = 
            if suiteName.Trim().Length > 0 then suiteName.Trim()
            else ""

        let tests = 
            templates
            |> List.map (
                fun { Name = name; Function = tst } ->
                    {
                        TestName = name
                        Function = tst
                    }
            )

        {
            GroupName = suiteName
            Tags = []
            Tests = tests
        }

    let tests = 
        [
            "Railroad should:" |> asTests
                [
                    {
                        Name = "Success Calls function" 
                        Function =
                            fun _ -> 
                            (
                                let mutable a = 0
                                let call x = 
                                    a <- x
                                    Success

                                Success |> railroad call 2 |> ignore

                                a |> expectsToBe 2 |> withComment "Function was not called"
                            )
                    }
                    {
                        Name = "Failure prevents function call"
                        Function = 
                            fun _ ->
                            (
                                let mutable a = 0
                                let call x =
                                    a <- x
                                    Success

                                "This is a failure"
                                |> General |> Failure |> railroad call 2 |> ignore

                                a |> expectsToBe 0 |> withComment "Function was called"
                            )
                    }
                    {
                        Name = "Exception is not thrown out"
                        Function =
                            fun _ -> 
                            (
                                let call x =
                                    failwith "Bang"

                                try
                                    Success |> railroad call 2 |> ignore
                                    Success
                                with
                                | ex -> ex |> Exception |> Failure 
                            )
                    }
                ]
            "expectsToBe Should:" |> asTests
                [
                    {
                        Name = "succeed when comparing 1 to 1"
                        Function = fun _ -> 1 |> expectsToBe 1
                    }
                    {
                        Name = "Return an expectation failure when comparing 1 to 2"
                        Function = 
                            (fun _ -> 
                                match (1 |> expectsToBe 2) with
                                | Failure (Expectation "1 expected to be 2") ->
                                    Success
                                | a ->
                                    (sprintf "%A expected to be %A" a ("1 expected to be 2" |> Expectation |> Failure))
                                    |> Expectation |> Failure
                            )
                    }
                    {
                        Name = "Allow a comment to be added to a failure"
                        Function = 
                            (fun _ ->
                                let result = 2 |> expectsToBe 1 |> withComment "This is a failure"

                                let expected =
                                    ("This is a failure", "2 expected to be 1" |> Expectation) 
                                    |> WithComment
                                    |> Failure

                                result |> expectsToBe expected
                            )
                    }
                    {
                        Name = "Comment is not added if expectation is met"
                        Function = 
                            (fun _ ->
                                let result = "Hello" |> expectsToBe "Hello" |> withComment "This is a failure"

                                result |> expectsToBe Success
                            )
                    }
                ]
            "Setup railroad should:" |> asTests
                [
                    {
                        Name = "Success Calls function" 
                        Function =
                            fun _ -> 
                            (
                                let mutable a = 0
                                let call x = 
                                    a <- x
                                    Success

                                Success |> setupRailroad call 2 |> ignore

                                a |> expectsToBe 2 |> withComment "Function was not called"
                            )
                    }
                    {
                        Name = "Failure prevents function call"
                        Function = 
                            fun _ ->
                            (
                                let mutable a = 0
                                let call x =
                                    a <- x
                                    Success

                                "This is a failure"
                                |> General |> Failure |> setupRailroad call 2 |> ignore

                                a |> expectsToBe 0 |> withComment "Function was called"
                            )
                    }
                    {
                        Name = "Exception is not thrown out"
                        Function =
                            fun _ -> 
                            (
                                let call x =
                                    failwith "Bang"

                                try
                                    Success |> setupRailroad call 2 |> ignore
                                    Success
                                with
                                | ex -> (ex |> Exception |> SetupFailure) 
                            )
                    }
                ]
            "Teardown railroad should:" |> asTests
                [
                    {
                        Name = "Success Calls function" 
                        Function =
                            fun _ -> 
                            (
                                let mutable a = 0
                                let call x = 
                                    a <- x
                                    Success

                                Success |> tearDownRailroad call 2 |> ignore

                                a |> expectsToBe 2 |> withComment "Function was not called"
                            )
                    }
                    {
                        Name = "Failure prevents function call"
                        Function = 
                            fun _ ->
                            (
                                let mutable a = 0
                                let call x =
                                    a <- x
                                    Success

                                "This is a failure"
                                |> General |> Failure |> tearDownRailroad call 2 |> ignore

                                a |> expectsToBe 0 |> withComment "Function was called"
                            )
                    }
                    {
                        Name = "Exception is not thrown out"
                        Function =
                            fun _ -> 
                            (
                                let call x =
                                    failwith "Bang"

                                try
                                    Success |> tearDownRailroad call 2 |> ignore
                                    Success
                                with
                                | ex -> ex |> Exception |> TeardownFailure 
                            )
                    }
                ]
            "Randomize should randomize:" |> asTests
                [
                    {
                        Name = "Using a randomizer"
                        Function = 
                            fun _ ->
                            (
                                let mutable randOut = 
                                    [19;18;17;16;15;14;13;12;11;10;10;11;12;13;14;15;16;17;18;19]

                                let pop x =
                                    match randOut with
                                    | [] -> failwith "index out of bounds"
                                    | next::tail ->
                                        randOut <- tail
                                        next

                                let items = 
                                    [0..19] 
                                    |> Seq.toList

                                let actual = items |> randomize pop
                                actual |> expectsToBe (items |> List.rev)
                            )
                    }
                    {
                        Name = "Actually using randomizer"
                        Function = 
                            fun _ ->
                            (
                                let expected = 
                                    [1;4;0;0;4;9;2;1;1;7]
                                let mutable randOut = expected

                                let pop _ =
                                    match randOut with
                                    | [] -> failwith "index out of bounds"
                                    | next::tail ->
                                        randOut <- tail
                                        next

                                let items = 
                                    [0..9] 
                                    |> Seq.toList

                                let actual = items |> randomize pop
                                actual |> expectsToBe [3; 8; 6; 2; 0; 9; 1; 5; 7; 4]
                            )
                    }
                ]
        ]

    [<EntryPoint>]
    let main argv =
        let run (tests: TestGroup list) =
            let rand = System.Random()
            let getNext max =
                rand.Next max

            let flatten tests = 
                tests
                |> List.collect (fun { GroupName = groupName; Tags = _; Tests = ts } ->
                    ts |> List.map (fun test -> groupName, test)
                )

            let executeEach tests =
                tests
                |> List.map (fun (groupName : string, test) ->
                    groupName, (perform test)
                )
            
            let reinstateGroup tests =
                tests
                |> List.groupBy (fun (groupName : string, _) ->
                    groupName
                )
                |> List.map (fun (groupName, results) ->
                    let outPut =
                        results 
                        |> List.map (fun (_, (name : string, result)) -> (name, result))
                        |> List.filter (fun (_, result) -> result <> Success)
                    groupName, outPut
                )

            tests
            |> flatten
            |> randomize getNext
            |> executeEach
            |> reinstateGroup

        let results = run tests

        let failed = 
            results
            |> List.filter (fun (_, results) -> results <> [])

        let report = 
            failed        
            |> List.map (
                fun (group_name, results) ->
                    let reportedResults = 
                        results
                        |> List.mapi (fun index (name, result) ->
                            sprintf "\t%d: %s %A\n" (index + 1) name result
                        )
                        |> join
                    
                    sprintf "%s:\n%s" group_name reportedResults
            )
            |> join

        if 0 < report.Length then
            printfn "\n\n%s\n" report

        let countPartsBy getParts items =
            let numberGetter = getParts >> List.length
            items |> List.sumBy numberGetter

        let failedCount = failed |> (countPartsBy (fun (_, results) -> results))
        let runCount = 
            tests |> (countPartsBy(fun { GroupName = _; Tags = _; Tests = tests } -> tests))

        printfn "%d tests run" runCount
        printfn "%d tests failed" failedCount

        failedCount