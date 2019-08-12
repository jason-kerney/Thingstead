namespace ThingStead.Framework

open FSharp.Collections.ParallelSeq

module Execution = 
    type ExecutionResult = {
        Results: (string * (string * Results * Test) list) list
        Failures: (string * (string * Results * Test) list) list
        Successes: (string * (string * Results * Test) list) list
        Seed: int
    }

    type EngineParameters = {
        TestGroups: TestGroup list
    }

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
        
    let perform { TestName = name; Function = testAction } =
            let name = sprintf "%s" name
            name, (testAction ()), { TestName = name; Function = testAction }

    let runStatic { TestGroups = tests } seed =
        let rand = System.Random seed
        let getNext max =
            rand.Next max

        let flatten tests = 
            tests
            |> List.collect (fun { GroupName = groupName; Tags = _; Tests = ts } ->
                ts |> List.map (fun test -> groupName, test)
            )

        let executeEach tests =
            tests
            |> PSeq.map (fun (groupName : string, test) ->
                groupName, (perform test)
            )
            |> PSeq.toList
        
        let groupItems tests =
            tests
            |> List.groupBy (fun (groupName : string, _) ->
                groupName
            )
            |> List.map (fun (groupName, results) ->
                let outPut =
                    results 
                    |> List.map (fun (_, (name : string, result, test)) -> (name, result, test))
                groupName, outPut
            )

        let results = 
            tests
            |> flatten
            |> randomize getNext
            |> executeEach

        let failed = 
            results
            |> List.filter 
                (fun (_, (_, result, _)) ->
                    result <> Success
                )
            |> groupItems
    
        let succeeded = 
            results
            |> List.filter 
                (fun (_, (_, result, _)) ->
                    result = Success
                )
            |> groupItems

        {
            Results = results |> groupItems
            Failures = failed
            Successes = succeeded
            Seed = seed
        }

    let run tests =
        runStatic tests (int(System.DateTime.Now.Ticks) &&& 0x0000FFFF)
        
