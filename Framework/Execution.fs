namespace ThingStead.Framework

open FSharp.Collections.ParallelSeq

module Execution = 
    type ExecutedTest =
        {
            TestName: string
            Result: Results
            Test: Test
        }

    type ExecutedTestGroup =
        {
            GroupName: string
            TestResults: ExecutedTest list
        }

    type ExecutionResult = {
        Results: ExecutedTestGroup list
        Failures: ExecutedTestGroup list
        Successes: ExecutedTestGroup list
        Seed: int
        TimeElapsedMilliseconds: int64
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

    let perform { TestName = name; Function = testAction } (groupName : string) =
            let name = sprintf "%s" name
            groupName, {
                    TestName = name
                    Result = testAction ()
                    Test = { TestName = name; Function = testAction }
                }

    let runStatic { TestGroups = tests } seed =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()

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
                perform test groupName
            )
            |> PSeq.toList
        
        let groupItems (tests: (string * ExecutedTest) list) =
            tests
            |> List.groupBy (fun (groupName : string, _) ->
                groupName
            )
            |> List.map (fun (groupName, results) ->
                let results =
                    results
                    |> List.map (fun (_, result) -> result)

                {
                    GroupName = groupName
                    TestResults = results
                }
            )

        let testRunner =
            flatten >> (randomize getNext) >> executeEach

        let results = 
            tests
            |> testRunner

        let failed = 
            results
            |> List.filter 
                (fun (_, { TestName = _; Result = result; Test = _ }) ->
                    result <> Success
                )
            |> groupItems
    
        let succeeded = 
            results
            |> List.filter 
                (fun (_, {TestName = _; Result = result; Test = _}) ->
                    result = Success
                )
            |> groupItems

        stopWatch.Stop()

        {
            Results = results |> groupItems
            Failures = failed
            Successes = succeeded
            Seed = seed
            TimeElapsedMilliseconds = stopWatch.ElapsedMilliseconds
        }

    let run tests =
        runStatic tests (int(System.DateTime.Now.Ticks) &&& 0x0000FFFF)
        
