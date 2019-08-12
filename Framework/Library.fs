namespace ThingStead.Framework

[<AutoOpen>]
module Decisions = 
    let resultRailroad handleFailure f a prior = 
        match prior with
        | Success ->
            try
                f a
            with
            | ex -> ex |> Exception |> handleFailure
        | _ -> prior
    
    let railroad f a prior = resultRailroad Failure f a prior

    let setupRailroad f a prior = resultRailroad SetupFailure f a prior

    let tearDownRailroad f a prior = resultRailroad TeardownFailure f a prior

module Execution = 
    type ExecutionResult = {
        Results: (string * (string * Results * Test) list) list
        Failures: (string * (string * Results * Test) list) list
        Successes: (string * (string * Results * Test) list) list
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
        }
        
