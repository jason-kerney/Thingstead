namespace SolStone.TestRunner.Default
open SolStone.SharedTypes
open System

module Framework =
    let shuffle<'a> (getRandom: (int * int) -> int) (items: 'a list) =
        let arr = items |> List.toArray

        let rec shuffle pt =
            if pt >= arr.Length
            then arr
            else
                let pt2 = getRandom (pt, arr.Length - 1)
                let hold = arr.[pt]
                arr.[pt] <- arr.[pt2]
                arr.[pt2] <- hold
                shuffle (pt + 1)

        shuffle 0 |> List.ofArray

    let addTest result test =
        match test.TestFunction () with
        | Success
            -> { result with Successes = test :: result.Successes; TotalTests = result.TotalTests + 1 }
        | Failure failure -> { result with Failures = (test, failure) :: result.Failures; TotalTests = result.TotalTests + 1  }

    let executerWithSeed (tests : Test list) seed =
        let rand = Random (seed) 
        
        tests
            |> shuffle (rand.Next)
            |> List.fold addTest { startingReport with Seed = Some seed } 
                   

    let executer (tests : Test list) = 
        let dateTimeAsIntSeed = int(DateTime.Now.Ticks &&& 0x0000FFFFL)
        dateTimeAsIntSeed |> executerWithSeed tests