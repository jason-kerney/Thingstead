namespace SolStone.TestRunner.Default
open SolStone.SharedTypes

module Framework =
    let addTest result test =
        match test.TestFunction () with
        | Success
            -> { result with Successes = test :: result.Successes}
        | Failure failure -> { result with Failures = (test, failure) :: result.Failures }

    let executer (tests : Test list) =  
        tests
            |> List.fold (addTest) startingReport