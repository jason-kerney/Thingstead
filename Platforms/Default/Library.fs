namespace SolStone.TestRunner.Default
open SolStone.SharedTypes

module Framework =
    let executer tests =  
        {
            Failures = []
            Successes = tests
        }