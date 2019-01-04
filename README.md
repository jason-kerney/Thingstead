# Notes:

Want something that enables this:
```fsharp
namespace ThingStead.SolStone

open ThingStead.SolStone.Framework
open Bowling
open Bowling.ScoreCardUtils

module ``Bowling Score Card Should`` =
    let createScoreCard = create (fun _ -> ScoreCard ())
    let getScore (Some card) _ = card |> score

    let verifyScoreIs expected = 
        verify (fun (Some score) _ -> score |> isExpectedToBe expected)

    let andThrow (pinSets : int list) = 
        let throws = pinSets |> List.map throw |> List.reduce (>>)
        andConfigure throws

    let tests = [
        "Start with a score of 0" 
        |> test (
            arrange (fun _ -> 
                ScoreCard ()
            )
            |> act (fun (Some scoreCard) _ ->
                scoreCard |> score
            )
            |> verifyScoreIs 0
        );

        "Have a score of 5 after throwing a 3 and a 2"
        |> test (
            arrange (
                createScoreCard
                |> andThrow [2; 3]
            )
            |> act getScore
            |> verifyScoreIs 5
        );

        "Have a score of 0 after throwing just a 3"
        |> test (
            arrange (
                createScoreCard
                |> andThrow [3]
            )
            |> act getScore
            |> verifyScoreIs 0
        )

        "Have a score of 0 after throwing just a 5"
        |> test (
            arrange (
                createScoreCard
                |> andThrow [5]
            )
            |> act getScore
            |> verifyScoreIs 0
        )
    ]
```