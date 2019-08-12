namespace Tests.Framework.Execution

open Tests.Utils
open ThingStead.DomainLanguage.Expectations
open ThingStead.Framework
open ThingStead.Framework.Execution

module Randomizer = 
    let tests =
        "Randomize should randomize" |> asTests
            [
                {
                    TestName = "Using a randomizer"
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
                    TestName = "Actually using randomizer"
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