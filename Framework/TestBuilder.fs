namespace ThingStead.Framework

[<AutoOpen>]
module TestBuilder =
    type Arrange<'precoditions> = Arrange of Setup<'precoditions>
    type Act<'precoditions, 'results> = Act of Do<'precoditions, 'results>
    type Assert<'results> = Assert of Verify<'results>
    type TearDown<'precoditions, 'results> = TearDown of CleanUp<'precoditions, 'results>

    let arrange f = Arrange f
    let emptyArrange = Arrange (fun _ -> ())
    let act f (setup : Arrange<'precoditions>) = Act f, setup
    let emptyAct (setup : Arrange<'precoditions>) = Act (fun _ _ -> ()), setup
    let verify f ((Act action), (Arrange setup)) = 
        let emptyCleanUp _ _ testingResult _ =
            match testingResult with
            | None -> Success
            | Some r -> r

        {
            Setup = setup
            Do = action
            Verify = f
            CleanUp = emptyCleanUp
        }

    let test { Setup = setup; Do = action; Verify = verify; CleanUp = cleanUp } =
        let runTest context =
            let (precondition, doResult, assertResult, executionResult) = 
                try
                    let precondition = context |> setup

                    try
                        let doResult = context |> action precondition

                        try
                            let verifyResult = context |> verify doResult
                            (Some(precondition), Some(doResult), Some(verifyResult), verifyResult)
                        with
                        | ex ->
                            (Some(precondition), Some(doResult), None, ex |> Exception |> Failure)
                    with
                    | ex -> (Some(precondition), None, None, ex |> Exception |> DoFailure |> Failure)
                with
                | ex ->
                    (None, None, None, ex |> Exception |> SetupFailure |> Failure)

            try
                let finalResults = context |> cleanUp precondition doResult assertResult

                {
                    SetupResults = precondition
                    DoResults = doResult
                    VerifyResults = assertResult
                    ExecutionResult = executionResult
                    CleanUpResults = finalResults |> Some
                }
            with
            | ex ->
                let cleanUpResult = ex |> Exception |> CleanUpFailure
                let eResult =
                    match executionResult with
                    | Success -> cleanUpResult
                    | Failure f ->
                        (cleanUpResult, f) |> Complex
                {
                    SetupResults = precondition
                    DoResults = doResult
                    VerifyResults = assertResult
                    ExecutionResult = eResult |> Failure
                    CleanUpResults = cleanUpResult |> Failure |> Some
                }


        runTest