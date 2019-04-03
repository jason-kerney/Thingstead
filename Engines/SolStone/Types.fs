namespace ThingStead.Engines.SolStone

type FailureTypes =
    | General of string
    | Exception of System.Exception

type Results =
    | Success
    | Failure of FailureTypes