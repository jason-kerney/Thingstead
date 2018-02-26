namespace Thingstead.Tests

    module ``Registry Should`` =
        open Thingstead
        open Thingstead.Registry
        open System

        let ``create a new registry when "createRegistry" is called`` = 
            (fun _ -> 
                let registry1 = createRegistry ();
                let registry2 = createRegistry ();

                if Object.ReferenceEquals (registry1, registry2) then
                    Failure("failed")
                else
                    Success
            )


        let ``behave as defined`` = [
            "create a new registry when \"createRegistry\" is called", ``create a new registry when "createRegistry" is called``
        ]
