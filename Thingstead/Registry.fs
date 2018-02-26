namespace Thingstead

module Registry = 
    type RegistryManager = {
        registerExecutableTestsCases: ExecutableTestBlob list -> TestResult list
    }
    let createRegistry _ = 
        let random = System.Random ()
        {
            registerExecutableTestsCases = fun _ -> 
                printfn "%A" (random.Next ())
                [Success]
        }