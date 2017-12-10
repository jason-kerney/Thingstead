open System.Runtime.InteropServices.ComTypes
open System.IO
// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open System

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDir  = "./build/" |> IO.Path.GetFullPath
let appReferences = !! "/**/*.fsproj"
let dotnetcliVersion = "2.0.2"
let mutable dotnetExePath = "dotnet"

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

let run' timeout cmd args dir =
    if execProcess (fun info ->
        info.FileName <- cmd
        if not (String.IsNullOrWhiteSpace dir) then
            info.WorkingDirectory <- dir
        info.Arguments <- args
    ) timeout |> not then
        failwithf "Error while running '%s' with args: %s" cmd args

let run = run' System.TimeSpan.MaxValue

let runDotnet workingDir args =
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
    if result <> 0 then failwithf "dotnet %s failed" args

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

let clean _ =
    CleanDirs [buildDir]
    
Target "Clean" clean
Target "FClean" clean

let installDotNetCli _ =
    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
Target "InstallDotNetCLI" installDotNetCli

let restore _ =
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "restore"
    )

Target "Restore" restore
Target "FRestore" restore

let build _ =
    let currentPath = ".\\" |> IO.Path.GetFullPath
    runDotnet currentPath ("build -o " + buildDir)

Target "Build" build
Target "FBuild" build

let test _ = 
    buildDir
    |> DirectoryInfo
    |> fun d -> d.GetFiles ("*.Tests.dll")
    |> Seq.iter (fun file ->
        let fileName = file.FullName
        let path = fileName |> IO.Path.GetDirectoryName
        printfn "running: %s" fileName
        runDotnet path fileName
    )

Target "Test" test
Target "FTest" test

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
  ==> "InstallDotNetCLI"
  ==> "Restore"
  ==> "Build"
  ==> "Test"

"FClean"
  ==> "FRestore"
  ==> "FBuild"
  ==> "FTest"
RunTargetOrDefault "FTest"
