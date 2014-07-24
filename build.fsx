#I "packages/FAKE/tools"
#r "FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.Git

RestorePackages()

let buildDir = "./build/"
let testDir = "./test/"

let version =
    match buildServer with
    | AppVeyor -> buildVersion
    | _ -> "0.0.0.1"

let commitHash = Information.getCurrentHash()

Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "SetAssemblyInfo" (fun _ ->
    let assemblyPath = "./Fred/Properties/AssemblyInfo.cs"
    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(assemblyPath)) |> ignore
    CreateCSharpAssemblyInfo assemblyPath
        [Attribute.Title "Fred"
         Attribute.Description "Parsing with derivatives"
         Attribute.Guid "6F3B81EE-EA0E-4546-B722-A7CEB6C73208"
         Attribute.Product "Fred"
         Attribute.Metadata("githash", commitHash)
         Attribute.Version version
         Attribute.FileVersion version]
)

Target "BuildSolution" (fun _ ->
    MSBuildWithDefaults "Build" ["Fred.sln"]
    |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    !! "./FredTests/bin/Release/FredTest.dll" // Don't like that "Release" there, but we need to limit ourselves to _one_ copy of the test dll
      |> NUnit (fun p ->
          {p with
             DisableShadowCopy = true;
             OutputFile = testDir + "TestResults.xml" })
)

// Default target
Target "Default" (fun _ ->
    trace "Fred, built through the magic of FAKE"
)

"Clean"
    ==> "SetAssemblyInfo"
    ==> "BuildSolution"
    ==> "Test"
    ==> "Default"

// start build
RunTargetOrDefault "Default"
