#I "packages/FAKE/tools"
#r "FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.Git
open System.IO
open System.Xml

RestorePackages()

let buildDir = "./build/"
let testDir = "./test/"

let authors = ["Frank Shearar"]
let projectName = "Fred"
let summary = "Parsing with derivatives"

let version =
    match buildServer with
    | AppVeyor -> buildVersion
    | _ -> "0.1.0.0"

let commitHash = Information.getCurrentHash()

// Nicked from UnionArgParser. Thanks, guys!
let addAssembly (target : string) assembly =
    printfn "addAssembly %A %A" target assembly
    let includeFile force file =
        let file = file
        if File.Exists (Path.Combine("nuget", file)) then [(file, Some target, None)]
        elif force then raise <| new FileNotFoundException(file)
        else []

    seq {
        yield! includeFile true assembly
        yield! includeFile false <| Path.ChangeExtension(assembly, "pdb")
        yield! includeFile false <| Path.ChangeExtension(assembly, "xml")
        yield! includeFile false <| assembly + ".config"
    }

// Turn the packages.config into something we can inject
// into the nuspec template.
let depsFromPackagesConfig (f: FileInfo) =
    let xml = new XmlDocument()
    let nsMgr = new XmlNamespaceManager(xml.NameTable)
    xml.Load(XmlReader.Create(f.FullName))
    xml.SelectNodes("//package", nsMgr)
    |> Seq.cast<XmlNode>
    |> Seq.map (fun (pkgNode: XmlNode) -> pkgNode.Attributes.["id"].Value, pkgNode.Attributes.["version"].Value)

let dependenciesFrom projectName =
    let projectDir = Path.Combine [|currentDirectory; projectName|]
    let pkgs = (new DirectoryInfo(projectDir)).GetFiles("packages.config").[0]
    depsFromPackagesConfig pkgs
    |> List.ofSeq

Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; "nugetpackages"]
)

Target "RestorePackages" RestorePackages

Target "SetAssemblyInfo" (fun _ ->
    let assemblyPath = "./Fred/Properties/AssemblyInfo.cs"
    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(assemblyPath)) |> ignore
    CreateCSharpAssemblyInfo assemblyPath
        [Attribute.Title projectName
         Attribute.Description summary
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

Target "CreatePackage" (fun _ ->
    // This is WAY too much effort. Surely there's an easier way?!
    CreateDir @"nuget\lib\net45"
    CopyFile @"nuget\lib\net45" @"Fred\bin\Release\Fred.dll"

    NuGet (fun p ->
        {p with
            Authors = authors
            Project = projectName
            Description = summary
            OutputPath = "nuget"
            Summary = summary
            WorkingDir = "nuget"
            Version = version
            Publish = false
            Dependencies = getDependencies "Fred/packages.config"
        })
        "nuget/Fred.nuspec")

Target "Test" (fun _ ->
    !! "./FredTest/bin/Release/FredTest.dll" // Don't like that "Release" there, but we need to limit ourselves to _one_ copy of the test dll
               ++ "./CsharpClientTest/bin/Release/CsharpClientTest.dll"
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
