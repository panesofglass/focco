#r "./packages/FAKE.1.58.9/tools/FakeLib.dll"

open Fake 
open System.IO

// properties
let currentDate = System.DateTime.UtcNow
let projectName = "Focco"
let version = "0.1." + currentDate.ToString("yMMdd")
let projectSummary = "Focco is a quick-and-dirty, literate-programming-style documentation generator."
let projectDescription = "Focco is a quick-and-dirty, literate-programming-style documentation generator."
let authors = ["Ryan Riley"]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://panesofglass.github.com/focco"
let nugetKey = if System.IO.File.Exists "./key.txt" then ReadFileAsString "./key.txt" else ""

// directories
let buildDir = "./build/"
let packagesDir = "./packages/"
let testDir = "./test/"
let deployDir = "./deploy/"
let docsDir = "./docs/"
let nugetDir = "./nuget/"
let targetPlatformDir = getTargetPlatformDir "4.0.30319"
let nugetToolsDir = nugetDir @@ "tools"
let markdownSharpVersion = GetPackageVersion packagesDir "MarkdownSharp"
let razorEngineVersion = GetPackageVersion packagesDir "RazorEngine"

// params
let target = getBuildParamOrDefault "target" "All"
let frameworkVersion = getBuildParamOrDefault "frameworkVersion" "v4.0"
let frameworkParams = 
    let v = ("[^\\d]" >=> "") frameworkVersion
    let v = v.Substring(0,2)
    ["TargetFrameworkVersion", frameworkVersion; "DefineConstants", "NET" + v]

// tools
let fakePath = "./packages/FAKE.1.58.9/tools"
let nugetPath = "./lib/Nuget/nuget.exe"

// files
let appReferences =
    !+ "./src/**/*.*proj"
        |> Scan

let filesToZip =
    !+ (buildDir + "/**/*.*")
        -- "*.zip"
        |> Scan

// targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "BuildApp" (fun _ ->
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = projectName
            AssemblyDescription = projectSummary
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/AssemblyInfo.fs" })

    MSBuild buildDir "Build" (["Configuration","Release"] @ frameworkParams) appReferences
        |> Log "AppBuild-Output: "
)

Target "CopyLicense" (fun _ ->
    [ "LICENSE.txt" ] |> CopyTo buildDir
)

Target "BuildNuGet" (fun _ ->
    CleanDirs [nugetDir; nugetToolsDir ]

    XCopy (buildDir + "Resources" |> FullName) (Path.Combine(nugetToolsDir, "Resources"))
    [ buildDir + "focco.exe"
      buildDir + "focco.pdb"
      buildDir + "focco.xml"
      buildDir + "LICENSE.txt" ]
        |> CopyTo nugetToolsDir

    NuGet (fun p -> 
        {p with               
            Authors = authors
            Project = projectName
            Description = projectDescription
            Version = version
            OutputPath = nugetDir
            Dependencies = ["MarkdownSharp",RequireExactly markdownSharpVersion; "RazorEngine",RequireExactly razorEngineVersion]
            AccessKey = nugetKey
            ToolPath = nugetPath
            Publish = nugetKey <> "" })
        "Focco.nuspec"

    [nugetDir + sprintf "Focco.%s.nupkg" version]
        |> CopyTo deployDir
)

Target "Deploy" (fun _ ->
    !+ (buildDir + "/**/*.*")
        -- "*.zip"
        |> Scan
        |> Zip buildDir (deployDir + sprintf "%s-%s.zip" projectName version)
)

Target "All" DoNothing

// Build order
"Clean"
  ==> "BuildApp" <=> "CopyLicense"
  ==> "BuildNuGet"
  ==> "Deploy"

"All" <== ["Deploy"]

// Start build
Run target

