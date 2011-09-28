// **Nocco** is a quick-and-dirty, literate-programming-style documentation
// generator. It is a F# port of [Docco](http://jashkenas.github.com/docco/) for .NET,
// which was written by [Jeremy Ashkenas](https://github.com/jashkenas) in
// Coffescript and runs on node.js.
//
// Nocco produces HTML that displays your comments alongside your code.
// Comments are passed through
// [Markdown](http://daringfireball.net/projects/markdown/syntax), and code is
// highlighted using [google-code-prettify](http://code.google.com/p/google-code-prettify/)
// syntax highlighting. This page is the result of running Nocco against its
// own source files.
//
// Currently, to build Nocco, you'll have to have Visual Studio 2010.
// The project depends on
// [MarkdownSharp](http://code.google.com/p/markdownsharp/) and
// [RazorEngine](http://razorengine.codeplex.com/).
//
// To use Nocco, run it from the command-line:
//
//     nocco *.cs
//
// ...will generate linked HTML documentation for the named source files, saving
// it into a `docs` folder.
//
// The [source for Nocco](http://github.com/panesofglass/nocco) is available on GitHub, and released under the MIT license.
// The [original source in C#](http://github.com/dontangg/nocco) is also available on GitHub.
//
// If **.NET** doesn't run on your platform, or you'd prefer a more convenient
// package, get [Rocco](http://rtomayko.github.com/rocco/), the Ruby port that's
// available as a gem. If you're writing shell scripts, try
// [Shocco](http://rtomayko.github.com/shocco/), a port for the **POSIX shell**.
// Both are by [Ryan Tomayko](http://github.com/rtomayko). If Python's more
// your speed, take a look at [Nick Fitzgerald](http://github.com/fitzgen)'s
// [Pycco](http://fitzgen.github.com/pycco/).
module Nocco

// Import namespaces to allow us to type shorter type names.
open System
open System.IO
open System.Text
open RazorEngine

// The language type stores each supported language,
// as well as regex matchers to determine whether or not
// a given file matches a supported language.
type Language = {
  Name : string
  Symbol : string
  MultilineStart : string option
  MultilineEnd : string option } with
  member x.CommentMatcher =
    RegularExpressions.Regex(@"^\s*" + x.Symbol + @"\s?")
  member x.CommentFilter =
    RegularExpressions.Regex(@"(^#![/]|^\s*#\{)")

// The section stores the various sections of a file's generated markup.
type Section = {
  CodeHtml : string
  DocsHtml : string }

// The model for the [RazorEngine](http://razorengine.codeplex.com/) template.
type Model = {
  Title : string
  PathToCss : string
  Sections : Section []
  Sources : string []
  GetSourcePath : Func<string,string> }

// A list of the languages that Nocco supports, mapping the file extension to
// the symbol that indicates a comment. To add another language to Nocco's
// repertoire, add it here.
[<CompiledNameAttribute("Languages")>]
let private languages =
  [|(".js", { Name = "javascript"; Symbol = "//"; MultilineStart = Some "/*"; MultilineEnd = Some "*/" })
    (".fs", { Name = "fsharp"; Symbol = "//"; MultilineStart = Some "/*"; MultilineEnd = Some "*/" })
    (".cs", { Name = "csharp"; Symbol = "//"; MultilineStart = Some "(*"; MultilineEnd = Some "*)" })
    (".vb", { Name = "vb.net"; Symbol = "'"; MultilineStart = None; MultilineEnd = None })
    (".sql", { Name = "sql"; Symbol = "--"; MultilineStart = None; MultilineEnd = None }) |]
  |> dict

// Get the current language we're documenting, based on the extension.
[<CompiledNameAttribute("GetLanguage")>]
let private getLanguage source =
  let extension = Path.GetExtension source
  if languages.ContainsKey(extension)
    then languages.[extension]
    else Unchecked.defaultof<Language>

// Given a string of source code, parse out each comment and the code that
// follows it, and create an individual `Section` for it.
[<CompiledNameAttribute("Parse")>]
let private parse source lines =
  let language = getLanguage(source)
  let sections, _, _, _ =
    lines |> Seq.fold (fun state line ->
      let sections, hasCode, docsText, codeText = state
      if language.CommentMatcher.IsMatch(line) &&
        not (language.CommentFilter.IsMatch(line)) then
        if hasCode then
          let sections' =
            { DocsHtml = docsText.ToString()
              CodeHtml = codeText.ToString() }
            :: sections
          let docsText' =
            let sb = new StringBuilder()
            in sb.AppendLine(language.CommentMatcher.Replace(line,""))
          (sections', false, docsText', new StringBuilder())
        else
          (sections, hasCode, docsText.AppendLine(language.CommentMatcher.Replace(line, "")), codeText)
      else (sections, hasCode, docsText, codeText.AppendLine(line)))
      ([], false, StringBuilder(), StringBuilder())
  List.rev sections

// Prepares a single chunk of code for HTML output and runs the text of its
// corresponding comment through **Markdown**, using a C# implementation
// called [MarkdownSharp](http://code.google.com/p/markdownsharp/).
[<CompiledNameAttribute("Highlight")>]
let private highlight source sections =
  let markdown = MarkdownSharp.Markdown()
  sections |> Seq.map (fun section ->
    { section with
        DocsHtml = markdown.Transform(section.DocsHtml)
        CodeHtml = System.Web.HttpUtility.HtmlEncode(section.CodeHtml) })

// Compute the destination HTML path for an input source file path. If the source
// is `Example.cs`, the HTML will be at `docs/example.html`
[<CompiledNameAttribute("GetDestination")>]
let private getDestination filepath =
  let directories = Path.GetDirectoryName(filepath).Substring(1).Split([| Path.DirectorySeparatorChar |], StringSplitOptions.RemoveEmptyEntries)
  let depth = directories.Length
  let destination = Path.Combine("docs", String.Join(Path.DirectorySeparatorChar.ToString(), directories)).ToLower()
  Directory.CreateDirectory(destination) |> ignore
  Path.Combine("docs", Path.ChangeExtension(filepath, "html").ToLower()), depth

// Once all of the code is finished highlighting, we can generate the HTML file
// and write out the documentation. Pass the completed sections into the template
// found in `Resources/Nocco.cshtml`
[<CompiledNameAttribute("GenerateHtml")>]
let private generateHtml source files sections =
  let destination, depth = getDestination source
  let pathToRoot = List.fold (fun pathToRoot _ -> Path.Combine("..", pathToRoot)) "" [0..(depth-1)]
  let template = File.ReadAllText(Path.Combine(Directory.GetCurrentDirectory(), "Nocco.cshtml"))
  let model = {
      Title = Path.GetFileName(source)
      PathToCss = Path.Combine(pathToRoot, "nocco.css").Replace('\\', '/')
      Sections = sections |> Array.ofSeq
      Sources = files |> Array.ofSeq
      GetSourcePath = Func<_,_>(fun s ->
        Path.Combine(pathToRoot, Path.ChangeExtension(s.ToLower(), ".html").Substring(2)).Replace('\\', '/')) }
  let result = Razor.Parse(template, model)
  File.WriteAllText(destination, result)

//### Main Documentation Generation Functions

// Generate the documentation for a source file by reading it in, splitting it
// up into comment/code sections, highlighting them for the appropriate language,
// and merging them into an HTML template.
[<CompiledNameAttribute("GenerateDocumentation")>]
let private generateDocumentation files source =
  let files = []
  File.ReadAllLines source
  |> parse source
  |> highlight source
  |> generateHtml source files

// Find all the files that match the pattern(s) passed in as arguments and
// generate documentation for each one.
[<CompiledNameAttribute("Generate")>]
let generate (targets:string[]) =
  if targets.Length <= 0 then
    failwith "At least one target must be specified"
  else
    Directory.CreateDirectory("docs") |> ignore
    let executingDirectory = Directory.GetCurrentDirectory()
    File.Copy(Path.Combine(executingDirectory, "Nocco.css"), Path.Combine("docs", "nocco.css"), true)
    File.Copy(Path.Combine(executingDirectory, "prettify.js"), Path.Combine("docs", "prettify.js"), true)
    let files =
      [ for target in targets do
          yield! Directory.GetFiles(".", target, SearchOption.AllDirectories)
                 |> Seq.filter (fun filename ->
                    not (getLanguage(Path.GetFileName(filename)) = Unchecked.defaultof<Language>)) ]
    for file in files do generateDocumentation files file
