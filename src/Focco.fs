(*
**Focco** is a quick-and-dirty, literate-programming-style documentation
generator. It is a F# port of [Nocco](http://dontangg.github.com/nocco/)
by [Don Wilson](https://github.com/dontangg/),
which in turn is a port of [Docco](http://jashkenas.github.com/docco/) for .NET,
which was written by [Jeremy Ashkenas](https://github.com/jashkenas) in
Coffescript and runs on node.js.

Focco produces HTML that displays your comments alongside your code.
Comments are passed through
[Markdown](http://daringfireball.net/projects/markdown/syntax), and code is
highlighted using [google-code-prettify](http://code.google.com/p/google-code-prettify/)
syntax highlighting. This page is the result of running Focco against its
own source files.

Currently, to build Focco, you'll have to have .NET 4.0 and [F# 2.0](http://github.com/fsharp/fsharp).
The project depends on
[MarkdownSharp](http://code.google.com/p/markdownsharp/) and
[RazorEngine](http://razorengine.codeplex.com/) (for the System.Web.Razor assembly).

To use Focco, run it from the command-line:

    focco *.fs

...will generate linked HTML documentation for the named source files, saving
it into a `docs` folder.

The [source for Focco](http://github.com/panesofglass/focco) is available on GitHub, and released under the MIT license.
The [original source in C#](http://github.com/dontangg/nocco) is also available on GitHub.

If **.NET** doesn't run on your platform, or you'd prefer a more convenient
package, get [Rocco](http://rtomayko.github.com/rocco/), the Ruby port that's
available as a gem. If you're writing shell scripts, try
[Shocco](http://rtomayko.github.com/shocco/), a port for the **POSIX shell**.
Both are by [Ryan Tomayko](http://github.com/rtomayko). If Python's more
your speed, take a look at [Nick Fitzgerald](http://github.com/fitzgen)'s
[Pycco](http://fitzgen.github.com/pycco/).
*)

// Import namespaces to allow us to type shorter type names.
open System
open System.CodeDom.Compiler
open System.IO
open System.Linq
open System.Text
open System.Text.RegularExpressions
open System.Web.Razor
open Microsoft.FSharp.Control
open FSharp.Control

// The language type stores each supported language,
// as well as regex matchers to determine whether or not
// a given file matches a supported language.
type Language = {
  Name            : string
  Singleline      : string
  MultilineStart  : string option
  MultilineEnd    : string option
  XmlDoc          : string option }
  with
  member x.SinglelineMatcher =
    Regex(@"^\s*" + Regex.Escape x.Singleline + @"\s?")

  member x.MultilineStartMatcher =
    Option.map (fun v -> Regex(@"^\s*" + Regex.Escape v + @"\s?")) x.MultilineStart

  member x.MultilineEndMatcher =
    Option.map (fun v -> Regex(@"(?!"")" + Regex.Escape v + @"(?!"")")) x.MultilineEnd

  member x.CommentFilter =
    let baseRegex = @"^#![/]|^\s*#\{"
    let matchRegex =
      match x.XmlDoc with
      | Some v -> baseRegex + @"|^\s*" + v
      | _ -> baseRegex
    Regex(sprintf "(%s)" matchRegex)

  /// Returns true if the line matches the MultilineStartMatcher and not the CommentFilter; otherwise, false.
  member x.IsStartingMultilineComment(line) =
    match x.MultilineStartMatcher with
    | Some m -> m.IsMatch(line) && not (x.CommentFilter.IsMatch(line))
    | _ -> false
    
  /// Returns true if the line matches the MultilineEndMatcher and not the CommentFilter; otherwise, false.
  member x.IsEndingMultilineComment(line) =
    match x.MultilineEndMatcher with
    | Some m -> m.IsMatch(line) && not (x.CommentFilter.IsMatch(line))
    | _ -> false

  /// Returns true if the line matches the SinglelineMatcher or
  /// both the MultilineStartMatcher and MultilineEndMatcher,
  /// excepting any matches with the CommentFilter; otherwise, false.
  member x.IsSinglelineComment(line) =
    not (x.CommentFilter.IsMatch(line)) &&
    (x.SinglelineMatcher.IsMatch(line) || 
     (x.IsStartingMultilineComment(line) &&
      x.IsEndingMultilineComment(line)))

  /// Returns the line stripped of any comment symbols.
  member x.StripMultilineStartComment(line) =
    match x.MultilineStartMatcher with
    | Some m -> m.Replace(line, "")
    | _ -> line

  /// Returns the line stripped of any comment symbols.
  member x.StripMultilineEndComment(line) =
    match x.MultilineEndMatcher with
    | Some m -> m.Replace(line, "")
    | _ -> line

  /// Returns the line stripped of any comment symbols.
  member x.StripSinglelineComment(line) =
    x.SinglelineMatcher.Replace(line, "")
    |> x.StripMultilineStartComment
    |> x.StripMultilineEndComment

// The section stores the various sections of a file's generated markup.
type Section = {
  CodeHtml : string
  DocsHtml : string }

// Stores the settings for rendering documentation.
type Settings = {
  Scripts : string[]
  Stylesheets : string[]
  TargetDirectory : string
  TemplateName : string }

// The template base class for the rendering via Razor.
[<AbstractClass>]
type TemplateBase() =
  let mutable buffer = new StringBuilder()
  let mutable title : string = null
  let mutable pathToResources : string = null
  let mutable stylesheets : string[] = null
  let mutable scripts : string[] = null
  let mutable getResourcePath : Func<string, string> = null
  let mutable getSourcePath : Func<string,string> = null
  let mutable sections : Section[] = null
  let mutable sources : string[] = null
  member x.Buffer
    with get() = buffer
    and  set(value) = buffer <- value
  member x.Title
    with get() = title
    and  set(value) = title <- value
  member x.Stylesheets
    with get() = stylesheets
    and  set(value) = stylesheets <- value
  member x.Scripts
    with get() = scripts
    and  set(value) = scripts <- value
  member x.GetResourcePath
    with get() = getResourcePath
    and  set(value) = getResourcePath <- value
  member x.GetSourcePath
    with get() = getSourcePath
    and  set(value) = getSourcePath <- value
  member x.Sections
    with get() = sections
    and  set(value) = sections <- value
  member x.Sources
    with get() = sources
    and  set(value) = sources <- value
  abstract Execute : unit -> unit
  abstract WriteLiteral : obj -> unit
  default x.WriteLiteral(value) = x.Buffer.Append(value) |> ignore
  abstract Write : obj -> unit
  default x.Write(value) = x.WriteLiteral(value)

//### Helpers & Setup

// In F#, literate means bottom up. You have to define what you want to use
// before you use it. Therefore, we start with the helpers and setup
// then get into the more meaty generation.

// A list of the languages that Focco supports, mapping the file extension to
// the symbol that indicates a comment. To add another language to Focco's
// repertoire, add it here.
let private languages =
  dict [| (".js",  { Name = "javascript"
                     Singleline = "//"
                     MultilineStart = Some "/*"
                     MultilineEnd = Some "*/"
                     XmlDoc = None })
          (".fs",  { Name = "fsharp"
                     Singleline = "//"
                     MultilineStart = Some "(*"
                     MultilineEnd = Some "*)"
                     XmlDoc = Some "///" })
          (".cs",  { Name = "csharp"
                     Singleline = "//"
                     MultilineStart = Some "/*"
                     MultilineEnd = Some "*/"
                     XmlDoc = Some "///" })
          (".vb",  { Name = "vb.net"
                     Singleline = "'"
                     MultilineStart = None
                     MultilineEnd = None
                     XmlDoc = Some "'''" })
          (".sql", { Name = "sql"
                     Singleline = "--"
                     MultilineStart = None
                     MultilineEnd = None
                     XmlDoc = None }) |]

// Get the current language we're documenting, based on the extension.
let private getLanguage source =
  let extension = Path.GetExtension source
  if languages.ContainsKey(extension)
    then languages.[extension]
    else Unchecked.defaultof<Language>

// Helper function to copy resource files into the target directory from embedded resources.
let private copyResources (settings : Settings) =
  Directory.CreateDirectory settings.TargetDirectory |> ignore
  for resource in seq { yield! settings.Scripts; yield! settings.Stylesheets } do
    let outputFile = Path.Combine(settings.TargetDirectory, resource)
    if not <| File.Exists outputFile then
      try
        use writer = File.CreateText outputFile
        use stream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        if stream = null then
          failwith <| sprintf "Could not find embedded resource '%s'" resource
        use reader = new StreamReader(stream)
        writer.Write(reader.ReadToEnd())
      with _ ->
        try File.Delete outputFile
        with _ -> ()
    else ()

// Setup the Razor templating engine so that we can quickly pass the data in
// and generate HTML.
//
// The file `Resources\Focco.cshtml` is read and compiled into a new dll
// with a type that extends the `TemplateBase` class. This new assembly is
// loaded so that we can create an instance and pass data into it
// and generate the HTML.
let private getTemplateType templateName =
  let host = RazorEngineHost(CSharpRazorCodeLanguage())
  host.DefaultBaseClass <- typeof<TemplateBase>.FullName
  host.DefaultNamespace <- "RazorOutput"
  host.DefaultClassName <- "Template"
  host.NamespaceImports.Add("System") |> ignore

  let templateStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(templateName)
  if templateStream = null then
    failwith <| sprintf "Could not find embedded resource '%s'" templateName

  use reader = new StreamReader(templateStream)
  let razorResult = RazorTemplateEngine(host).GenerateCode(reader)

  let compilerParams =
    CompilerParameters(
      GenerateInMemory = true,
      GenerateExecutable = false,
      IncludeDebugInformation = false,
      CompilerOptions = "/target:library /optimize")
  compilerParams.ReferencedAssemblies.Add(typeof<TemplateBase>.Assembly.CodeBase.Replace("file:///","").Replace("/","\\")) |> ignore

  let codeProvider = new Microsoft.CSharp.CSharpCodeProvider()
  let results = codeProvider.CompileAssemblyFromDom(compilerParams, razorResult.GeneratedCode)

  // Check for errors that may have occurred during template generation.
  if results.Errors.HasErrors then
    results.Errors.OfType<CompilerError>()
    |> Seq.filter (fun x -> not x.IsWarning)
    |> Seq.iter (fun x -> printfn "Error compiling template: (%d, %d) %s" x.Line x.Column x.ErrorText)

  results.CompiledAssembly.GetType("RazorOutput.Template")

// Given a string of source code, parse out each comment and the code that
// follows it, and create an individual `Section` for it.
let private parse source =
  let language = getLanguage(source)

  // Save is a helper function to make the code below easier to read.
  let save docsText codeText sections =
    { DocsHtml = docsText.ToString()
      CodeHtml = codeText.ToString() }
    :: sections

  // Asynchronously fold over the lines and retrieve the final state,
  // consisting of the identified sections and any remaining docs or code text.
  let sections, _, _, docsText, codeText =
    File.ReadLines source
    |> Seq.fold (fun current line ->
      // Unpack the current state values.
      let sections, hasCode, isMultiline, (docsText:StringBuilder), codeText = current 
      // Capture the final multiline comment symbol, and turn off multiline comments.
      if isMultiline && language.IsEndingMultilineComment(line) then
        (sections, hasCode, false, docsText.AppendLine(language.StripMultilineEndComment(line)), codeText)
      // Process the multiline comment and keep going.
      elif isMultiline then
        (sections, hasCode, isMultiline, docsText.AppendLine(line), codeText)
      // Single line comment. This should come before a beginning multiline comment,
      // because a multiline comment may also complete on the same line.
      elif language.IsSinglelineComment(line) then
        if hasCode then
          let sections' = sections |> save docsText codeText
          let docsText' = let sb = StringBuilder() in sb.AppendLine(language.StripSinglelineComment(line))
          (sections', false, false, docsText', new StringBuilder())
        else
          (sections, hasCode, false, docsText.AppendLine(language.StripSinglelineComment(line)), codeText)
      // We're starting a multiline comment!
      elif language.IsStartingMultilineComment(line) then
        if hasCode then
          let sections' = sections |> save docsText codeText
          let docsText' = let sb = StringBuilder() in sb.AppendLine(language.StripMultilineStartComment(line))
          (sections', false, true, docsText', new StringBuilder())
        else
          (sections, hasCode, true, docsText.AppendLine(language.StripMultilineStartComment(line)), codeText)
      // Ignore xml doc comments.
      elif language.CommentFilter.IsMatch(line) then
        (sections, hasCode, isMultiline, docsText, codeText)
      // This line has code.
      else (sections, true, false, docsText, codeText.AppendLine(line)) )
      ([], false, false, StringBuilder(), StringBuilder())

  // Add the remaining items to the sections list, then reverse it back into the correct order.
  sections |> save docsText codeText |> List.rev

// Prepares a single chunk of code for HTML output and runs the text of its
// corresponding comment through **Markdown**, using a C# implementation
// called [MarkdownSharp](http://code.google.com/p/markdownsharp/).
let private highlight sections =
  let markdown = MarkdownSharp.Markdown()
  sections |> Seq.map (fun section ->
    { section with
        DocsHtml = markdown.Transform(section.DocsHtml)
        CodeHtml = System.Web.HttpUtility.HtmlEncode(section.CodeHtml) })

// Compute the destination HTML path for an input source file path. If the source
// is `Example.cs`, the HTML will be at `docs/example.html`
let private getDestination filepath =
  let directories =
    Path.GetDirectoryName(filepath)
      .Substring(1)
      .Split([| Path.DirectorySeparatorChar |], StringSplitOptions.RemoveEmptyEntries)
  let depth = directories.Length
  let destination =
    Path.Combine("docs", String.Join(Path.DirectorySeparatorChar.ToString(), directories)).ToLower()
  Directory.CreateDirectory(destination) |> ignore
  Path.Combine("docs", Path.ChangeExtension(filepath, "html").ToLower()), depth

//### Main Documentation Generation Functions

// Once all of the code is finished highlighting, we can generate the HTML file
// and write out the documentation. Pass the completed sections into the template
// found in `Resources/Focco.cshtml`
let private generateHtml (settings:Settings) (templateType:Type) files source sections =
  let destination, depth = getDestination source
  let pathToRoot = List.fold (fun pathToRoot _ -> Path.Combine("..", pathToRoot)) "" [0..(depth-1)]
  
  let htmlTemplate = Activator.CreateInstance(templateType) :?> TemplateBase

  htmlTemplate.Title <- Path.GetFileName(source)
  htmlTemplate.Stylesheets <- settings.Stylesheets
  htmlTemplate.Scripts <- settings.Scripts
  htmlTemplate.Sections <- sections |> Array.ofSeq
  htmlTemplate.Sources <- files |> Array.ofSeq
  htmlTemplate.GetResourcePath <- Func<_,_>(fun s -> Path.Combine(pathToRoot, s).Replace('\\', '/'))
  htmlTemplate.GetSourcePath <- Func<_,_>(fun s ->
    Path.Combine(pathToRoot, Path.ChangeExtension(s.ToLower(), ".html").Substring(2)).Replace('\\', '/'))

  htmlTemplate.Execute()
  File.WriteAllText(destination, htmlTemplate.Buffer.ToString())

// Generate the documentation for a source file by reading it in, splitting it
// up into comment/code sections, highlighting them for the appropriate language,
// and merging them into an HTML template.
let private generateDocumentation settings templateType files source = async {
  do! Async.SwitchToThreadPool()
  parse source
  |> highlight
  |> generateHtml settings templateType files source
  return source }

// Find all the files that match the pattern(s) passed in as arguments and
// generate documentation for each one.
let generate (settings:Settings) (targets:string[]) =
  // Create the target directory and copy in the stylesheet and javascript files. 
  Directory.CreateDirectory(settings.TargetDirectory) |> ignore
  copyResources settings

  // Collect all the files in the targets.
  let files =
    seq { for target in targets do
            yield! Directory.GetFiles(".", target, SearchOption.AllDirectories)
                   |> Seq.filter (fun filename ->
                      not (getLanguage(Path.GetFileName(filename)) = Unchecked.defaultof<Language>)) }

  let templateType = getTemplateType settings.TemplateName

  // For each file found, create an asynchronous task to generate the output file,
  // then run them all in parallel.
  files
  |> Seq.map (generateDocumentation settings templateType files)
  |> Async.Parallel
  |> Async.RunSynchronously
  |> printfn "Successfully processed the following files: %A"

// The program entry point.
[<EntryPoint>]
let main args =
  #if DEBUG
  let args = [| "Focco.fs" |]
  #endif
  if args.Length > 0 then
    let settings = {
        Scripts = [| "prettify.js" |]
        Stylesheets = [| "focco.css" |]
        TargetDirectory = "docs"
        TemplateName = "Focco.cshtml" }
    generate settings args
  else printfn "Run focco with a filename or path with file extension, e.g. `focco.exe src\\*.fs`."
  0
