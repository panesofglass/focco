// **Focco** is a quick-and-dirty, literate-programming-style documentation
// generator. It is a F# port of [Nocco](http://dontangg.github.com/nocco/)
// by [Don Wilson](https://github.com/dontangg/),
// which in turn is a port of [Docco](http://jashkenas.github.com/docco/) for .NET,
// which was written by [Jeremy Ashkenas](https://github.com/jashkenas) in
// Coffescript and runs on node.js.
//
// Focco produces HTML that displays your comments alongside your code.
// Comments are passed through
// [Markdown](http://daringfireball.net/projects/markdown/syntax), and code is
// highlighted using [google-code-prettify](http://code.google.com/p/google-code-prettify/)
// syntax highlighting. This page is the result of running Focco against its
// own source files.
//
// Currently, to build Focco, you'll have to have .NET 4.0 and [F# 2.0](http://github.com/fsharp/fsharp).
// The project depends on
// [MarkdownSharp](http://code.google.com/p/markdownsharp/) and
// [RazorEngine](http://razorengine.codeplex.com/) (for the System.Web.Razor assembly).
//
// To use Focco, run it from the command-line:
//
//     focco *.fs
//
// ...will generate linked HTML documentation for the named source files, saving
// it into a `docs` folder.
//
// The [source for Focco](http://github.com/panesofglass/focco) is available on GitHub, and released under the MIT license.
// The [original source in C#](http://github.com/dontangg/nocco) is also available on GitHub.
//
// If **.NET** doesn't run on your platform, or you'd prefer a more convenient
// package, get [Rocco](http://rtomayko.github.com/rocco/), the Ruby port that's
// available as a gem. If you're writing shell scripts, try
// [Shocco](http://rtomayko.github.com/shocco/), a port for the **POSIX shell**.
// Both are by [Ryan Tomayko](http://github.com/rtomayko). If Python's more
// your speed, take a look at [Nick Fitzgerald](http://github.com/fitzgen)'s
// [Pycco](http://fitzgen.github.com/pycco/).
module Focco =

  // Import namespaces to allow us to type shorter type names.
  open System
  open System.CodeDom.Compiler
  open System.IO
  open System.Linq
  open System.Text
  open System.Web.Razor
  
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
  
  // The template base class for the rendering via Razor.
  [<AbstractClass>]
  type TemplateBase() =
    let mutable buffer = new StringBuilder()
    let mutable title : string = null
    let mutable pathToCss : string = null
    let mutable getSourcePath : Func<string,string> = null
    let mutable sections : Section[] = null
    let mutable sources : string[] = null
    member x.Buffer
      with get() = buffer
      and  set(value) = buffer <- value
    member x.Title
      with get() = title
      and  set(value) = title <- value
    member x.PathToCss
      with get() = pathToCss
      and  set(value) = pathToCss <- value
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
  
  // A list of the languages that Focco supports, mapping the file extension to
  // the symbol that indicates a comment. To add another language to Focco's
  // repertoire, add it here.
  let private languages =
    [|(".js", { Name = "javascript"; Symbol = "//"; MultilineStart = Some "/*"; MultilineEnd = Some "*/" })
      (".fs", { Name = "fsharp"; Symbol = "//"; MultilineStart = Some "/*"; MultilineEnd = Some "*/" })
      (".cs", { Name = "csharp"; Symbol = "//"; MultilineStart = Some "(*"; MultilineEnd = Some "*)" })
      (".vb", { Name = "vb.net"; Symbol = "'"; MultilineStart = None; MultilineEnd = None })
      (".sql", { Name = "sql"; Symbol = "--"; MultilineStart = None; MultilineEnd = None }) |]
    |> dict
  
  let private executingDirectory = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
  
  let private getTemplateType() =
    let host = RazorEngineHost(CSharpRazorCodeLanguage())
    host.DefaultBaseClass <- typeof<TemplateBase>.FullName
    host.DefaultNamespace <- "RazorOutput"
    host.DefaultClassName <- "Template"
    host.NamespaceImports.Add("System") |> ignore
  
    use reader = new StreamReader(Path.Combine(executingDirectory, "Resources", "Focco.cshtml"))
    let razorResult = RazorTemplateEngine(host).GenerateCode(reader)
  
    let compilerParams =
      CompilerParameters(
        GenerateInMemory = true,
        GenerateExecutable = false,
        IncludeDebugInformation = false,
        CompilerOptions = "/target:library /optimize")
    compilerParams.ReferencedAssemblies.
      Add(typeof<TemplateBase>.Assembly.CodeBase.Replace("file:///","").Replace("/","\\")) |> ignore
  
    let codeProvider = new Microsoft.CSharp.CSharpCodeProvider()
    let results = codeProvider.CompileAssemblyFromDom(compilerParams, razorResult.GeneratedCode)
  
    // Check for errors that may have occurred during template generation.
    if results.Errors.HasErrors then
      results.Errors.OfType<CompilerError>()
      |> Seq.filter (fun x -> not x.IsWarning)
      |> Seq.iter (fun x -> printfn "Error compiling template: (%d, %d) %s" x.Line x.Column x.ErrorText)
  
    results.CompiledAssembly.GetType("RazorOutput.Template")
  
  // Get the current language we're documenting, based on the extension.
  let private getLanguage source =
    let extension = Path.GetExtension source
    if languages.ContainsKey(extension)
      then languages.[extension]
      else Unchecked.defaultof<Language>
  
  // Given a string of source code, parse out each comment and the code that
  // follows it, and create an individual `Section` for it.
  let private parse source lines =
    let language = getLanguage(source)
    let save docsText codeText sections =
      { DocsHtml = docsText.ToString()
        CodeHtml = codeText.ToString() }
      :: sections
    let sections, _, docsText, codeText =
      lines |> Seq.fold (fun state line ->
        let sections, hasCode, docsText, codeText = state
        if language.CommentMatcher.IsMatch(line) &&
          not (language.CommentFilter.IsMatch(line)) then
          if hasCode then
            let sections' = sections |> save docsText codeText
            let docsText' =
              let sb = new StringBuilder()
              in sb.AppendLine(language.CommentMatcher.Replace(line,""))
            (sections', false, docsText', new StringBuilder())
          else
            (sections, hasCode, docsText.AppendLine(language.CommentMatcher.Replace(line, "")), codeText)
        else (sections, true, docsText, codeText.AppendLine(line)))
        ([], false, StringBuilder(), StringBuilder())
    sections |> save docsText codeText |> List.rev
  
  // Prepares a single chunk of code for HTML output and runs the text of its
  // corresponding comment through **Markdown**, using a C# implementation
  // called [MarkdownSharp](http://code.google.com/p/markdownsharp/).
  let private highlight source sections =
    let markdown = MarkdownSharp.Markdown()
    sections |> Seq.map (fun section ->
      { section with
          DocsHtml = markdown.Transform(section.DocsHtml)
          CodeHtml = System.Web.HttpUtility.HtmlEncode(section.CodeHtml) })
  
  // Compute the destination HTML path for an input source file path. If the source
  // is `Example.cs`, the HTML will be at `docs/example.html`
  let private getDestination filepath =
    let directories = Path.GetDirectoryName(filepath).Substring(1).Split([| Path.DirectorySeparatorChar |], StringSplitOptions.RemoveEmptyEntries)
    let depth = directories.Length
    let destination = Path.Combine("docs", String.Join(Path.DirectorySeparatorChar.ToString(), directories)).ToLower()
    Directory.CreateDirectory(destination) |> ignore
    Path.Combine("docs", Path.ChangeExtension(filepath, "html").ToLower()), depth
  
  // Once all of the code is finished highlighting, we can generate the HTML file
  // and write out the documentation. Pass the completed sections into the template
  // found in `Resources/Focco.cshtml`
  let private generateHtml source files sections =
    let destination, depth = getDestination source
    let pathToRoot = List.fold (fun pathToRoot _ -> Path.Combine("..", pathToRoot)) "" [0..(depth-1)]
  
    let templateType = getTemplateType()
    let htmlTemplate = Activator.CreateInstance(templateType) :?> TemplateBase

    htmlTemplate.Title <- Path.GetFileName(source)
    htmlTemplate.PathToCss <- Path.Combine(pathToRoot, "nocco.css").Replace('\\', '/')
    htmlTemplate.Sections <- sections |> Array.ofSeq
    htmlTemplate.Sources <- files |> Array.ofSeq
    htmlTemplate.GetSourcePath <- Func<_,_>(fun s ->
      Path.Combine(pathToRoot, Path.ChangeExtension(s.ToLower(), ".html").Substring(2)).Replace('\\', '/'))
  
    htmlTemplate.Execute()
    File.WriteAllText(destination, htmlTemplate.Buffer.ToString())
  
  //### Main Documentation Generation Functions
  
  // Generate the documentation for a source file by reading it in, splitting it
  // up into comment/code sections, highlighting them for the appropriate language,
  // and merging them into an HTML template.
  let private generateDocumentation files source =
    File.ReadAllLines source
    |> parse source
    |> highlight source
    |> generateHtml source files
  
  // Find all the files that match the pattern(s) passed in as arguments and
  // generate documentation for each one.
  let generate (targets:string[]) =
    if targets.Length <= 0 then
      failwith "At least one target must be specified"
    else
      Directory.CreateDirectory("docs") |> ignore
      File.Copy(Path.Combine(executingDirectory, "Resources", "Focco.css"), Path.Combine("docs", "nocco.css"), true)
      File.Copy(Path.Combine(executingDirectory, "Resources", "prettify.js"), Path.Combine("docs", "prettify.js"), true)
      let files =
        [ for target in targets do
            yield! Directory.GetFiles(".", target, SearchOption.AllDirectories)
                   |> Seq.filter (fun filename ->
                      not (getLanguage(Path.GetFileName(filename)) = Unchecked.defaultof<Language>)) ]
      for file in files do generateDocumentation files file

// The program entry point.
[<EntryPoint>] let main args = Focco.generate args; 0

