#if INTERACTIVE
#r @"..\packages\RazorEngine.2.1\lib\.NetFramework 4.0\System.Web.Razor.dll"
#r @"..\packages\RazorEngine.2.1\lib\.NetFramework 4.0\RazorEngine.dll"
#r @"..\packages\MarkdownSharp.1.13.0.0\lib\35\MarkdownSharp.dll"
#load "Nocco.fs"
#endif

let private main args = Nocco.generate(args)

#if INTERACTIVE
fsi.CommandLineArgs |> Seq.skip 1 |> Array.ofSeq |> main
#else
[<EntryPoint>]
let entryPoint args = main args; 0
#endif
