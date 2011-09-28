#if INTERACTIVE
#r @"..\packages\RazorEngine.2.1\lib\.NetFramework 4.0\System.Web.Razor.dll"
#r @"..\packages\RazorEngine.2.1\lib\.NetFramework 4.0\RazorEngine.dll"
#endif

open RazorEngine

type Model = { Name : string }
let template = "Hello @Model.Name!"
let result = Razor.Parse(template, { Name = "Ryan" })
printfn "%s" result
