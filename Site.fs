module AlphaShelf.Site

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open AlphaShelf.Model
open AlphaShelf.BookStore
open AlphaShelf.Queries
open AlphaShelf.Render

let private writeHtml (ctx: HttpContext) (html: string) =
    ctx.Response.ContentType <- "text/html; charset=utf-8"
    ctx.Response.WriteAsync(html)

let private writeText (ctx: HttpContext) (text: string) =
    ctx.Response.ContentType <- "text/plain; charset=utf-8"
    ctx.Response.WriteAsync(text)

let private getForm (form: IFormCollection) (name: string) =
    let mutable v = StringValues.Empty

    if form.TryGetValue(name, &v) then
        (v.ToString()).Trim()
    else
        ""

let mapRoutes (app: WebApplication) =
    app.MapGet("/health", RequestDelegate(fun ctx -> writeText ctx "AlphaShelf ok"))
    |> ignore

    app.MapGet("/", RequestDelegate(fun ctx -> writeHtml ctx homeView))
    |> ignore

    app.MapGet("/shelf", RequestDelegate(fun ctx -> writeHtml ctx (shelfView (all ()) None)))
    |> ignore

    app.MapGet("/stats", RequestDelegate(fun ctx -> writeHtml ctx (statsView (all ()))))
    |> ignore

    app.MapGet("/plain", RequestDelegate(fun ctx -> writeHtml ctx (plainView (all ()))))
    |> ignore

    app.MapGet(
        "/export.txt",
        RequestDelegate(fun ctx ->
            let body = all () |> plaintextLines |> String.concat "\r\n"
            writeText ctx body))
    |> ignore

    app.MapPost(
        "/shelf/add",
        RequestDelegate(fun ctx ->
            task {
                let! form = ctx.Request.ReadFormAsync()
                let title = getForm form "title"
                let author = getForm form "author"
                let stRaw = getForm form "st"
                let st = statusFromForm stRaw |> Option.defaultValue ToRead

                match parsePages (getForm form "pages") with
                | None ->
                    return! writeHtml ctx (shelfView (all ()) (Some "Pages must be empty or a non-negative integer."))
                | Some pagesOpt ->
                    if tryAdd title author st pagesOpt then
                        return! writeHtml ctx (shelfView (all ()) None)
                    else
                        return! writeHtml ctx (shelfView (all ()) (Some "Title was empty after trimming."))
            })
    )
    |> ignore

    app.MapPost(
        "/shelf/status",
        RequestDelegate(fun ctx ->
            task {
                let! form = ctx.Request.ReadFormAsync()

                let msg =
                    match Int32.TryParse(getForm form "id"), statusFromForm (getForm form "st") with
                    | (true, id), Some st ->
                        if trySetStatus id st then
                            None
                        else
                            Some "Unknown id."
                    | _, None -> Some "Unknown status value."
                    | _ -> Some "Bad id."

                return! writeHtml ctx (shelfView (all ()) msg)
            })
    )
    |> ignore

    app.MapPost(
        "/shelf/remove",
        RequestDelegate(fun ctx ->
            task {
                let! form = ctx.Request.ReadFormAsync()

                let msg =
                    match Int32.TryParse(getForm form "id") with
                    | true, id ->
                        if tryRemove id then
                            None
                        else
                            Some "Unknown id."
                    | _ -> Some "Bad id."

                return! writeHtml ctx (shelfView (all ()) msg)
            })
    )
    |> ignore
