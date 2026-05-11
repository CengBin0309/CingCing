namespace ReadShelfWs

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    [<Inline "window.localStorage.getItem($k) || ''">]
    let private lsGet (k: string) : string = ""

    [<Inline "window.localStorage.setItem($k,$v)">]
    let private lsSet (k: string) (v: string) : unit = ()

    let private storageKey = "readShelf.v1"

    let private initialBooks () =
        let raw = lsGet storageKey

        match ShelfDomain.ShelfPersistence.tryDecodeShelf raw with
        | Some bs when not (List.isEmpty bs) -> bs
        | _ -> ShelfDomain.seedBooks ()

    let shelf = Var.Create (initialBooks ())

    let sortKey = Var.Create "title"

    let editOpen = Var.Create false
    let editTarget = Var.Create ""
    let edAuthor = Var.Create ""
    let edPages = Var.Create ""
    let edStatus = Var.Create "ToRead"

    let Log = ListModel.FromSeq [ "Ready." ]

    let auditText =
        Var.Create(ShelfDomain.ShelfAudit.explainShelf (ShelfDomain.seedBooks ()))

    let private persist () =
        shelf.Value |> ShelfDomain.ShelfPersistence.encodeShelf |> lsSet storageKey

    let private auditDoc =
        auditText.View
        |> Doc.BindView (fun s -> Doc.Element "pre" [ Attr.Class "audit" ] [ Doc.TextNode s ])

    let private logPush (line: string) = Log.Add(line)

    let private refreshAudit () =
        auditText.Value <- ShelfDomain.ShelfAudit.explainShelf shelf.Value

    let private subDoc =
        shelf.View
        |> Doc.BindView (fun books ->
            let l1 =
                ShelfDomain.ShelfQueries.summaryLines books |> String.concat " · "

            let l2 = ShelfDomain.ShelfQueries.extraStatsLine books

            Doc.Element "div" [] [
                Doc.Element "p" [ Attr.Class "sub-lead" ] [ Doc.TextNode l1 ]
                if l2 <> "" then
                    Doc.Element "p" [ Attr.Class "sub-muted" ] [ Doc.TextNode l2 ]
                else
                    Doc.Empty
            ])

    let private formHintDoc (formHint: Var<string>) =
        formHint.View
        |> Doc.BindView (fun s -> Doc.Element "div" [ Attr.Class "form-msg-inner" ] [ Doc.TextNode s ])

    let private sortedBooks =
        View.Map2 (fun books sk -> ShelfDomain.ShelfQueries.sortBooks sk books) shelf.View sortKey.View

    let private removeFirstByTitle (title: string) (books: ShelfDomain.Book list) =
        let rec go (acc: ShelfDomain.Book list) =
            function
            | [] -> List.rev acc
            | (h: ShelfDomain.Book) :: t when h.Title = title -> List.rev acc @ t
            | h :: t -> go (h :: acc) t

        go [] books

    let private authorText (b: ShelfDomain.Book) =
        b.Author |> Option.defaultValue "(no author)"

    let private pagesText (b: ShelfDomain.Book) =
        b.Pages |> Option.map string |> Option.defaultValue "—"

    let private statusSelectValue (b: ShelfDomain.Book) =
        match ShelfDomain.ReadingStatus.label b.Status with
        | "Reading" -> "Reading"
        | "Done" -> "Done"
        | _ -> "ToRead"

    let private bookRowsDoc =
        sortedBooks.DocSeqCached(fun (b: ShelfDomain.Book) ->
            let title = b.Title

            IndexTemplate.BookRow()
                .Title(b.Title)
                .Author(authorText b)
                .Status(ShelfDomain.ReadingStatus.label b.Status)
                .Pages(pagesText b)
                .StartEdit(fun _ ->
                    editTarget.Value <- title
                    edAuthor.Value <- (b.Author |> Option.defaultValue "")
                    edPages.Value <- (b.Pages |> Option.map string |> Option.defaultValue "")
                    edStatus.Value <- statusSelectValue b
                    editOpen.Value <- true
                    logPush (sprintf "Editing “%s”." title))
                .Advance(fun _ ->
                    shelf.Value <-
                        shelf.Value
                        |> List.map (fun x ->
                            if x.Title = title then
                                ShelfDomain.Book.withStatus (ShelfDomain.ReadingStatus.cycle x.Status) x
                            else
                                x)

                    logPush (sprintf "Advanced status for “%s”." title)
                    persist ()
                    refreshAudit ())
                .Remove(fun _ ->
                    shelf.Value <- removeFirstByTitle title shelf.Value
                    logPush (sprintf "Removed “%s”." title)
                    persist ()
                    refreshAudit ())
                .Doc())

    let private editSheetDoc =
        View.Map2 (fun open' t -> open', t) editOpen.View editTarget.View
        |> Doc.BindView (fun (open', t) ->
            if not open' || t = "" then
                Doc.Empty
            else
                IndexTemplate.EditForm()
                    .EdTitle(t)
                    .EdAuthor(edAuthor)
                    .EdPages(edPages)
                    .EdStatus(edStatus)
                    .SaveEdit(fun _ ->
                        let st = ShelfDomain.ReadingStatus.ofForm edStatus.Value

                        match
                            ShelfDomain.Validation.tryUpdateBookAtTitle t edAuthor.Value edPages.Value st shelf.Value
                        with
                        | Error msg -> logPush msg
                        | Ok next ->
                            shelf.Value <- next
                            editOpen.Value <- false
                            logPush (sprintf "Updated “%s”." t)
                            persist ()
                            refreshAudit ())
                    .CancelEdit(fun _ ->
                        editOpen.Value <- false
                        logPush "Edit cancelled.")
                    .Doc())

    [<SPAEntryPoint>]
    let Main () =
        let addTitle = Var.Create ""
        let addAuthor = Var.Create ""
        let addPages = Var.Create ""
        let addStatus = Var.Create "ToRead"
        let formHint = Var.Create ""

        auditText.Value <- ShelfDomain.ShelfAudit.explainShelf shelf.Value

        subDoc |> Doc.RunById "sub"
        auditDoc |> Doc.RunById "audit"
        refreshAudit ()

        IndexTemplate.Main()
            .SortKey(sortKey)
            .Books(bookRowsDoc)
            .AddTitle(addTitle)
            .AddAuthor(addAuthor)
            .AddPages(addPages)
            .AddStatus(addStatus)
            .AddBook(fun _ ->
                let st = ShelfDomain.ReadingStatus.ofForm addStatus.Value

                match
                    ShelfDomain.Validation.tryAddBook addTitle.Value addAuthor.Value addPages.Value st shelf.Value
                with
                | ShelfDomain.Validation.Err msg ->
                    formHint.Value <- msg
                    logPush msg
                | ShelfDomain.Validation.Ok book ->
                    shelf.Value <- shelf.Value @ [ book ]
                    formHint.Value <- ""
                    addTitle.Value <- ""
                    addAuthor.Value <- ""
                    addPages.Value <- ""
                    addStatus.Value <- "ToRead"
                    logPush (sprintf "Added “%s” to the shelf." book.Title)
                    persist ()
                    refreshAudit ())
            .RefreshLog(fun _ ->
                shelf.Value <- ShelfDomain.seedBooks ()
                formHint.Value <- ""
                editOpen.Value <- false
                persist ()
                refreshAudit ()
                logPush "Restored the sample shelf.")
            .RpcUtc(fun _ ->
                async {
                    let! s = ServerShelfRpc.ServerUtc()
                    logPush (sprintf "RPC ServerUtc: %s" s)
                }
                |> Async.Start)
            .Log(
                Log.View.DocSeqCached(fun (msg: string) ->
                    IndexTemplate.LogItem().Msg(msg).Doc())
            )
            .Doc()
        |> Doc.RunById "main"

        formHintDoc formHint |> Doc.RunById "formhint"
        editSheetDoc |> Doc.RunById "editsheet"
