namespace ReadShelfWs

open WebSharper

[<JavaScript>]
module ShelfDomain =
    type ReadingStatus =
        | ToRead
        | Reading
        | Done

    type Book =
        {
            Title: string
            Author: string option
            Status: ReadingStatus
            Pages: int option
        }

    module ReadingStatus =
        let label =
            function
            | ToRead -> "To read"
            | Reading -> "Reading"
            | Done -> "Done"

        let parse (s: string) =
            match s.Trim().ToLower() with
            | "reading" -> Some Reading
            | "done" -> Some Done
            | "toread"
            | "to-read"
            | "want" -> Some ToRead
            | _ -> None

        /// Values from the Add book HTML select.
        let ofForm (s: string) =
            match s.Trim() with
            | "Reading" -> Reading
            | "Done" -> Done
            | _ -> ToRead

        let cycle =
            function
            | ToRead -> Reading
            | Reading -> Done
            | Done -> ToRead

    module Book =
        let trimTitle (t: string) =
            let u = t.Trim()
            if u.Length > 200 then u.Substring(0, 200) else u

        let normalizeAuthor (a: string) =
            let u = a.Trim()
            if u.Length = 0 then
                None
            elif u.Length > 120 then
                Some(u.Substring(0, 120))
            else
                Some u

        let parsePages (raw: string) =
            let u = raw.Trim()
            if u.Length = 0 then
                Some None
            else
                match System.Int32.TryParse(u) with
                | true, n when n >= 0 -> Some(Some n)
                | _ -> None

        let make title author status pages : Book =
            {
                Title = trimTitle title
                Author = normalizeAuthor author
                Status = status
                Pages = pages
            }

        let withStatus st b = { b with Status = st }

        let displayLine (b: Book) =
            let auth =
                match b.Author with
                | None -> ""
                | Some a -> sprintf " — %s" a

            let pg =
                match b.Pages with
                | None -> ""
                | Some p -> sprintf " (%i pp.)" p

            sprintf "%s%s [%s]%s" b.Title auth (ReadingStatus.label b.Status) pg

    module ShelfQueries =
        let byStatus st books = books |> List.filter (fun b -> b.Status = st)

        let titlesSorted books =
            books |> List.sortBy (fun b -> b.Title.ToLower())

        let countWhere pred books = books |> List.filter pred |> List.length

        let summaryLines (books: Book list) =
            if List.isEmpty books then
                [ "No books on the shelf." ]
            else
                let n = books.Length
                let d = countWhere (fun b -> b.Status = Done) books
                let r = countWhere (fun b -> b.Status = Reading) books
                let t = countWhere (fun b -> b.Status = ToRead) books
                [ sprintf "%i books" n
                  sprintf "Done %i · Reading %i · To read %i" d r t ]

        let plaintextExport (books: Book list) =
            books
            |> titlesSorted
            |> List.map Book.displayLine

        let totalPages (books: Book list) =
            books |> List.choose (fun b -> b.Pages) |> List.sum

        let averagePagesWhereSet (books: Book list) =
            let xs = books |> List.choose (fun b -> b.Pages)

            if List.isEmpty xs then
                None
            else
                Some(float (List.sum xs) / float xs.Length)

        let titlesLowerSet (books: Book list) =
            books |> List.map (fun b -> b.Title.ToLower()) |> Set.ofList

        let hasDuplicateTitle (title: string) (books: Book list) =
            let key = title.Trim().ToLower()
            books |> List.exists (fun b -> b.Title.ToLower() = key)

        let private statusSortKey =
            function
            | ToRead -> 0
            | Reading -> 1
            | Done -> 2

        /// `sortKey`: `"title"` | `"status"` | `"pages"`.
        let sortBooks (sortKey: string) (books: Book list) =
            let sk = sortKey.Trim().ToLower()

            match sk with
            | "status" ->
                books |> List.sortBy (fun b -> statusSortKey b.Status, b.Title.ToLower())
            | "pages" ->
                books
                |> List.sortBy (fun b ->
                    match b.Pages with
                    | Some p -> (0, p)
                    | None -> (1, 0))
            | _ -> titlesSorted books

        let extraStatsLine (books: Book list) =
            if List.isEmpty books then
                ""
            else
                let tp = totalPages books

                let ap =
                    averagePagesWhereSet books |> Option.map (sprintf "%.0f pp.") |> Option.defaultValue "n/a"

                sprintf "Recorded pages (sum): %i · Average where known: %s" tp ap

        let countByStatus (books: Book list) =
            [ ToRead; Reading; Done ]
            |> List.map (fun st -> st, byStatus st books |> List.length)

        let markdownTable (books: Book list) =
            let header = "| Title | Author | Status | Pages |"
            let sep = "| --- | --- | --- | --- |"

            let row (b: Book) =
                let auth = b.Author |> Option.defaultValue ""
                let pg = b.Pages |> Option.map string |> Option.defaultValue ""
                sprintf "| %s | %s | %s | %s |" b.Title auth (ReadingStatus.label b.Status) pg

            header :: sep :: (books |> titlesSorted |> List.map row)

    module Validation =
        type AddResult =
            | Ok of Book
            | Err of string

        let tryBuildBook (titleRaw: string) (authorRaw: string) (pagesRaw: string) (status: ReadingStatus) =
            let t = Book.trimTitle titleRaw

            if t.Length = 0 then
                Err "Title cannot be empty."
            else
                match Book.parsePages pagesRaw with
                | None -> Err "Pages must be blank or a non-negative integer."
                | Some pages ->
                    Ok(Book.make t authorRaw status pages)

        /// Reject duplicate titles (case-insensitive) against the current shelf.
        let tryAddBook (titleRaw: string) (authorRaw: string) (pagesRaw: string) (status: ReadingStatus) (existing: Book list) =
            let t = Book.trimTitle titleRaw

            if t.Length > 0 && ShelfQueries.hasDuplicateTitle t existing then
                Err "A book with that title is already on the shelf."
            else
                tryBuildBook titleRaw authorRaw pagesRaw status

        /// Update author, pages, and status for an existing title (exact match).
        let tryUpdateBookAtTitle (title: string) (authorRaw: string) (pagesRaw: string) (status: ReadingStatus) (books: Book list) : Result<Book list, string> =
            match books |> List.tryFind (fun b -> b.Title = title) with
            | None -> Result.Error "That title is not on the shelf."
            | Some _ ->
                match Book.parsePages pagesRaw with
                | None -> Result.Error "Pages must be blank or a non-negative integer."
                | Some pages ->
                    Result.Ok(
                        books
                        |> List.map (fun x ->
                            if x.Title = title then
                                { x with
                                    Author = Book.normalizeAuthor authorRaw
                                    Pages = pages
                                    Status = status }
                            else
                                x))

    let seedBooks () =
        [
            Book.make "Expert F#" "" Reading None
            Book.make "TAPL" "Pierce" ToRead (Some 623)
            Book.make "Domain Modeling Made Functional" "Wlaschin" Done (Some 432)
        ]

    module TextTools =
        let collapseSpaces (s: string) =
            let parts = s.Split([| ' '; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)
            System.String.Join(" ", parts)

        let truncate (maxLen: int) (s: string) =
            if s.Length <= maxLen then
                s
            else
                s.Substring(0, maxLen) + "…"

        let containsIgnoreCase (needle: string) (hay: string) =
            hay.ToLower().Contains(needle.ToLower())

        let startsWithIgnoreCase (prefix: string) (s: string) =
            s.Trim().ToLower().StartsWith(prefix.Trim().ToLower())

        let wordCount (s: string) =
            if s.Trim().Length = 0 then
                0
            else
                s.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries).Length

        let reverseWords (s: string) =
            let parts = s.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
            parts |> Array.rev |> String.concat " "

    module ListExtras =
        let distinctByTitleLower (books: Book list) =
            let rec walk seen acc rest =
                match rest with
                | [] -> List.rev acc
                | b :: tail ->
                    let k = b.Title.ToLower()

                    if Set.contains k seen then
                        walk seen acc tail
                    else
                        walk (Set.add k seen) (b :: acc) tail

            walk Set.empty [] books

        let partitionByStatus (books: Book list) =
            (ShelfQueries.byStatus ToRead books, ShelfQueries.byStatus Reading books, ShelfQueries.byStatus Done books)

        let takeLast n (books: Book list) =
            let xs = List.rev books
            xs |> List.truncate n |> List.rev

    /// One line per book: title TAB author TAB statusLetter TAB pages (machine line, not full CSV).
    module ShelfPersistence =
        let private stLetter =
            function
            | ToRead -> "T"
            | Reading -> "R"
            | Done -> "D"

        let private letterStatus (c: string) =
            match c.Trim() with
            | "R" -> Reading
            | "D" -> Done
            | _ -> ToRead

        let encodeLine (b: Book) =
            let auth = b.Author |> Option.defaultValue "" |> fun s -> s.Replace("\t", " ")
            let pg = b.Pages |> Option.map string |> Option.defaultValue ""
            let t = Book.trimTitle b.Title |> fun s -> s.Replace("\t", " ")
            sprintf "%s\t%s\t%s\t%s" t auth (stLetter b.Status) pg

        let encodeShelf (books: Book list) =
            books |> List.map encodeLine |> String.concat "\n"

        let tryParseLine (line: string) : Book option =
            let parts = line.Split([| '\t' |])

            if parts.Length < 4 then
                None
            else
                let title = parts.[0].Trim()
                let authorRaw = parts.[1]
                let status = letterStatus parts.[2]

                if title.Length = 0 then
                    None
                else
                    match Book.parsePages parts.[3] with
                    | None -> None
                    | Some pages -> Some(Book.make title authorRaw status pages)

        let tryDecodeShelf (text: string) : Book list option =
            if System.String.IsNullOrWhiteSpace text then
                Some []
            else
                let lines =
                    text.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList

                let parsed = lines |> List.map tryParseLine

                if List.exists Option.isNone parsed then
                    None
                else
                    Some(parsed |> List.choose id)

    module ShelfScore =
        let doneRatio (books: Book list) =
            if List.isEmpty books then
                0.0
            else
                let d = ShelfQueries.countWhere (fun b -> b.Status = Done) books |> float
                d / float books.Length

        let readingProgressScore (books: Book list) =
            let r = doneRatio books
            let avg = ShelfQueries.averagePagesWhereSet books |> Option.defaultValue 0.0
            let n = float (max 1 books.Length)
            int (100.0 * r + min 20.0 (avg / 50.0) + 5.0 * System.Math.Log(n))

    module ShelfAudit =
        let findMissingAuthors (books: Book list) =
            books |> List.filter (fun b -> Option.isNone b.Author) |> List.map (fun b -> b.Title)

        let findMissingPages (books: Book list) =
            books |> List.filter (fun b -> Option.isNone b.Pages) |> List.map (fun b -> b.Title)

        let heavyBooks minPages (books: Book list) =
            books |> List.filter (fun b -> match b.Pages with | Some p -> p >= minPages | None -> false)

        let statusHistogramText (books: Book list) =
            ShelfQueries.countByStatus books
            |> List.map (fun (st, c) -> sprintf "%s=%i" (ReadingStatus.label st) c)
            |> String.concat ", "

        let private firstToReadTitle (books: Book list) =
            ShelfQueries.byStatus ToRead books |> ShelfQueries.titlesSorted |> List.tryHead |> Option.map (fun b -> b.Title)

        let explainShelf (books: Book list) =
            if List.isEmpty books then
                "The shelf is empty."
            else
                let missA = findMissingAuthors books |> List.length
                let missP = findMissingPages books |> List.length
                let score = ShelfScore.readingProgressScore books

                let tip =
                    match firstToReadTitle books with
                    | None -> "No titles are queued as “to read”."
                    | Some t -> sprintf "Suggested next read: “%s”." t

                sprintf
                    "You have %i books. %i lack an author and %i lack a page count. Shelf score: %i. %s"
                    books.Length
                    missA
                    missP
                    score
                    tip
