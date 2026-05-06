module AlphaShelf.Queries

open AlphaShelf.Model

let groupCounts (books: Book list) =
    let inc m k =
        match Map.tryFind k m with
        | Some n -> Map.add k (n + 1) m
        | None -> Map.add k 1 m

    let folder m b =
        let lab = statusLabel b.Status
        inc m lab

    books |> List.fold folder Map.empty |> Map.toList |> List.sortBy fst

let pagesKnown (books: Book list) =
    books |> List.choose (fun b -> b.Pages)

let averagePages (books: Book list) =
    let xs = pagesKnown books

    if List.isEmpty xs then
        None
    else
        let s = List.sum xs
        Some(float s / float xs.Length)

let totalPagesTracked (books: Book list) =
    pagesKnown books |> List.sum

let titlesSorted (books: Book list) =
    books |> List.sortBy (fun b -> b.Title.ToLowerInvariant())

let byStatus (st: ReadingStatus) (books: Book list) =
    books |> List.filter (fun b -> b.Status = st)

let recentFirst (books: Book list) =
    books |> List.sortByDescending (fun b -> b.AddedUtc)

let plaintextLines (books: Book list) =
    books
    |> titlesSorted
    |> List.map (fun b ->
        let auth =
            match b.Author with
            | None -> ""
            | Some a -> sprintf " — %s" a

        let pg =
            match b.Pages with
            | None -> ""
            | Some p -> sprintf " (%i pp.)" p

        sprintf "%s%s [%s]%s" b.Title auth (statusLabel b.Status) pg)

let summaryParagraph (books: Book list) =
    let n = books.Length

    let buckets = groupCounts books

    let parts =
        buckets |> List.map (fun (lab, c) -> sprintf "%s: %i" lab c) |> String.concat "; "

    let avg = averagePages books

    let tail =
        match avg with
        | None -> "No page counts entered."
        | Some a -> sprintf "Mean page count (where set): %.1f." a

    sprintf "You have %i books. %s. %s" n parts tail
