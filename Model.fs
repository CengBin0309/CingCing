module AlphaShelf.Model

open System

type ReadingStatus =
    | ToRead
    | Reading
    | Done

type Book =
    {
        Id: int
        Title: string
        Author: string option
        Status: ReadingStatus
        Pages: int option
        AddedUtc: DateTime
    }

let statusLabel =
    function
    | ToRead -> "To read"
    | Reading -> "Reading"
    | Done -> "Done"

let statusFromForm (s: string) =
    match s.Trim().ToLowerInvariant() with
    | "reading" -> Some Reading
    | "done" -> Some Done
    | "toread"
    | "to-read"
    | "want" -> Some ToRead
    | _ -> None

let parsePages (t: string) =
    let u = t.Trim()

    if u.Length = 0 then
        Some None
    else
        match Int32.TryParse(u) with
        | true, n when n >= 0 -> Some(Some n)
        | _ -> None

let trimTitle (t: string) =
    let s = t.Trim()
    if s.Length > 200 then s.Substring(0, 200) else s

let trimAuthor (t: string) =
    let s = t.Trim()

    if s.Length = 0 then
        None
    elif s.Length > 120 then
        Some(s.Substring(0, 120))
    else
        Some s
