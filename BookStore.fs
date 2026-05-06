module AlphaShelf.BookStore

open System
open System.Collections.Generic
open AlphaShelf.Model

let private gate = obj ()
let private items = ResizeArray<Book> ()
let mutable private nextId = 1

let private seedIfEmpty () =
    if items.Count = 0 then
        let mk t a st pg =
            let row =
                {
                    Id = nextId
                    Title = t
                    Author = a
                    Status = st
                    Pages = pg
                    AddedUtc = DateTime.UtcNow
                }

            nextId <- nextId + 1
            items.Add(row)

        mk "Types and Programming Languages" (Some "Pierce") ToRead (Some 623)
        mk "Expert F# 4.0" None Reading None
        mk "Domain Modeling Made Functional" (Some "Wlaschin") Done (Some 432)

let all () =
    lock gate (fun () ->
        seedIfEmpty ()
        items |> Seq.toList |> List.sortBy (fun b -> b.Id))

let tryAdd (title: string) (author: string) (status: ReadingStatus) (pages: int option) =
    let t = trimTitle title

    if t.Length = 0 then
        false
    else
        lock gate (fun () ->
            seedIfEmpty ()

            items.Add(
                {
                    Id = nextId
                    Title = t
                    Author = trimAuthor author
                    Status = status
                    Pages = pages
                    AddedUtc = DateTime.UtcNow
                })

            nextId <- nextId + 1
            true)

let trySetStatus (id: int) (st: ReadingStatus) =
    lock gate (fun () ->
        match items |> Seq.tryFindIndex (fun b -> b.Id = id) with
        | Some i ->
            let b = items.[i]
            items.[i] <- { b with Status = st }
            true
        | None -> false)

let tryRemove (id: int) =
    lock gate (fun () ->
        match items |> Seq.tryFindIndex (fun b -> b.Id = id) with
        | Some i ->
            items.RemoveAt(i)
            true
        | None -> false)
