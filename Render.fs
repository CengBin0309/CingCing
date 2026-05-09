module AlphaShelf.Render

open System
open System.Net
open AlphaShelf.Model
open AlphaShelf.Queries

let esc (s: string | null) : string =
    let u =
        match s with
        | null -> ""
        | x -> x

    let r = WebUtility.HtmlEncode(u)

    match r with
    | null -> ""
    | x -> x

let layout (title: string) (inner: string) =
    sprintf
        """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>%s</title>
<link rel="stylesheet" href="/css/shelf.css"/>
</head>
<body>
<header class="bar">
  <a class="name" href="/">AlphaShelf</a>
  <nav>
    <a href="/">Home</a>
    <a href="/shelf">Shelf</a>
    <a href="/stats">Stats</a>
    <a href="/plain">Plain list</a>
    <a href="/export.txt">Export</a>
  </nav>
</header>
<main class="main">%s</main>
<footer class="end">Alpha assignment — in-memory data only.</footer>
</body>
</html>"""
        (esc title)
        inner

let homeView =
    layout
        "AlphaShelf"
        """<section class="panel">
<h1>Reading shelf</h1>
<p class="sub">Add books, flip status, skim counts. Nothing is written to disk; restarting the process clears the list except the built-in seed on first run.</p>
<p><a class="go" href="/shelf">Open shelf</a> · <a class="go" href="/stats">Stats</a> · <a class="go" href="/export.txt">Download plain list</a></p>
</section>"""

let statusOptions (current: ReadingStatus) =
    let cur =
        match current with
        | ToRead -> "toread"
        | Reading -> "reading"
        | Done -> "done"

    let opt v lab =
        if v = cur then
            sprintf """<option value="%s" selected>%s</option>""" v lab
        else
            sprintf """<option value="%s">%s</option>""" v lab

    String.concat "" [ opt "toread" "To read"; opt "reading" "Reading"; opt "done" "Done" ]

let rowView (b: Book) =
    sprintf
        """<tr>
<td class="t">%s</td>
<td class="a">%s</td>
<td>%s</td>
<td>%s</td>
<td class="acts">
<form method="post" action="/shelf/status" class="inl"><input type="hidden" name="id" value="%i"/>
<select name="st" onchange="this.form.submit()">%s</select>
</form>
<form method="post" action="/shelf/remove" class="inl" onsubmit="return confirm('Remove this row?');">
<input type="hidden" name="id" value="%i"/><button type="submit" class="linkbtn">Remove</button>
</form>
</td>
</tr>"""
        (esc b.Title)
        (match b.Author with
         | None -> "—"
         | Some a -> esc a)
        (match b.Pages with
         | None -> "—"
         | Some p -> string p)
        (esc (statusLabel b.Status))
        b.Id
        (statusOptions b.Status)
        b.Id

let shelfView (books: Book list) (msg: string option) =
    let banner =
        match msg with
        | None -> ""
        | Some t -> sprintf """<p class="note">%s</p>""" (esc t)

    let rows =
        if List.isEmpty books then
            "<tr><td colspan=\"5\" class=\"muted\">No rows yet.</td></tr>"
        else
            books |> recentFirst |> List.map rowView |> String.concat ""

    layout
        "Shelf"
        (sprintf
            """%s
<section class="panel">
<h1>Your shelf</h1>
<table class="grid">
<thead><tr><th>Title</th><th>Author</th><th>Pages</th><th>Status</th><th></th></tr></thead>
<tbody>%s</tbody>
</table>
</section>
<section class="panel">
<h2>Add</h2>
<form method="post" action="/shelf/add" class="stack">
<label>Title <input name="title" type="text" maxlength="200" required/></label>
<label>Author <input name="author" type="text" maxlength="120"/></label>
<label>Pages <input name="pages" type="number" min="0" step="1" placeholder="optional"/></label>
<label>Status
<select name="st"><option value="toread">To read</option><option value="reading">Reading</option><option value="done">Done</option></select>
</label>
<button type="submit" class="go">Save</button>
</form>
<p class="sub"><a href="/">Home</a> · <a href="/stats">Stats</a> · <a href="/plain">Plain list</a></p>
</section>"""
            banner
            rows)

let statsView (books: Book list) =
    let para = summaryParagraph books
    let lines = groupCounts books |> List.map (fun (k, v) -> sprintf "<li><strong>%s</strong>: %i</li>" (esc k) v) |> String.concat ""

    let totalPg = totalPagesTracked books

    let pgLine =
        if totalPg = 0 then
            "<p class=\"muted\">Sum of pages is zero (most rows have blank pages).</p>"
        else
            sprintf "<p>Sum of entered page counts: <strong>%i</strong></p>" totalPg

    layout
        "Stats"
        (sprintf
            """<section class="panel">
<h1>Stats</h1>
<p>%s</p>
<ul class="lst">%s</ul>
%s
<p class="sub">Done count: %i · Reading: %i · To read: %i</p>
<p class="sub"><a href="/">Home</a> · <a href="/shelf">Shelf</a> · <a href="/plain">Plain list</a></p>
</section>"""
            (esc para)
            lines
            pgLine
            (byStatus Done books |> List.length)
            (byStatus Reading books |> List.length)
            (byStatus ToRead books |> List.length))

let plainView (books: Book list) =
    let body =
        if List.isEmpty books then
            "(empty)"
        else
            plaintextLines books |> String.concat "\r\n" |> esc

    layout
        "Plain"
        (sprintf
            """<section class="panel">
<h1>Copy-friendly list</h1>
<pre class="block">%s</pre>
<p class="sub"><a href="/">Home</a> · <a href="/shelf">Shelf</a> · <a href="/stats">Stats</a></p>
</section>"""
            body)
