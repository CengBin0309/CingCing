namespace ReadShelfWs

open WebSharper

/// Server-side RPC (no `[<JavaScript>]` here). Client calls compile to remoting stubs.
module ServerShelfRpc =
    [<Remote>]
    let ServerUtc () =
        async { return System.DateTime.UtcNow.ToString("o") }
