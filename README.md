# ReadShelfWs

WebSharper + F# SPA: reading list with sort, add/edit/remove, status cycle, `localStorage`, and one remote call `ServerUtc` in `Rpc.fs`. Host entry `Startup.fs`, markup `wwwroot/index.html`.

Run:

```bash
dotnet build
dotnet run
```

Default URL `http://127.0.0.1:5012` (see `Properties/launchSettings.json`). Requires .NET 8. Use `NuGet.config` in this folder if restore fails.

Screenshot: `screenshots/home.png`.

Source layout: `ShelfDomain.fs` (model and `localStorage` encoding), `Client.fs` (UI), `Rpc.fs`, `Startup.fs`. Docker: `Dockerfile` in this folder.
