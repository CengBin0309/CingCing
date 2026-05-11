open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.HttpOverrides
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open WebSharper.AspNetCore
open ReadShelfWs

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services.Configure<ForwardedHeadersOptions>(fun (o: ForwardedHeadersOptions) ->
        o.ForwardedHeaders <- ForwardedHeaders.XForwardedFor ||| ForwardedHeaders.XForwardedProto
        o.KnownNetworks.Clear()
        o.KnownProxies.Clear())
    |> ignore

    builder.Services.AddWebSharper()
        .AddAuthentication("WebSharper")
        .AddCookie("WebSharper", fun options -> ())
    |> ignore

    let app = builder.Build()

    app.UseForwardedHeaders() |> ignore

    if not (app.Environment.IsDevelopment()) then
        app.UseExceptionHandler("/Error")
            .UseHsts()
        |> ignore

        app.UseHttpsRedirection() |> ignore

    app.UseAuthentication()
        .UseWebSharper(fun ws ->
            // SPA + remoting only: no Sitelet registered; sitelet middleware must be off.
            ws.UseSitelets(false) |> ignore)
        .UseDefaultFiles()
        .UseStaticFiles()
    |> ignore

    app.Run()

    0
