# Build and run ReadShelfWs for platforms like https://dashboard.render.com/
# Render sets PORT; local fallback 8080.

FROM mcr.microsoft.com/dotnet/sdk:8.0-bookworm-slim AS build
WORKDIR /src

COPY NuGet.config ReadShelfWs.fsproj ./
RUN dotnet restore ReadShelfWs.fsproj

COPY . .
RUN dotnet publish ReadShelfWs.fsproj -c Release -o /app/publish --no-restore

FROM mcr.microsoft.com/dotnet/aspnet:8.0-bookworm-slim AS final
WORKDIR /app
COPY --from=build /app/publish .

ENV ASPNETCORE_ENVIRONMENT=Production
EXPOSE 8080

# Render injects PORT; bind all interfaces for the container network.
ENTRYPOINT ["/bin/sh", "-c", "exec dotnet ReadShelfWs.dll --urls \"http://0.0.0.0:${PORT:-8080}\""]
