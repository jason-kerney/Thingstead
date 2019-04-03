@echo off
cls

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*
dotnet run -p .\ThingStead.Temp.Runner\ThingStead.Temp.Runner.fsproj