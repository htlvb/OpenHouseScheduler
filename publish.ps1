Push-Location $PSScriptRoot

dotnet publish .\src\server -c Release -o .\deploy
yarn --cwd .\src\client install --frozen-lockfile
yarn --cwd .\src\client webpack --output-path "$pwd\deploy\wwwroot"

Pop-Location
