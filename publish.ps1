$outputDir = "$PSScriptRoot\deploy"
dotnet publish .\src\server -c Release -o $outputDir
yarn --cwd .\src\client install --frozen-lockfile
yarn --cwd .\src\client webpack --output-path "$outputDir\wwwroot"
