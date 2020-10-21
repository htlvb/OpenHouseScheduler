$serverDir = "$PSScriptRoot\src\server"
$clientDir = "$PSScriptRoot\src\client"
yarn --cwd $clientDir install --frozen-lockfile

wt `
    new-tab --title Server --startingDirectory $serverDir `-`- dotnet watch run --urls=http://+:8000`; `
    new-tab --title Client --startingDirectory $clientDir `-`- yarn.cmd webpack-dev-server
