$daynum = 9
$downloadToPath = "aoc$daynum.in"
$remoteFileLocation = "https://adventofcode.com/2022/day/$daynum/input"

$session = New-Object Microsoft.PowerShell.Commands.WebRequestSession

$cookie = New-Object System.Net.Cookie 

$cookie.Name = "session"
$cookie.Value = (Get-Content session.txt -Raw).Trim()
$cookie.Domain = "adventofcode.com"
$session.Cookies.Add($cookie);

Invoke-WebRequest $remoteFileLocation -WebSession $session -TimeoutSec 900 -OutFile $downloadToPath
