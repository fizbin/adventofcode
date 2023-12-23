param([Parameter(Mandatory)] [int]$daynum)
$downloadToPath = "aoc$daynum.in"
$remoteFileLocation = "https://adventofcode.com/2023/day/$daynum/input"

$session = New-Object Microsoft.PowerShell.Commands.WebRequestSession

$cookie = New-Object System.Net.Cookie 

$cookie.Name = "session"
$cookie.Value = (Get-Content session.txt -Raw).Trim()
$cookie.Domain = "adventofcode.com"
$session.Cookies.Add($cookie);

Invoke-WebRequest $remoteFileLocation -UserAgent "https://github.com/fizbin/adventofcode/blob/main/aoc2023/GetInput.ps1 by fizbin@gmail.com" -WebSession $session -TimeoutSec 900 -OutFile $downloadToPath
