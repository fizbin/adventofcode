param([Parameter(Mandatory)] [int]$daynum)
$downloadToPath = "aoc$daynum.in"
$remoteFileLocation = "https://adventofcode.com/2025/day/$daynum/input"

$session = New-Object Microsoft.PowerShell.Commands.WebRequestSession

$cookie = New-Object System.Net.Cookie 

$cookie.Name = "session"
$cookie.Value = (Get-Content session.txt -Raw).Trim()
$cookie.Domain = "adventofcode.com"
$session.Cookies.Add($cookie);

# Needed because it's invalid (RFC9110) to include a URL in a User-Agent string
$PSDefaultParameterValues['Invoke-WebRequest:SkipHeaderValidation'] = $true
Invoke-WebRequest $remoteFileLocation -UserAgent "https://github.com/fizbin/adventofcode by fizbin-at-gmail.com" -WebSession $session -TimeoutSec 900 -OutFile $downloadToPath
