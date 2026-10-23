# Recovery only: never stages, commits, pushes, or invokes Git.
$repoRoot = Split-Path -Parent $PSScriptRoot
$lockPath = Join-Path $repoRoot '.git\index.lock'
try {
    if (-not (Test-Path -LiteralPath $lockPath)) {
        Write-Host 'No Git index lock exists. Nothing to repair.'
        exit 0
    }
    if (Get-Process -Name git -ErrorAction SilentlyContinue) {
        throw 'Git is running. Close Codex and wait for Git operations to finish, then retry. Nothing changed.'
    }
    $before = Get-Item -LiteralPath $lockPath -ErrorAction Stop
    if ($before.LastWriteTime -gt (Get-Date).AddMinutes(-2)) {
        throw 'The lock is recent. Wait at least two minutes and retry. Nothing changed.'
    }
    # Refuse an open lock, then recheck processes and metadata before preserving
    # it. Close Codex first to prevent background Git operations restarting.
    $handle = [System.IO.File]::Open($lockPath, [System.IO.FileMode]::Open,
        [System.IO.FileAccess]::ReadWrite, [System.IO.FileShare]::None)
    try {
        $confirmedLength = $handle.Length
    } finally { $handle.Dispose() }
    $after = Get-Item -LiteralPath $lockPath -ErrorAction Stop
    if ((Get-Process -Name git -ErrorAction SilentlyContinue) -or
        $before.LastWriteTime -ne $after.LastWriteTime -or
        $before.Length -ne $after.Length -or $confirmedLength -ne $after.Length) {
        throw 'Git activity or a changing lock detected. Nothing changed.'
    }
    $backup = $lockPath + '.stale-' + (Get-Date -Format 'yyyyMMdd-HHmmss-fff')
    Move-Item -LiteralPath $lockPath -Destination $backup -ErrorAction Stop
    Write-Host 'Stale lock preserved as:'
    Write-Host $backup
    Write-Host 'Retry your commit in GitHub Desktop. No files, commits or branches were changed.' -ForegroundColor Green
} catch {
    Write-Host $_.Exception.Message -ForegroundColor Red
    exit 1
}
