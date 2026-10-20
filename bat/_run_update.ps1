param(
    [Parameter(Mandatory=$true)][string]$Mode,
    [switch]$ValidateOnly
)

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location -LiteralPath $repoRoot
$env:J_RATINGS_REPO = $repoRoot
$env:GITHUB_WORKSPACE = $repoRoot
$goStages = @('05_build_games.R','06_build_player_map.R','07_calculate_live_elo.R',
    '08_build_combined_ratings.R','09_build_combined_history.R','02_write_json.R') |
    ForEach-Object { 'scripts/go/' + $_ }
$plans = @{
    'go-quick' = @('scripts/go/run_daily_tournament_results.R')
    'go-weekly' = @('scripts/go/04_scrape_goratings.R') + $goStages + @('scripts/go/build_tournament_links.R')
    'go-full' = @('scripts/go/04_scrape_goratings.R') + $goStages + @('scripts/go/build_tournament_links.R')
    'go-recalculate' = $goStages + @('scripts/go/build_tournament_links.R')
    'club' = @('scripts/EuropeanFootball/00_download_openfootball_current.R',
        'scripts/EuropeanFootball/01_parse_openfootball.R',
        'scripts/EuropeanFootball/02_calculate_elo.R','scripts/EuropeanFootball/03_write_json.R')
    'club-recalculate' = @('scripts/EuropeanFootball/02_calculate_elo.R','scripts/EuropeanFootball/03_write_json.R')
    'international' = @('scripts/world_football/00_download_data.R','scripts/world_football/01_combine_results.R',
        'scripts/world_football/02_calculate_elo.R','scripts/world_football/03_write_json.R')
    'international-recalculate' = @('scripts/world_football/02_calculate_elo.R','scripts/world_football/03_write_json.R')
    'snooker' = @('scripts/snooker/00_refresh_lookups.R','scripts/snooker/01_update_matches.R',
        'scripts/snooker/02_calculate_elo.R','scripts/snooker/03_write_json.R')
    'snooker-recalculate' = @('scripts/snooker/02_calculate_elo.R','scripts/snooker/03_write_json.R')
    'rugby' = @('scripts/world_rugby/00_api_merge.R','scripts/world_rugby/01_calculation.R','scripts/world_rugby/02_write_json.R')
    'rugby-recalculate' = @('scripts/world_rugby/01_calculation.R','scripts/world_rugby/02_write_json.R')
}
$exitCode = 1
$mutex = New-Object System.Threading.Mutex($false, 'Local\JRatingsLocalDataUpdate')
$ownsMutex = $false
try {
    if (-not $plans.ContainsKey($Mode)) { throw "Unknown update mode: $Mode" }
    foreach ($stage in $plans[$Mode]) {
        if (-not (Test-Path -LiteralPath $stage)) { throw "Missing script: $stage" }
    }
    $rscript = $env:J_RATINGS_RSCRIPT
    if (-not $rscript) {
        $command = Get-Command Rscript.exe -ErrorAction SilentlyContinue
        if ($command) { $rscript = $command.Source }
    }
    if (-not $rscript) {
        $candidates = Get-ChildItem -Path "$env:ProgramFiles\R\R-*\bin\x64\Rscript.exe", "$env:ProgramFiles\R\R-*\bin\Rscript.exe" -ErrorAction SilentlyContinue |
            Sort-Object FullName -Descending
        if ($candidates) { $rscript = $candidates[0].FullName }
    }
    if (-not $rscript -or -not (Test-Path -LiteralPath $rscript)) {
        throw 'Rscript.exe not found. Set J_RATINGS_RSCRIPT to its full path.'
    }
    if ($ValidateOnly) {
        Write-Output "Validated $Mode using $rscript"
        exit 0
    }
    try { $ownsMutex = $mutex.WaitOne(0) } catch [System.Threading.AbandonedMutexException] { $ownsMutex = $true }
    if (-not $ownsMutex) { throw 'Another BAT update is running. Finish it before starting another.' }
    # These settings apply only to this launcher and its child processes.
    if ($Mode -like 'go-*') {
        Remove-Item Env:GO_DAILY_DATE, Env:GO_DAILY_TOURNAMENT_LINKS, Env:GO_DAILY_FUNCTIONS_ONLY -ErrorAction SilentlyContinue
        $env:GO_TOURNAMENT_LINK_OUTPUT = 'Go/data/tournaments/catalogue'
        if ($Mode -eq 'go-quick') { $env:GO_DAILY_APPLY = 'true' }
        if ($Mode -eq 'go-weekly') { $env:GORATINGS_SCRAPE_MODE = 'weekly' }
        if ($Mode -eq 'go-full') { $env:GORATINGS_SCRAPE_MODE = 'full' }
    }
    if ($Mode -eq 'go-full') {
        Write-Host 'Full GoRatings refresh: checks every listed profile and can take a long time.'
        if ((Read-Host 'Type FULL to continue') -cne 'FULL') { throw 'Full refresh cancelled.' }
    }
    $logDir = Join-Path $PSScriptRoot 'logs'
    New-Item -ItemType Directory -Path $logDir -Force | Out-Null
    $logFile = Join-Path $logDir ($Mode + '_' + (Get-Date -Format 'yyyyMMdd_HHmmss') + '.log')
    Write-Host "Mode: $Mode"
    Write-Host "Log: $logFile"
    Write-Host 'Updates local data only. No Git commands, publication or workflow changes.'
    foreach ($stage in $plans[$Mode]) {
        "`nRunning $stage" | Tee-Object -FilePath $logFile -Append
        & $rscript 'bat/_run_stage.R' $stage 2>&1 | Tee-Object -FilePath $logFile -Append
        if ($LASTEXITCODE -ne 0) { throw "Stage failed: $stage. See $logFile. Later stages were not run." }
    }
    Write-Host "`nUpdate complete. Log: $logFile" -ForegroundColor Green
    $exitCode = 0
} catch {
    Write-Host "`nUPDATE STOPPED: $($_.Exception.Message)" -ForegroundColor Red
} finally {
    if ($ownsMutex) { $mutex.ReleaseMutex() }
    $mutex.Dispose()
}
exit $exitCode
