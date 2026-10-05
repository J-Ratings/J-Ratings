$repoPath = Split-Path (Split-Path $PSScriptRoot -Parent) -Parent
Set-Location -LiteralPath $repoPath
$rScriptPath = 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe'
if (!(Test-Path -LiteralPath $rScriptPath)) { throw "Rscript not found: $rScriptPath" }
$logFolder = Join-Path $repoPath 'EuropeanFootball\pipeline_data\Elo\run_logs'
New-Item -ItemType Directory -Path $logFolder -Force | Out-Null
$runStamp = Get-Date -Format 'yyyyMMdd_HHmmss'
foreach ($stageNumber in @('02','03')) {
    $logPath = Join-Path $logFolder "stage_${stageNumber}_${runStamp}.log"
    Write-Host "Running stage $stageNumber in a fresh R process. Log: $logPath"
    & $rScriptPath --vanilla 'scripts/EuropeanFootball/run_pipeline_stage.R' $stageNumber 2>&1 |
        Tee-Object -FilePath $logPath
    $stageExitCode = $LASTEXITCODE
    if ($stageExitCode -ne 0) {
        Write-Host "Stage $stageNumber failed (exit code $stageExitCode). Stopping. Log: $logPath" -ForegroundColor Red
        exit 1
    }
}
Write-Host '02 and 03 completed successfully.' -ForegroundColor Green
try { [Console]::Beep(800, 400) } catch {}
