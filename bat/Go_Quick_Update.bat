@echo off
setlocal
rem Enable tournament-source refresh for this launcher only, at the user's request.
set "GO_K2SS_PERMISSION_GRANTED=true"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0_run_update.ps1" -Mode go-quick
set "RUN_EXIT=%ERRORLEVEL%"
echo.
pause
exit /b %RUN_EXIT%
