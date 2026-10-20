@echo off
setlocal
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0_run_update.ps1" -Mode snooker
set "RUN_EXIT=%ERRORLEVEL%"
echo.
pause
exit /b %RUN_EXIT%
