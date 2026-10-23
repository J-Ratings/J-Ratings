@echo off
setlocal
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0_repair_git_lock.ps1"
set "RUN_EXIT=%ERRORLEVEL%"
echo.
pause
exit /b %RUN_EXIT%
