@echo off
cd /d "%~dp0"

for /f "tokens=2,*" %%a in (
'reg query "HKLM\Software\R-core\R" /v InstallPath 2^>nul'
) do set RHOME=%%b

if not defined RHOME (
    for /f "tokens=2,*" %%a in (
    'reg query "HKCU\Software\R-core\R" /v InstallPath 2^>nul'
    ) do set RHOME=%%b
)

if not defined RHOME (
    echo No se encontro una instalacion de R.
    pause
    exit /b 1
)

echo Instalando dependencias de ForestMap...
echo.
"%RHOME%\bin\Rscript.exe" dev\install.R

pause
