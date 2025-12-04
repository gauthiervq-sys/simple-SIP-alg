@echo off
REM Build script for SIP ALG Client Tester (Windows)

echo Building SIP ALG Client Tester...
echo.

REM Check if pkg is installed
where pkg >nul 2>nul
if %errorlevel% neq 0 (
    echo pkg is not installed. Installing...
    npm install -g pkg
)

REM Create output directories
if not exist "public\downloads" mkdir "public\downloads"
if not exist "dist" mkdir "dist"

echo Compiling executables...

echo   - Building for Windows (x64)...
call pkg client-tester.js --targets node18-win-x64 --output public/downloads/sip-alg-tester-win.exe

echo   - Building for Linux (x64)...
call pkg client-tester.js --targets node18-linux-x64 --output public/downloads/sip-alg-tester-linux

echo   - Building for macOS (x64)...
call pkg client-tester.js --targets node18-macos-x64 --output public/downloads/sip-alg-tester-macos

echo.
echo Build complete! Executables are in public\downloads\
echo.
dir /B public\downloads\
echo.
echo To test the client locally, run:
echo   node client-tester.js ^<server-ip^>
echo.
pause
