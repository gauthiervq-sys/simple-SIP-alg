@echo off
REM Build script for SIP ALG Client Tester (Windows)

echo Building SIP ALG Client Tester...
echo.

REM Check if pkg is installed
where pkg >nul 2>nul
if %errorlevel% neq 0 (
    echo pkg is not installed. Installing...
    npm install -g pkg
    if %errorlevel% neq 0 (
        npm install --save-dev pkg
    )
    
    REM Check again
    where pkg >nul 2>nul
    if %errorlevel% neq 0 (
        npx pkg --version >nul 2>nul
        if %errorlevel% neq 0 (
            echo.
            echo WARNING: Could not install 'pkg'.
            echo Creating placeholder scripts instead of full executables.
            echo.
            node create-placeholders.js
            goto :end
        )
    )
)

REM Create output directories
if not exist "public\downloads" mkdir "public\downloads"
if not exist "dist" mkdir "dist"

echo Compiling executables...
echo.
echo Note: Executables are pre-configured to connect to 193.105.36.15
echo       Users can run them without any command-line arguments.
echo.

REM Determine pkg command
where pkg >nul 2>nul
if %errorlevel% equ 0 (
    set PKG_CMD=pkg
) else (
    set PKG_CMD=npx pkg
)

echo   - Building for Windows (x64)...
call %PKG_CMD% client-tester.js --targets node18-win-x64 --output public/downloads/sip-alg-tester-win.exe
if %errorlevel% neq 0 (
    echo     Failed to build Windows executable. Creating placeholder...
    node create-placeholders.js
    goto :end
)

echo   - Building for Linux (x64)...
call %PKG_CMD% client-tester.js --targets node18-linux-x64 --output public/downloads/sip-alg-tester-linux
if %errorlevel% neq 0 (
    echo     Failed to build Linux executable. Creating placeholder...
    node create-placeholders.js
    goto :end
)

echo   - Building for macOS (x64)...
call %PKG_CMD% client-tester.js --targets node18-macos-x64 --output public/downloads/sip-alg-tester-macos
if %errorlevel% neq 0 (
    echo     Failed to build macOS executable. Creating placeholder...
    node create-placeholders.js
    goto :end
)

echo.
echo Build complete! Executables are in public\downloads\
echo.
dir /B public\downloads\
echo.
echo To test the client locally, run:
echo   node client-tester.js          (connects to 193.105.36.15 by default)
echo   node client-tester.js ^<ip^>     (to test a different server)
echo.

:end
pause
