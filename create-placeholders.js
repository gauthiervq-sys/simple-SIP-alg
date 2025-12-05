#!/usr/bin/env node

/**
 * Create placeholder executables when pkg is not available
 * These are minimal shell/batch scripts that guide users
 */

const fs = require('fs');
const path = require('path');

const downloadsDir = path.join(__dirname, 'public', 'downloads');

// Ensure directory exists
if (!fs.existsSync(downloadsDir)) {
    fs.mkdirSync(downloadsDir, { recursive: true });
}

// Windows batch file that downloads and runs the real client-tester
const winBatch = `@echo off
REM SIP ALG Tester for Windows
REM This is a placeholder. Building real executables requires 'pkg' npm package.

echo ================================================================================
echo SIP ALG Client Tester for Windows
echo Server: 193.105.36.15 (pre-configured)
echo ================================================================================
echo.
echo ERROR: This is a placeholder file.
echo.
echo To use this tool, you need Node.js installed on your system.
echo Download Node.js from: https://nodejs.org/
echo.
echo Once installed, download client-tester.js from the server and run:
echo   node client-tester.js
echo.
echo The test will automatically connect to 193.105.36.15
echo.
echo ================================================================================
echo.
echo For server administrators:
echo Build real executables by running: npm run build:client
echo This requires the 'pkg' package: npm install -g pkg
echo.
pause
`;

// Linux/macOS shell script
const linuxScript = `#!/bin/bash

# SIP ALG Tester for Linux
# This is a placeholder. Building real executables requires 'pkg' npm package.

cat << 'EOF'
================================================================================
SIP ALG Client Tester for Linux
Server: 193.105.36.15 (pre-configured)
================================================================================

ERROR: This is a placeholder file.

To use this tool, you need Node.js installed on your system.
Download Node.js from: https://nodejs.org/

Once installed, you can download and run client-tester.js directly:
  node client-tester.js

The test will automatically connect to 193.105.36.15

================================================================================

For server administrators:
Build real executables by running: ./build-client.sh
This requires the 'pkg' package: npm install -g pkg

EOF

read -p "Press Enter to exit..."
`;

const macScript = linuxScript; // Same for macOS

console.log('Creating placeholder executables...\n');

// Create Windows placeholder
const winPath = path.join(downloadsDir, 'sip-alg-tester-win.exe');
fs.writeFileSync(winPath, winBatch, 'utf8');
console.log('  ✓ Created placeholder: sip-alg-tester-win.exe (batch script)');

// Create Linux placeholder
const linuxPath = path.join(downloadsDir, 'sip-alg-tester-linux');
fs.writeFileSync(linuxPath, linuxScript, 'utf8');
fs.chmodSync(linuxPath, 0o755);
console.log('  ✓ Created placeholder: sip-alg-tester-linux (shell script)');

// Create macOS placeholder
const macPath = path.join(downloadsDir, 'sip-alg-tester-macos');
fs.writeFileSync(macPath, macScript, 'utf8');
fs.chmodSync(macPath, 0o755);
console.log('  ✓ Created placeholder: sip-alg-tester-macos (shell script)');

console.log('\n✓ Placeholder executables created!');
console.log('\nNote: These are minimal scripts that guide users to install Node.js.');
console.log('For production deployment, build real executables with: ./build-client.sh\n');
