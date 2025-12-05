#!/usr/bin/env node

/**
 * Validation script to check if client executables exist
 * Run before starting the server to ensure downloads will work
 */

const fs = require('fs');
const path = require('path');

// ANSI colors
const colors = {
    reset: '\x1b[0m',
    red: '\x1b[31m',
    green: '\x1b[32m',
    yellow: '\x1b[33m',
    cyan: '\x1b[36m',
    bold: '\x1b[1m'
};

const downloadsDir = path.join(__dirname, 'public', 'downloads');

const requiredFiles = [
    'sip-alg-tester-win.exe',
    'sip-alg-tester-linux',
    'sip-alg-tester-macos'
];

console.log(`${colors.bold}${colors.cyan}SIP ALG Server - Pre-Start Validation${colors.reset}\n`);

let allFilesExist = true;
let totalSize = 0;

console.log('Checking for client executables in public/downloads/...\n');

requiredFiles.forEach(file => {
    const filePath = path.join(downloadsDir, file);
    const exists = fs.existsSync(filePath);
    
    if (exists) {
        const stats = fs.statSync(filePath);
        const sizeMB = (stats.size / (1024 * 1024)).toFixed(2);
        totalSize += stats.size;
        console.log(`  ${colors.green}✓${colors.reset} ${file} (${sizeMB} MB)`);
    } else {
        console.log(`  ${colors.red}✗${colors.reset} ${file} ${colors.red}MISSING${colors.reset}`);
        allFilesExist = false;
    }
});

console.log('');

if (allFilesExist) {
    const totalSizeMB = (totalSize / (1024 * 1024)).toFixed(2);
    console.log(`${colors.green}${colors.bold}✓ All client executables found!${colors.reset} (Total: ${totalSizeMB} MB)`);
    console.log(`${colors.green}Users will be able to download test tools from the web interface.${colors.reset}\n`);
    process.exit(0);
} else {
    console.log(`${colors.red}${colors.bold}✗ Some client executables are missing!${colors.reset}`);
    console.log(`${colors.yellow}Users will get "File Not Available" errors when downloading.${colors.reset}\n`);
    console.log(`${colors.cyan}To fix this, build the client executables:${colors.reset}`);
    console.log(`  ${colors.cyan}./build-client.sh${colors.reset}  (Linux/macOS)`);
    console.log(`  ${colors.cyan}build-client.bat${colors.reset}   (Windows)`);
    console.log(`  ${colors.cyan}npm run build:client${colors.reset}  (using npm)\n`);
    console.log(`${colors.yellow}Note: Building requires 'pkg' to be installed.${colors.reset}`);
    console.log(`  ${colors.cyan}npm install -g pkg${colors.reset}\n`);
    process.exit(1);
}
