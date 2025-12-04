#!/usr/bin/env node
const fs = require('fs');
const { execSync } = require('child_process');
const path = require('path');

const nodeModulesPath = path.join(__dirname, 'node_modules');
const packageJsonPath = path.join(__dirname, 'package.json');

// Check if dependencies need to be installed or are incomplete
function needsInstall() {
    if (!fs.existsSync(nodeModulesPath)) {
        return true;
    }
    
    // Validate that key dependencies can be loaded
    try {
        const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
        if (packageJson.dependencies) {
            for (const dep of Object.keys(packageJson.dependencies)) {
                try {
                    // Use require.resolve to verify the package can be loaded
                    require.resolve(dep, { paths: [__dirname] });
                } catch (e) {
                    // Dependency missing or corrupted
                    return true;
                }
            }
        }
    } catch (error) {
        console.error('Error reading package.json:', error.message);
        console.error('Please ensure package.json exists and is valid JSON.');
        process.exit(1);
    }
    
    return false;
}

if (needsInstall()) {
    console.log('Dependencies not found or incomplete. Installing...');
    try {
        // Use execSync with shell:true to let Node.js choose appropriate shell
        // This is safe because we're executing in the project directory with a fixed command
        execSync('npm install', { 
            stdio: 'inherit', 
            cwd: __dirname,
            shell: true
        });
        console.log('Dependencies installed successfully.');
    } catch (error) {
        console.error('Failed to install dependencies:', error.message);
        console.error('Please ensure you have npm installed and network connectivity.');
        console.error('You can try running "npm install" manually.');
        process.exit(1);
    }
}
