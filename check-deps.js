#!/usr/bin/env node
const fs = require('fs');
const { execSync } = require('child_process');
const path = require('path');

const nodeModulesPath = path.join(__dirname, 'node_modules');
const packageJsonPath = path.join(__dirname, 'package.json');

// Check if node_modules exists and has content
function needsInstall() {
    if (!fs.existsSync(nodeModulesPath)) {
        return true;
    }
    
    // Check if key dependencies exist
    try {
        const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
        if (packageJson.dependencies) {
            for (const dep of Object.keys(packageJson.dependencies)) {
                const depPath = path.join(nodeModulesPath, dep);
                if (!fs.existsSync(depPath)) {
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
