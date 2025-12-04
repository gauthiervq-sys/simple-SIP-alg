#!/usr/bin/env node
const fs = require('fs');
const { execSync } = require('child_process');
const path = require('path');

const nodeModulesPath = path.join(__dirname, 'node_modules');

if (!fs.existsSync(nodeModulesPath)) {
    console.log('Dependencies not found. Installing...');
    try {
        execSync('npm install', { stdio: 'inherit', cwd: __dirname });
        console.log('Dependencies installed successfully.');
    } catch (error) {
        console.error('Failed to install dependencies:', error.message);
        process.exit(1);
    }
}
