#!/bin/bash

# Build script for SIP ALG Client Tester
# This script compiles the client-tester.js into standalone executables for different platforms

echo "Building SIP ALG Client Tester..."
echo ""

# Check if pkg is installed
if ! command -v pkg &> /dev/null; then
    echo "pkg is not installed. Installing..."
    npm install -g pkg
fi

# Create output directory
mkdir -p public/downloads
mkdir -p dist

echo "Compiling executables..."

# Build for each platform
echo "  - Building for Windows (x64)..."
pkg client-tester.js --targets node18-win-x64 --output public/downloads/sip-alg-tester-win.exe

echo "  - Building for Linux (x64)..."
pkg client-tester.js --targets node18-linux-x64 --output public/downloads/sip-alg-tester-linux

echo "  - Building for macOS (x64)..."
pkg client-tester.js --targets node18-macos-x64 --output public/downloads/sip-alg-tester-macos

echo ""
echo "Build complete! Executables are in public/downloads/"
echo ""
ls -lh public/downloads/
echo ""
echo "To test the client locally, run:"
echo "  node client-tester.js <server-ip>"
echo ""
