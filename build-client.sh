#!/bin/bash

# Build script for SIP ALG Client Tester
# This script compiles the client-tester.js into standalone executables for different platforms

echo "Building SIP ALG Client Tester..."
echo ""

# Check if pkg is installed
if ! command -v pkg &> /dev/null; then
    echo "pkg is not installed. Attempting to install..."
    npm install -g pkg || npm install --save-dev pkg
    
    # Check again if pkg is now available
    if ! command -v pkg &> /dev/null && ! npx pkg --version &> /dev/null; then
        echo ""
        echo "WARNING: Could not install 'pkg'."
        echo "Creating placeholder scripts instead of full executables."
        echo ""
        node create-placeholders.js
        exit 0
    fi
fi

# Create output directory
mkdir -p public/downloads
mkdir -p dist

echo "Compiling executables..."
echo ""
echo "Note: Executables are pre-configured to connect to 193.105.36.15"
echo "      Users can run them without any command-line arguments."
echo ""

# Determine if we should use global pkg or local npx pkg
PKG_CMD="pkg"
if ! command -v pkg &> /dev/null; then
    PKG_CMD="npx pkg"
fi

# Build for each platform
echo "  - Building for Windows (x64)..."
$PKG_CMD client-tester.js --targets node18-win-x64 --output public/downloads/sip-alg-tester-win.exe

if [ $? -ne 0 ]; then
    echo "    Failed to build Windows executable. Creating placeholder..."
    node create-placeholders.js
    exit 1
fi

echo "  - Building for Linux (x64)..."
$PKG_CMD client-tester.js --targets node18-linux-x64 --output public/downloads/sip-alg-tester-linux

if [ $? -ne 0 ]; then
    echo "    Failed to build Linux executable. Creating placeholder..."
    node create-placeholders.js
    exit 1
fi

echo "  - Building for macOS (x64)..."
$PKG_CMD client-tester.js --targets node18-macos-x64 --output public/downloads/sip-alg-tester-macos

if [ $? -ne 0 ]; then
    echo "    Failed to build macOS executable. Creating placeholder..."
    node create-placeholders.js
    exit 1
fi

echo ""
echo "Build complete! Executables are in public/downloads/"
echo ""
ls -lh public/downloads/
echo ""
echo "To test the client locally, run:"
echo "  node client-tester.js          (connects to 193.105.36.15 by default)"
echo "  node client-tester.js <ip>     (to test a different server)"
echo ""
