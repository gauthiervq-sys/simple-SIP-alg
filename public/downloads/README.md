# SIP ALG Client Test Tools

This directory contains the standalone executable files for the SIP ALG Client Tester.

## Pre-configured Server

**Important:** All executables are pre-configured to connect to server **193.105.36.15** by default.
Users can simply run the executable without any command-line arguments!

## Building the Executables

To build the executables, run the build script from the project root:

### On Linux/macOS:
```bash
./build-client.sh
```

### On Windows:
```batch
build-client.bat
```

### Using npm:
```bash
npm run build:client
```

## Files

After building, this directory will contain:
- `sip-alg-tester-win.exe` - Windows executable (x64)
- `sip-alg-tester-linux` - Linux executable (x64)
- `sip-alg-tester-macos` - macOS executable (x64)

These files are excluded from git due to their large size. They should be built on the server or as part of the deployment process.

## Usage

**Windows:**
```cmd
sip-alg-tester-win.exe
```

**Linux/macOS:**
```bash
chmod +x sip-alg-tester-linux
./sip-alg-tester-linux
```

To test a different server, provide the IP as an argument:
```bash
sip-alg-tester-win.exe 192.168.1.100
./sip-alg-tester-linux 192.168.1.100
```

## Deployment

When deploying the server, make sure to:
1. Install dependencies: `npm install`
2. Build the client executables: `./build-client.sh` or `npm run build:client`
3. Start the server: `npm start`

The executables will be served from the web interface at `/downloads/sip-alg-tester-*`
