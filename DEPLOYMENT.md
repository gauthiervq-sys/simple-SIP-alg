# Deployment Guide for SIP ALG Test Server

This guide walks you through deploying the SIP ALG test server, ensuring all client executables are built and available for download.

## Prerequisites

- Node.js v14 or higher
- npm (Node Package Manager)
- Ubuntu 24.04 (recommended, but works on other Linux distributions)

## Step-by-Step Deployment

### 1. Clone the Repository

```bash
git clone https://github.com/gauthiervq-sys/simple-SIP-alg.git
cd simple-SIP-alg
```

### 2. Install Dependencies

```bash
npm install
```

If you encounter npm registry issues, you can try:
```bash
npm install --registry=https://registry.npmjs.org/
```

Or use yarn as an alternative:
```bash
yarn install
```

### 3. **CRITICAL: Build Client Executables**

This step is **REQUIRED** before starting the server. Without it, users will get "File Not Available" errors when downloading test tools.

```bash
./build-client.sh
```

Or on Windows:
```bash
build-client.bat
```

Or using npm:
```bash
npm run build:client
```

This will create three executables in `public/downloads/`:
- `sip-alg-tester-win.exe` (Windows)
- `sip-alg-tester-linux` (Linux)
- `sip-alg-tester-macos` (macOS)

**Note:** All executables are pre-configured to connect to **193.105.36.15** by default. Users don't need to provide command-line arguments.

### 4. Verify Executables Were Built

```bash
ls -lh public/downloads/
```

You should see three executable files. If they don't exist, users will get download errors.

### 5. Configure Firewall

Ensure the following ports are open:

```bash
sudo ufw allow 3000/tcp
sudo ufw allow 5060/tcp
sudo ufw allow 5060/udp
sudo ufw allow 5062/tcp
sudo ufw allow 5062/udp
```

### 6. Start the Server

For testing:
```bash
npm start
```

For production (with PM2):
```bash
npm install -g pm2
pm2 start server.js --name simple-sip-alg
pm2 save
pm2 startup systemd
```

For production (with systemd):
```bash
sudo cp deploy/simple-sip-alg.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable simple-sip-alg
sudo systemctl start simple-sip-alg
```

### 7. Verify Server is Running

```bash
curl http://localhost:3000
```

Or visit in browser:
```
http://193.105.36.15:3000
```

### 8. Test Downloads

Navigate to the web interface and click on the "SIP ALG Check" tab. Verify that all three download buttons work and files download successfully.

## Troubleshooting

### "File Not Available" Download Errors

This means the client executables weren't built. Run:
```bash
./build-client.sh
```

Then restart the server.

### pkg Not Found

If `pkg` isn't installed:
```bash
npm install -g pkg
```

Or install it locally:
```bash
npm install --save-dev pkg
npx pkg client-tester.js --targets node18-win-x64 --output public/downloads/sip-alg-tester-win.exe
npx pkg client-tester.js --targets node18-linux-x64 --output public/downloads/sip-alg-tester-linux
npx pkg client-tester.js --targets node18-macos-x64 --output public/downloads/sip-alg-tester-macos
```

### Port 5060 Already in Use

Asterisk or another SIP service may be using port 5060. The server will continue to work on port 5062, but you may want to stop the conflicting service:

```bash
sudo systemctl stop asterisk
```

### Rebuilding After Updates

If you update the `client-tester.js` file, you must rebuild the executables:

```bash
./build-client.sh
```

Then restart the server for users to download the updated versions.

## Important Notes

- **All client executables connect to 193.105.36.15 by default** - Users don't need to specify the server IP
- **Executables must be rebuilt** after any changes to `client-tester.js`
- **Executables are excluded from git** (in .gitignore) due to file size - they must be built on each deployment
- **Users run executables without PowerShell/CMD** - Just double-click on Windows, or `chmod +x && ./executable` on Linux/macOS

## Client Usage (For End Users)

Users simply need to:

**Windows:**
1. Download `sip-alg-tester-win.exe`
2. Double-click to run (no arguments needed)

**Linux/macOS:**
1. Download the appropriate executable
2. Run: `chmod +x sip-alg-tester-linux && ./sip-alg-tester-linux`

The tool automatically connects to 193.105.36.15 and runs the test.
