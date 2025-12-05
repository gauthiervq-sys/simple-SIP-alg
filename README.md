# SIP ALG & Quality Check Tool

A Node.js-based tool for testing SIP (Session Initiation Protocol) connectivity and quality on Ubuntu 24.04 servers. This tool helps diagnose SIP ALG (Application Layer Gateway) issues and provides network quality metrics.

## Features

- **SIP ALG Detection**: Client-side test that detects SIP ALG modifications from your PC to the backend
  - **Client-Side Testing**: Download standalone executable to test from your personal computer
  - Tests both UDP and TCP transports on ports 5060 and 5062 simultaneously
  - Built-in SIP mirror servers that echo back received requests
  - Compares original vs mirrored SIP INVITE requests to detect modifications
  - Cross-platform support: Windows, Linux, macOS
- **SIP Protocol Testing**: Tests SIP REGISTER, SUBSCRIBE, NOTIFY, and INVITE messages
- **Multi-Port Support**: 
  - Web interface on port 3000
  - SIP UDP/TCP mirror servers on ports 5060 and 5062
- **Quality Metrics**: 2-minute connectivity test measuring:
  - Latency (average round-trip time)
  - Jitter (variance in latency)
  - Packet Loss percentage
- **Asterisk Compatibility**: Graceful handling if Asterisk is already using SIP ports
- **Automatic Backend**: Runs as a systemd service or with PM2

## Prerequisites

- Node.js (v14 or higher)
- npm (Node Package Manager)
- Ubuntu 24.04 (recommended, but works on other Linux distributions)

## Installation

### Server Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd simple-SIP-alg
```

2. Install dependencies:
```bash
npm install
```

3. **Build client test tools (REQUIRED for downloads to work):**
```bash
./build-client.sh
# or on Windows: build-client.bat
# or using npm: npm run build:client
```

**Important:** The client test tools must be built before starting the server, otherwise users will get "File Not Available" errors when trying to download them. The executables are pre-configured to connect to **193.105.36.15** automatically.

## Usage

### Quick Start (Manual)

1. Start the server:
```bash
npm start
```

Or if you've already run `npm install`:
```bash
node server.js
```

The server will start:
- Web server on port 3000
- SIP mirror servers on ports 5060 and 5062 (UDP and TCP)

**Note:** If Asterisk is running on port 5060, the server will log a warning but continue running on ports 3000 and 5062.

2. Open your web browser and navigate to:
```
http://localhost:3000
```

Or use your server's public IP:
```
http://193.105.36.15:3000
```

### SIP ALG Testing

#### Important: Client-Side Testing Required

To accurately detect SIP ALG modifications, the test **must be run from your local computer** (not from the server). This ensures that SIP packets pass through your router where ALG modifications occur.

#### Option 1: Using the Web Interface (Recommended)

1. Navigate to the web interface at `http://<server-ip>:3000`
2. Click on the "SIP ALG Check" tab
3. Download the appropriate client test tool for your operating system:
   - Windows: `sip-alg-tester-win.exe`
   - Linux: `sip-alg-tester-linux`
   - macOS: `sip-alg-tester-macos`
4. Run the downloaded tool from your PC (no arguments needed - connects to 193.105.36.15 by default):

**Windows:**
```cmd
sip-alg-tester-win.exe
```

**Linux/macOS:**
```bash
chmod +x sip-alg-tester-linux  # or sip-alg-tester-macos
./sip-alg-tester-linux
```

To test a different server, you can optionally provide the server IP:
```bash
sip-alg-tester-win.exe <server-ip>
./sip-alg-tester-linux <server-ip>
```

#### Option 2: Using Node.js Directly

If you have Node.js installed on your client PC:

```bash
node client-tester.js [server-ip] [ports]
```

Examples:
```bash
# Connect to default server (193.105.36.15)
node client-tester.js

# Connect to specific server
node client-tester.js 192.168.1.100

# Connect to specific server with custom ports
node client-tester.js 193.105.36.15 5060,5062
```

### Automatic Backend Runtime (Production)

For production deployments on Ubuntu 24.04, you can set up the backend to run automatically at boot using systemd:

#### Option 1: Systemd Service (Recommended for Ubuntu)

1. Copy your application to `/opt/simple-SIP-alg`:
```bash
sudo mkdir -p /opt/simple-SIP-alg
sudo cp -r /path/to/simple-SIP-alg/* /opt/simple-SIP-alg/
cd /opt/simple-SIP-alg
sudo npm install
sudo ./build-client.sh  # Build client test tools
```

2. Copy the systemd service file:
```bash
sudo cp /opt/simple-SIP-alg/deploy/simple-sip-alg.service /etc/systemd/system/
```

3. Edit the service file to customize if needed (adjust paths, user, environment variables):
```bash
sudo nano /etc/systemd/system/simple-sip-alg.service
```

4. Reload systemd and enable the service:
```bash
sudo systemctl daemon-reload
sudo systemctl enable simple-sip-alg
```

5. Start the service:
```bash
sudo systemctl start simple-sip-alg
```

6. Check the service status:
```bash
sudo systemctl status simple-sip-alg
```

7. View logs:
```bash
sudo journalctl -u simple-sip-alg -f
```

To stop the service:
```bash
sudo systemctl stop simple-sip-alg
```

To restart the service:
```bash
sudo systemctl restart simple-sip-alg
```

#### Option 2: PM2 Process Manager

Alternatively, you can use PM2 for process management:

1. Install PM2 globally:
```bash
sudo npm install -g pm2
```

2. Start the application with PM2:
```bash
npm run start:pm2
```

Or directly:
```bash
pm2 start server.js --name simple-sip-alg
```

3. Set PM2 to start on boot:
```bash
pm2 startup systemd
pm2 save
```

4. Check PM2 status:
```bash
pm2 status
pm2 logs simple-sip-alg
```

### Configuration

The following environment variables can be used to configure the application:

- `SIPALG_HOST_IP`: Your WAN/public IP address (default: `193.105.36.15`)

These can be set in:
- The systemd service file (`/etc/systemd/system/simple-sip-alg.service`)
- Your shell environment before starting the server
- Or as command line arguments: `SIPALG_HOST_IP=1.2.3.4 node server.js`

## Firewall Configuration

Ensure the following ports are open in your firewall:

```bash
sudo ufw allow 3000/tcp
sudo ufw allow 5060/tcp
sudo ufw allow 5060/udp
sudo ufw allow 5062/tcp
sudo ufw allow 5062/udp
```

## Port Information

- **Port 3000**: HTTP server and WebSocket for web interface
- **Port 5060**: UDP and TCP SIP mirror servers (standard SIP port, may conflict with Asterisk)
- **Port 5062**: UDP and TCP SIP mirror servers (alternative SIP port)

## Testing Process

The tool provides two main tests:

### SIP ALG Detection Test

**Client-Side Testing (Recommended):**

The SIP ALG test runs from your local computer to the backend server. This is critical because:
- **Why client-side?** SIP ALG modifications occur at your router. Testing must originate from your PC and pass through your router to detect these modifications.
- **What it tests:** The tool sends SIP INVITE requests over UDP and TCP from your computer to the server's mirror service on ports 5060 and 5062.
- **How it works:** The server mirrors back the exact SIP packet it received. By comparing the original sent packet with the mirrored response, the tool can detect any modifications made by your router's SIP ALG.
- **Results:** Shows results for all 4 test combinations (2 ports × 2 protocols: UDP/TCP on 5060/5062)

**To run the test:**
1. Download the client test tool from the web interface
2. Run it from your personal computer: `sip-alg-tester-[platform] <server-ip>`
3. Review the results to see if your router is modifying SIP packets

### VoIP Quality Test (Connection Quality Tab)

1. **SIP REGISTER**: Tests client registration with the server
2. **SIP SUBSCRIBE**: Tests presence subscription
3. **SIP NOTIFY**: Tests server-initiated notifications
4. **SIP INVITE**: Tests call invitation flow (100 Trying → 180 Ringing → 200 OK)
5. **Quality Test**: 2-minute connectivity test measuring latency, jitter, and packet loss

## Pass Criteria

- **Packet Loss**: < 1%
- **Jitter**: < 30ms
- **Latency**: < 150ms

## Troubleshooting

### Port 5060 Already in Use

If you see a warning about port 5060, it's likely Asterisk is running:

```bash
# Check if Asterisk is using port 5060
sudo netstat -tulpn | grep 5060

# Stop Asterisk if needed
sudo systemctl stop asterisk
```

### Connection Refused

Ensure the firewall allows connections:
```bash
sudo ufw status
```

### WebSocket Connection Failed

Check that the server is running and the correct port is selected in the web interface.

### SIP ALG Test Issues

**"Test Failed" or Connection Timeout:**

1. Ensure the server is running and accessible:
   ```bash
   # Check if server is running
   curl http://<server-ip>:3000
   
   # Test if SIP ports are accessible
   nc -zv <server-ip> 5060
   nc -zv <server-ip> 5062
   ```

2. Check firewall on both server and client:
   - **Server:** Ports 5060 and 5062 must be open for UDP and TCP
   - **Client:** Your local firewall must allow outbound UDP/TCP on ports 5060 and 5062

3. Verify you're running the test from your PC (not from the server itself)

**"No ALG Detected" When You Expect ALG:**

This is actually good news! If the test shows "No ALG Detected", it means:
- Your router is not modifying SIP packets
- Your VoIP setup should work without SIP ALG-related issues

**"ALG Detected" - What to Do:**

If the test detects SIP ALG modifications:
1. Access your router's admin interface
2. Look for "SIP ALG", "SIP Passthrough", or "SIP Helper" settings
3. Disable SIP ALG (the exact location varies by router manufacturer)
4. Reboot your router
5. Run the test again to verify ALG is disabled

**Cannot Download Client Tools:**

If the download links don't work:
1. Ensure the server administrator has built the client tools: `./build-client.sh`
2. Check that the `public/downloads/` directory contains the executables
3. Alternatively, download `client-tester.js` and run with Node.js: `node client-tester.js <server-ip>`

## License

ISC
