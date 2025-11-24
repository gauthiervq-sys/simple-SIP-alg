# SIP ALG & Quality Check Tool

A Node.js-based tool for testing SIP (Session Initiation Protocol) connectivity and quality on Ubuntu 24.04 servers. This tool helps diagnose SIP ALG (Application Layer Gateway) issues and provides network quality metrics.

## Features

- **SIP ALG Detection**: Mirror-style test similar to Ruby sip-alg-detector client
  - Tests both UDP and TCP transports
  - Compares original vs mirrored SIP INVITE requests
  - Detects ALG modifications in real-time
- **SIP Protocol Testing**: Tests SIP REGISTER, SUBSCRIBE, NOTIFY, and INVITE messages
- **Multi-Port Support**: WebSocket servers on ports 3000, 5060, and 5062
- **Quality Metrics**: 30-second connectivity test measuring:
  - Latency (average round-trip time)
  - Jitter (variance in latency)
  - Packet Loss percentage
- **Asterisk Compatibility**: Graceful handling if Asterisk is already using port 5060
- **Automatic Backend**: Runs as a systemd service or with PM2

## Prerequisites

- Node.js (v14 or higher)
- npm (Node Package Manager)
- Ubuntu 24.04 (recommended, but works on other Linux distributions)

## Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd simple-SIP-alg
```

2. Install dependencies:
```bash
npm install
```

## Usage

### Quick Start (Manual)

1. Start the server:
```bash
node server.js
```

The server will start:
- Web server on port 3000
- WebSocket servers on ports 3000, 5060, and 5062

**Note:** If Asterisk is running on port 5060, the server will log a warning but continue running on ports 3000 and 5062.

2. Open your web browser and navigate to:
```
http://localhost:3000
```

Or use your server's public IP:
```
http://193.105.36.4:3000
```

3. Select the port you want to test (3000, 5060, or 5062) and click "Start Check"

### Automatic Backend Runtime (Production)

For production deployments on Ubuntu 24.04, you can set up the backend to run automatically at boot using systemd:

#### Option 1: Systemd Service (Recommended for Ubuntu)

1. Copy your application to `/opt/simple-SIP-alg`:
```bash
sudo mkdir -p /opt/simple-SIP-alg
sudo cp -r /path/to/simple-SIP-alg/* /opt/simple-SIP-alg/
cd /opt/simple-SIP-alg
sudo npm install
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

- `SIPALG_HOST_IP`: Your WAN/public IP address (default: `193.105.36.4`)
- `SIPALG_TEST_IP`: Target IP for ALG test (default: same as `SIPALG_HOST_IP`)
- `SIPALG_TEST_PORT`: Target port for ALG test (default: `5060`)

These can be set in:
- The systemd service file (`/etc/systemd/system/simple-sip-alg.service`)
- Your shell environment before starting the server
- Or as command line arguments: `SIPALG_HOST_IP=1.2.3.4 node server.js`

## Firewall Configuration

Ensure the following ports are open in your firewall:

```bash
sudo ufw allow 3000/tcp
sudo ufw allow 5060/tcp
sudo ufw allow 5062/tcp
```

## Port Information

- **Port 3000**: HTTP server and WebSocket (always available)
- **Port 5060**: WebSocket server (standard SIP port, may conflict with Asterisk)
- **Port 5062**: WebSocket server (alternative SIP port)

## Testing Process

The tool performs 6 checks in sequence:

0. **SIP ALG Detection (Mirror Test)**: Sends SIP INVITE to a detector server and compares the mirrored response to detect ALG modifications
1. **SIP REGISTER**: Tests client registration with the server
2. **SIP SUBSCRIBE**: Tests presence subscription
3. **SIP NOTIFY**: Tests server-initiated notifications
4. **SIP INVITE**: Tests call invitation flow (100 Trying → 180 Ringing → 200 OK)
5. **Quality Test**: 30-second connectivity test measuring latency, jitter, and packet loss

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

## License

ISC
