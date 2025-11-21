# SIP ALG & Quality Check Tool

A Node.js-based tool for testing SIP (Session Initiation Protocol) connectivity and quality on Ubuntu 24.04 servers. This tool helps diagnose SIP ALG (Application Layer Gateway) issues and provides network quality metrics.

## Features

- **SIP Protocol Testing**: Tests SIP REGISTER, SUBSCRIBE, NOTIFY, and INVITE messages
- **Multi-Port Support**: WebSocket servers on ports 3000, 5060, and 5062
- **Quality Metrics**: 30-second connectivity test measuring:
  - Latency (average round-trip time)
  - Jitter (variance in latency)
  - Packet Loss percentage
- **Asterisk Compatibility**: Graceful handling if Asterisk is already using port 5060

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

The tool performs 5 checks in sequence:

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
