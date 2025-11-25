const express = require('express');
const WebSocket = require('ws');
const http = require('http');
const path = require('path');
const dgram = require('dgram');
const { runAlgTest } = require('./algTest');

const app = express();
const server = http.createServer(app);
const wss3000 = new WebSocket.Server({ server });

const PORT = 3000;
const WEB_PORT = PORT;
const PORT_5060 = 5060;
const PORT_5062 = 5062;
const SIP_PORTS = [PORT_5060, PORT_5062];
const HOST_IP = process.env.SIPALG_HOST_IP || '193.105.36.4'; // Your WAN IP
const ALG_TEST_IP = process.env.SIPALG_TEST_IP || HOST_IP;
const ALG_TEST_PORT = parseInt(process.env.SIPALG_TEST_PORT || '5060', 10);
const OPEN_STATE = 1; // WebSocket.OPEN constant value, used for both WebSocket and UDP wrapper

// Serve static frontend files
app.use(express.static(path.join(__dirname, 'public')));

// Create standalone WebSocket servers for ports 5060 and 5062
let wss5060 = null;
let wss5062 = null;

// Helper function to setup WebSocket connection handlers
function setupWebSocketHandlers(wss, portName) {
    wss.on('connection', (ws, req) => {
        const clientIp = req.socket.remoteAddress;
        console.log(`[Port ${portName}] New client connected from ${clientIp}`);

        ws.on('message', (message) => {
            try {
                const data = JSON.parse(message);
                handleMessage(ws, data, clientIp);
            } catch (e) {
                console.error(`[Port ${portName}] Invalid JSON received`, e);
            }
        });

        ws.on('close', () => {
            console.log(`[Port ${portName}] Client disconnected`);
        });
    });
}

// Setup WebSocket handlers for main server (port 3000)
setupWebSocketHandlers(wss3000, PORT);

function handleMessage(ws, data, clientIp) {
    const { type, step, payload } = data;

    // Helper to send back to client
    const send = (msgType, content) => {
        // Check if connection is open (works for both WebSocket and UDP wrapper)
        if (ws.readyState === OPEN_STATE) {
            ws.send(JSON.stringify({ type: msgType, ...content }));
        }
    };

    // 0. ALG TEST (Mirror-style test)
    if (type === 'ALG_TEST') {
        console.log(`[ALG Test] Received ALG_TEST request from ${clientIp}`);
        
        const testServerIp = (payload && payload.serverIp) || ALG_TEST_IP;
        const testServerPort = (payload && payload.serverPort) || ALG_TEST_PORT;
        const localIp = '0.0.0.0'; // Let OS pick appropriate local address
        const localPort = 0; // Let OS assign a free port
        
        runAlgTest({
            serverIp: testServerIp,
            serverPort: testServerPort,
            localIp: localIp,
            localPort: localPort,
            timeout: 10000
        })
        .then(result => {
            console.log(`[ALG Test] Test completed. Return code: ${result.returnCode}`);
            send('ALG_RESULT', { result });
        })
        .catch(error => {
            console.error(`[ALG Test] Test failed: ${error.message}`);
            send('ALG_RESULT', { error: error.message });
        });
        
        return;
    }

    // 1. REGISTER CHECK
    if (type === 'SIP_REGISTER') {
        // Check if the client's local IP (in Via) matches the public IP we see.
        // If they match, no NAT. If different, NAT is present.
        // If the header looks "weird" or modified incorrectly, it might be ALG.
        send('SIP_RESPONSE', {
            step: 'register',
            status: 200,
            method: 'REGISTER',
            reason: 'OK',
            publicIp: clientIp
        });
    }

    // 2. INVITE CHECK
    else if (type === 'SIP_INVITE') {
        // Simulate typical SIP flow: 100 Trying -> 180 Ringing -> 200 OK
        setTimeout(() => send('SIP_RESPONSE', { step: 'invite', status: 100, method: 'INVITE', reason: 'Trying' }), 100);
        setTimeout(() => send('SIP_RESPONSE', { step: 'invite', status: 180, method: 'INVITE', reason: 'Ringing' }), 500);
        setTimeout(() => send('SIP_RESPONSE', { step: 'invite', status: 200, method: 'INVITE', reason: 'OK' }), 1000);
    }

    // 3. SUBSCRIBE CHECK
    else if (type === 'SIP_SUBSCRIBE') {
        send('SIP_RESPONSE', { step: 'subscribe', status: 200, method: 'SUBSCRIBE', reason: 'OK' });
        
        // 4. NOTIFY CHECK (Server initiates this after subscription)
        setTimeout(() => {
            send('SIP_REQUEST', { 
                step: 'notify', 
                method: 'NOTIFY', 
                headers: { 'Event': 'presence' } 
            });
        }, 1500);
    }

    // 5. QUALITY CHECK (Ping/Pong for Jitter/Loss)
    else if (type === 'PING') {
        // Echo back immediately with server timestamp
        send('PONG', { 
            clientTimestamp: payload.timestamp, 
            serverTimestamp: Date.now(),
            seq: payload.seq 
        });
    }
}

// Start the main web server on port 3000
server.listen(WEB_PORT, '0.0.0.0', () => {
    console.log(`SIP Check Server running on http://0.0.0.0:${WEB_PORT}`);
    console.log(`Public access: http://${HOST_IP}:${WEB_PORT}`);
});

// Try to start WebSocket servers on SIP ports (5060, 5062)
SIP_PORTS.forEach(port => {
    const sipHttpServer = http.createServer();
    const sipWss = new WebSocket.Server({ server: sipHttpServer });
    
    setupWebSocketHandlers(sipWss, `SIP port ${port}`);
    
    // Handle errors from the HTTP server
    sipHttpServer.on('error', (err) => {
        if (err.code === 'EADDRINUSE') {
            console.warn(`⚠ Warning: Port ${port} is already in use (possibly by Asterisk). Skipping this port.`);
        } else {
            console.error(`Error on port ${port}:`, err.message);
        }
    });
    
    // Handle errors from the WebSocket server
    sipWss.on('error', (err) => {
        if (err.code === 'EADDRINUSE') {
            console.warn(`⚠ Warning: WebSocket port ${port} is already in use. Skipping this port.`);
        } else {
            console.error(`WebSocket error on port ${port}:`, err.message);
        }
    });
    
    try {
        sipHttpServer.listen(port, '0.0.0.0', () => {
            console.log(`WebSocket server running on port ${port} for SIP testing`);
            console.log(`Test connection: ws://${HOST_IP}:${port}`);
        });
    } catch (err) {
        console.warn(`⚠ Warning: Could not start server on port ${port}: ${err.message}`);
    }
});

// Try to create WebSocket server on port 5060
try {
    wss5060 = new WebSocket.Server({ port: PORT_5060, host: '0.0.0.0' });
    
    wss5060.on('error', (error) => {
        if (error.code === 'EADDRINUSE') {
            console.warn(`⚠️  Warning: Port ${PORT_5060} is already in use`);
            console.warn(`   This is likely because Asterisk or another service is using this port.`);
            console.warn(`   Server will continue running on ports ${PORT} and ${PORT_5062}.`);
        } else {
            console.warn(`⚠️  Warning: WebSocket server error on port ${PORT_5060}: ${error.message}`);
        }
        wss5060 = null;
    });
    
    setupWebSocketHandlers(wss5060, PORT_5060);
    console.log(`WebSocket server running on port ${PORT_5060}`);
} catch (error) {
    console.warn(`⚠️  Warning: Could not start WebSocket server on port ${PORT_5060}`);
    console.warn(`   Error: ${error.message}`);
}

// Create WebSocket server on port 5062
try {
    wss5062 = new WebSocket.Server({ port: PORT_5062, host: '0.0.0.0' });
    
    wss5062.on('error', (error) => {
        if (error.code === 'EADDRINUSE') {
            console.error(`❌ Error: Port ${PORT_5062} is already in use`);
        } else {
            console.error(`❌ Error: WebSocket server error on port ${PORT_5062}: ${error.message}`);
        }
        wss5062 = null;
    });
    
    setupWebSocketHandlers(wss5062, PORT_5062);
    console.log(`WebSocket server running on port ${PORT_5062}`);
} catch (error) {
    console.error(`❌ Error: Could not start WebSocket server on port ${PORT_5062}`);
    console.error(`   Error: ${error.message}`);
}

// Create UDP sockets for ports 5060 and 5062
let udp5060 = null;
let udp5062 = null;

// Helper function to create a WebSocket-like wrapper for UDP responses
function createUdpWrapper(socket, rinfo, port) {
    return {
        readyState: OPEN_STATE, // Use same state as WebSocket.OPEN for compatibility
        send: (message) => {
            socket.send(message, rinfo.port, rinfo.address, (err) => {
                if (err) {
                    console.error(`[UDP ${port}] Error sending response:`, err);
                }
            });
        }
    };
}

// Helper function to send error response over UDP
function sendUdpError(socket, rinfo, errorMessage) {
    const errorResponse = JSON.stringify({ 
        type: 'ERROR', 
        message: errorMessage 
    });
    socket.send(errorResponse, rinfo.port, rinfo.address, (err) => {
        if (err) {
            console.error(`[UDP] Error sending error response:`, err);
        }
    });
}

// Helper function to setup UDP socket
function setupUdpServer(port, isSecondary = false) {
    const udpSocket = dgram.createSocket('udp4');
    
    udpSocket.on('error', (err) => {
        if (err.code === 'EADDRINUSE') {
            if (isSecondary) {
                console.error(`❌ Error: UDP port ${port} is already in use`);
            } else {
                console.warn(`⚠️  Warning: UDP port ${port} is already in use`);
                console.warn(`   This is likely because Asterisk or another service is using this port.`);
                console.warn(`   UDP server will continue on port ${PORT_5062} only.`);
            }
        } else {
            console.error(`❌ Error: UDP server error on port ${port}: ${err.message}`);
        }
    });
    
    udpSocket.on('message', (msg, rinfo) => {
        console.log(`[UDP ${port}] Received message from ${rinfo.address}:${rinfo.port}`);
        try {
            const data = JSON.parse(msg.toString());
            const udpWrapper = createUdpWrapper(udpSocket, rinfo, port);
            handleMessage(udpWrapper, data, rinfo.address);
        } catch (e) {
            console.error(`[UDP ${port}] Invalid JSON received:`, e.message);
            sendUdpError(udpSocket, rinfo, 'Invalid JSON format');
        }
    });
    
    udpSocket.on('listening', () => {
        const address = udpSocket.address();
        console.log(`UDP server running on ${address.address}:${address.port}`);
    });
    
    // Bind the socket (errors are handled by the error event listener)
    udpSocket.bind(port, '0.0.0.0');
    return udpSocket;
}

// Setup UDP servers
udp5060 = setupUdpServer(PORT_5060, false);
udp5062 = setupUdpServer(PORT_5062, true);

// Graceful shutdown handler
process.on('SIGINT', () => {
    console.log('\nShutting down servers...');
    server.close();
    if (wss5060) wss5060.close();
    if (wss5062) wss5062.close();
    if (udp5060) udp5060.close();
    if (udp5062) udp5062.close();
    process.exit(0);
});
