const express = require('express');
const WebSocket = require('ws');
const http = require('http');
const path = require('path');

const app = express();
const server = http.createServer(app);

const PORTS = [3000, 5060, 5062];
const HOST_IP = '193.105.36.4'; // Your WAN IP

// Serve static frontend files
app.use(express.static(path.join(__dirname, 'public')));

// Helper function to setup WebSocket connection handler
function setupWebSocketHandler(wss) {
    wss.on('connection', (ws, req) => {
        const clientIp = req.socket.remoteAddress;
        const localPort = req.socket.localPort;
        console.log(`New client connected from ${clientIp} on port ${localPort}`);

        ws.on('message', (message) => {
            try {
                const data = JSON.parse(message);
                handleMessage(ws, data, clientIp);
            } catch (e) {
                console.error('Invalid JSON received', e);
            }
        });

        ws.on('close', () => {
            console.log(`Client disconnected from port ${localPort}`);
        });
    });
}

function handleMessage(ws, data, clientIp) {
    const { type, step, payload } = data;

    // Helper to send back to client
    const send = (msgType, content) => {
        if (ws.readyState === WebSocket.OPEN) {
            ws.send(JSON.stringify({ type: msgType, ...content }));
        }
    };

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

server.listen(PORTS[0], '0.0.0.0', () => {
    console.log(`SIP Check Server (HTTP + WebSocket) running on port ${PORTS[0]}`);
    console.log(`Public access: http://${HOST_IP}:${PORTS[0]}`);
});

// Setup WebSocket on the main HTTP server (port 3000)
const mainWss = new WebSocket.Server({ server });
setupWebSocketHandler(mainWss);

// Create additional WebSocket servers on ports 5060 and 5062
PORTS.slice(1).forEach(port => {
    try {
        const wsServer = http.createServer((req, res) => {
            // Handle non-WebSocket HTTP requests
            res.writeHead(200, { 'Content-Type': 'text/plain' });
            res.end(`SIP WebSocket Server on port ${port}\n`);
        });
        const wss = new WebSocket.Server({ server: wsServer });
        
        setupWebSocketHandler(wss);
        
        wsServer.listen(port, '0.0.0.0', () => {
            console.log(`WebSocket SIP Server running on port ${port}`);
            console.log(`Public access: ws://${HOST_IP}:${port}`);
        });

        wsServer.on('error', (err) => {
            if (err.code === 'EADDRINUSE') {
                console.warn(`⚠️  Warning: Port ${port} is already in use (possibly Asterisk). Skipping this port.`);
            } else {
                console.error(`Error on port ${port}:`, err);
            }
        });
    } catch (err) {
        console.error(`Failed to create server on port ${port}:`, err);
    }
});
