const express = require('express');
const WebSocket = require('ws');
const http = require('http');
const path = require('path');

const app = express();
const server = http.createServer(app);
const wss3000 = new WebSocket.Server({ server });

const PORT = 3000;
const HOST_IP = '193.105.36.4'; // Your WAN IP

// Serve static frontend files
app.use(express.static(path.join(__dirname, 'public')));

// Handle WebSocket connection logic (shared across all ports)
function setupWebSocketHandlers(wss, portName) {
    wss.on('connection', (ws, req) => {
        const clientIp = req.socket.remoteAddress;
        console.log(`New client connected from ${clientIp} on ${portName}`);

        ws.on('message', (message) => {
            try {
                const data = JSON.parse(message);
                handleMessage(ws, data, clientIp);
            } catch (e) {
                console.error('Invalid JSON received', e);
            }
        });

        ws.on('close', () => {
            console.log(`Client disconnected from ${portName}`);
        });
    });
}

// Setup WebSocket handlers for port 3000
setupWebSocketHandlers(wss3000, 'Port 3000');

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

server.listen(PORT, '0.0.0.0', () => {
    console.log(`SIP Check Server running on http://0.0.0.0:${PORT}`);
    console.log(`Public access: http://${HOST_IP}:${PORT}`);
});

// Additional WebSocket server on Port 5060 (Standard SIP port)
// This port might be in use by Asterisk, so handle errors gracefully
const server5060 = http.createServer();
const wss5060 = new WebSocket.Server({ server: server5060 });
setupWebSocketHandlers(wss5060, 'Port 5060');

server5060.on('error', (err) => {
    if (err.code === 'EADDRINUSE') {
        console.warn('⚠️  WARNING: Port 5060 is already in use (possibly by Asterisk). Continuing without it.');
    } else {
        console.error(`Error on port 5060: ${err.message}`);
    }
});

server5060.listen(5060, '0.0.0.0', () => {
    console.log(`WebSocket server running on port 5060`);
});

// Additional WebSocket server on Port 5062 (Alternative SIP port)
const server5062 = http.createServer();
const wss5062 = new WebSocket.Server({ server: server5062 });
setupWebSocketHandlers(wss5062, 'Port 5062');

server5062.on('error', (err) => {
    console.error(`Error on port 5062: ${err.message}`);
    process.exit(1);
});

server5062.listen(5062, '0.0.0.0', () => {
    console.log(`WebSocket server running on port 5062`);
});
