const express = require('express');
const WebSocket = require('ws');
const http = require('http');
const path = require('path');

const app = express();
const server = http.createServer(app);
const wss = new WebSocket.Server({ server });

const WEB_PORT = 3000;
const SIP_PORTS = [5060, 5062];
const HOST_IP = '193.105.36.4'; // Your WAN IP

// Serve static frontend files
app.use(express.static(path.join(__dirname, 'public')));

// Helper function to setup WebSocket connection handlers
function setupWebSocketHandlers(wss, portInfo) {
    wss.on('connection', (ws, req) => {
        const clientIp = req.socket.remoteAddress;
        console.log(`New client connected from ${clientIp} on ${portInfo}`);

        ws.on('message', (message) => {
            try {
                const data = JSON.parse(message);
                handleMessage(ws, data, clientIp);
            } catch (e) {
                console.error('Invalid JSON received', e);
            }
        });

        ws.on('close', () => {
            console.log('Client disconnected');
        });
    });
}

// Handle WebSocket connections on port 3000 (web interface)
setupWebSocketHandlers(wss, `port ${WEB_PORT}`);

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
