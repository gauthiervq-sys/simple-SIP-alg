const express = require('express');
const WebSocket = require('ws');
const http = require('http');
const net = require('net');
const path = require('path');
const dgram = require('dgram');
const crypto = require('crypto');
const { runAlgTest } = require('./algTest');

const app = express();
const server = http.createServer(app);
const wss3000 = new WebSocket.Server({ server });

const PORT = 3000;
const WEB_PORT = PORT;
const PORT_5060 = 5060;
const PORT_5062 = 5062;
const SIP_PORTS = [PORT_5060, PORT_5062];
const HOST_IP = process.env.SIPALG_HOST_IP || '193.105.36.15'; // Your WAN IP
const ALG_TEST_IP = process.env.SIPALG_TEST_IP || HOST_IP;
const ALG_TEST_PORT = parseInt(process.env.SIPALG_TEST_PORT || '5060', 10);
const OPEN_STATE = 1; // WebSocket.OPEN constant value, used for both WebSocket and UDP wrapper

// Serve static frontend files
app.use(express.static(path.join(__dirname, 'public')));

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
        
        const testServerIp = (payload && payload.serverIp) || HOST_IP; // Test against public IP to route through router/ALG
        const localIp = '0.0.0.0'; // Let OS pick appropriate local address
        const localPort = 0; // Let OS assign a free port
        
        runAlgTest({
            serverIp: testServerIp,
            serverPorts: [5060, 5062], // Test both ports
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

// SIP methods that indicate a raw SIP message
const SIP_METHODS = Object.freeze(new Set(['REGISTER', 'INVITE', 'OPTIONS', 'ACK', 'BYE', 'CANCEL', 'SUBSCRIBE', 'NOTIFY', 'REFER', 'MESSAGE', 'INFO', 'PRACK', 'UPDATE']));

// Helper function to check if a message is a raw SIP message
function isRawSipMessage(message) {
    const firstLine = message.split('\r\n')[0] || message.split('\n')[0] || '';
    // Check if it starts with a SIP method or is a SIP response (SIP/2.0)
    const method = firstLine.split(' ')[0];
    return SIP_METHODS.has(method) || firstLine.startsWith('SIP/2.0');
}

// Helper function to generate a SIP 200 OK response with mirrored request in body
function generateSipMirrorResponse(sipRequest, rinfo) {
    const lines = sipRequest.split(/\r\n|\n/);
    const requestLine = lines[0] || '';
    
    // Extract Via, From, To, Call-ID, and CSeq headers
    let via = '';
    let from = '';
    let to = '';
    let callId = '';
    let cseq = '';
    
    for (const line of lines) {
        const lineLower = line.toLowerCase();
        if (lineLower.startsWith('via:')) {
            via = line;
        } else if (lineLower.startsWith('from:')) {
            from = line;
        } else if (lineLower.startsWith('to:')) {
            to = line;
            // Add tag if not present
            if (!lineLower.includes(';tag=')) {
                to += `;tag=${crypto.randomBytes(6).toString('hex')}`;
            }
        } else if (lineLower.startsWith('call-id:')) {
            callId = line;
        } else if (lineLower.startsWith('cseq:')) {
            cseq = line;
        }
    }
    
    // The body will contain the original request exactly as received
    const bodyContent = sipRequest;
    const contentLength = Buffer.byteLength(bodyContent, 'utf8');
    
    // Build response with required headers and the mirrored request as body
    const responseHeaders = [
        'SIP/2.0 200 OK',
        via,
        from,
        to,
        callId,
        cseq,
        `Content-Length: ${contentLength}`
    ].filter(line => line !== '');
    
    // SIP requires CRLF line endings and an empty line before the body
    return responseHeaders.join('\r\n') + '\r\n\r\n' + bodyContent;
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
        const messageStr = msg.toString('utf8');
        console.log(`[UDP ${port}] Received message from ${rinfo.address}:${rinfo.port}`);
        
        // Check if this is a raw SIP message
        if (isRawSipMessage(messageStr)) {
            const firstLine = messageStr.split(/\r\n|\n/)[0] || '';
            const method = firstLine.split(' ')[0] || 'UNKNOWN';
            console.log(`[UDP ${port}] Received raw SIP ${method} message`);
            
            // Send a SIP 200 OK response with the original request mirrored in the body
            const response = generateSipMirrorResponse(messageStr, rinfo);
            udpSocket.send(response, rinfo.port, rinfo.address, (err) => {
                if (err) {
                    console.error(`[UDP ${port}] Error sending SIP response:`, err);
                } else {
                    console.log(`[UDP ${port}] Sent SIP 200 OK response to ${rinfo.address}:${rinfo.port}`);
                }
            });
            return;
        }
        
        // Try to parse as JSON
        try {
            const data = JSON.parse(messageStr);
            const udpWrapper = createUdpWrapper(udpSocket, rinfo, port);
            handleMessage(udpWrapper, data, rinfo.address);
        } catch (e) {
            console.error(`[UDP ${port}] Invalid message format (not JSON or SIP):`, e.message);
            sendUdpError(udpSocket, rinfo, 'Invalid message format');
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

// Create TCP servers for SIP ALG testing on ports 5060 and 5062
let tcp5060 = null;
let tcp5062 = null;

// Helper function to setup TCP server for SIP mirroring
function setupTcpServer(port, isSecondary = false) {
    const tcpServer = net.createServer((socket) => {
        console.log(`[TCP ${port}] New connection from ${socket.remoteAddress}:${socket.remotePort}`);
        
        let receivedData = '';
        
        socket.on('data', (data) => {
            receivedData += data.toString('utf8');
            
            // Check if we have a complete SIP message (ends with \r\n\r\n)
            if (receivedData.includes('\r\n\r\n')) {
                console.log(`[TCP ${port}] Received complete SIP message`);
                
                // Check if this is a raw SIP message
                if (isRawSipMessage(receivedData)) {
                    const firstLine = receivedData.split(/\r\n|\n/)[0] || '';
                    const method = firstLine.split(' ')[0] || 'UNKNOWN';
                    console.log(`[TCP ${port}] Received raw SIP ${method} message`);
                    
                    // Send a SIP 200 OK response with the original request mirrored in the body
                    const response = generateSipMirrorResponse(receivedData, {
                        address: socket.remoteAddress,
                        port: socket.remotePort
                    });
                    
                    socket.write(response, () => {
                        console.log(`[TCP ${port}] Sent SIP 200 OK mirror response`);
                        socket.end();
                    });
                } else {
                    console.log(`[TCP ${port}] Not a SIP message, closing connection`);
                    socket.end();
                }
                
                receivedData = ''; // Reset for next message
            }
        });
        
        socket.on('error', (err) => {
            console.error(`[TCP ${port}] Socket error:`, err.message);
        });
        
        socket.on('close', () => {
            console.log(`[TCP ${port}] Connection closed`);
        });
    });
    
    tcpServer.on('error', (err) => {
        if (err.code === 'EADDRINUSE') {
            if (isSecondary) {
                console.error(`❌ Error: TCP port ${port} is already in use`);
            } else {
                console.warn(`⚠️  Warning: TCP port ${port} is already in use`);
                console.warn(`   This is likely because Asterisk or another service is using this port.`);
                console.warn(`   TCP server will continue on port ${PORT_5062} only.`);
            }
        } else {
            console.error(`❌ Error: TCP server error on port ${port}: ${err.message}`);
        }
    });
    
    tcpServer.listen(port, '0.0.0.0', () => {
        console.log(`TCP SIP mirror server running on port ${port}`);
    });
    
    return tcpServer;
}

tcp5060 = setupTcpServer(PORT_5060, false);
tcp5062 = setupTcpServer(PORT_5062, true);

// Graceful shutdown handler
process.on('SIGINT', () => {
    console.log('\nShutting down servers...');
    server.close();
    if (udp5060) udp5060.close();
    if (udp5062) udp5062.close();
    if (tcp5060) tcp5060.close();
    if (tcp5062) tcp5062.close();
    process.exit(0);
});
