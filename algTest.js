/**
 * SIP ALG Detection Module
 * 
 * Performs SIP INVITE-based ALG detection similar to the Ruby sip-alg-detector client.
 * Sends a SIP INVITE request and compares the mirrored response to detect ALG modifications.
 */

const dgram = require('dgram');
const net = require('net');
const crypto = require('crypto');

/**
 * Result codes compatible with Ruby tool semantics
 */
const RESULT_CODES = {
    FALSE: { code: 1, text: 'FALSE' },      // No ALG detected
    TRUE: { code: 2, text: 'TRUE' },        // ALG detected
    DISABLED: { code: 3, text: 'disabled' }, // Test disabled (not used)
    FAILED: { code: 4, text: 'test failed' } // Test failed
};

/**
 * Generate a random alphanumeric string
 */
function randomString(length) {
    return crypto.randomBytes(Math.ceil(length / 2))
        .toString('hex')
        .slice(0, length);
}

/**
 * Generate a random Call-ID
 */
function generateCallId() {
    return `${randomString(16)}@${randomString(8)}`;
}

/**
 * Generate a random branch parameter
 */
function generateBranch() {
    return `z9hG4bK-${randomString(16)}`;
}

/**
 * Generate a random tag
 */
function generateTag() {
    return randomString(10);
}

/**
 * Build a SIP INVITE request with randomized fields
 */
function buildSipInvite(localIp, localPort, serverIp, serverPort, transport) {
    const callId = generateCallId();
    const branch = generateBranch();
    const fromTag = generateTag();
    const cseq = Math.floor(Math.random() * 1000) + 1;
    
    // Generate random SDP session ID and version
    const sdpSessionId = randomString(10);
    const sdpSessionVersion = randomString(8);
    
    const sdpBody = [
        'v=0',
        `o=user ${sdpSessionId} ${sdpSessionVersion} IN IP4 ${localIp}`,
        's=SIP ALG Test',
        `c=IN IP4 ${localIp}`,
        't=0 0',
        'm=audio 10000 RTP/AVP 0 8 101',
        'a=rtpmap:0 PCMU/8000',
        'a=rtpmap:8 PCMA/8000',
        'a=rtpmap:101 telephone-event/8000',
        'a=fmtp:101 0-16',
        ''
    ].join('\r\n');
    
    const contentLength = Buffer.byteLength(sdpBody, 'utf8');
    
    const sipRequest = [
        `INVITE sip:test@${serverIp}:${serverPort} SIP/2.0`,
        `Via: SIP/2.0/${transport} ${localIp}:${localPort};branch=${branch}`,
        `From: <sip:client@${localIp}>;tag=${fromTag}`,
        `To: <sip:test@${serverIp}>`,
        `Call-ID: ${callId}`,
        `CSeq: ${cseq} INVITE`,
        'Max-Forwards: 70',
        `Contact: <sip:client@${localIp}:${localPort};transport=${transport.toLowerCase()}>`,
        'Content-Type: application/sdp',
        `Content-Length: ${contentLength}`,
        '',
        sdpBody
    ].join('\r\n');
    
    return {
        request: sipRequest,
        metadata: {
            callId,
            branch,
            fromTag,
            cseq,
            localIp,
            localPort,
            serverIp,
            serverPort,
            transport
        }
    };
}

/**
 * Compare two SIP messages and return differences
 */
function compareSipMessages(original, mirrored) {
    const differences = [];
    
    // Normalize line endings for comparison
    const origLines = original.replace(/\r\n/g, '\n').split('\n');
    const mirrorLines = mirrored.replace(/\r\n/g, '\n').split('\n');
    
    const maxLen = Math.max(origLines.length, mirrorLines.length);
    
    for (let i = 0; i < maxLen; i++) {
        const origLine = origLines[i] || '';
        const mirrorLine = mirrorLines[i] || '';
        
        if (origLine !== mirrorLine) {
            differences.push({
                line: i + 1,
                original: origLine,
                mirrored: mirrorLine
            });
        }
    }
    
    return differences;
}

/**
 * Create a diff snippet for display
 */
function createDiffSnippet(differences) {
    if (differences.length === 0) {
        return '';
    }
    
    const snippet = differences.slice(0, 5).map(diff => {
        return `Line ${diff.line}:\n  Original: ${diff.original}\n  Mirrored:  ${diff.mirrored}`;
    }).join('\n\n');
    
    if (differences.length > 5) {
        return snippet + `\n\n... and ${differences.length - 5} more differences`;
    }
    
    return snippet;
}

/**
 * Test ALG detection over UDP
 */
function testUdp(localIp, localPort, serverIp, serverPort, timeout = 10000) {
    return new Promise((resolve) => {
        const socket = dgram.createSocket('udp4');
        let timeoutHandle;
        let sipData = null; // Store the SIP request data for comparison
        let isResolved = false;
        
        const cleanup = () => {
            clearTimeout(timeoutHandle);
            try {
                socket.close();
            } catch (e) {
                // Socket might already be closed
            }
        };
        
        const resolveOnce = (result) => {
            if (!isResolved) {
                isResolved = true;
                cleanup();
                resolve(result);
            }
        };
        
        // Handle timeout
        timeoutHandle = setTimeout(() => {
            resolveOnce({
                ...RESULT_CODES.FAILED,
                error: 'Timeout waiting for server response',
                diff: '',
                port: serverPort,
                transport: 'udp'
            });
        }, timeout);
        
        // Handle errors
        socket.on('error', (err) => {
            resolveOnce({
                ...RESULT_CODES.FAILED,
                error: `Socket error: ${err.message}`,
                diff: '',
                port: serverPort,
                transport: 'udp'
            });
        });
        
        // Handle incoming messages
        socket.on('message', (msg, rinfo) => {
            try {
                const response = msg.toString('utf8');
                
                // The test server echoes back the received request in the body
                // Extract body from SIP 200 OK response
                const bodyMatch = response.match(/\r\n\r\n(.+)$/s);
                if (!bodyMatch || !bodyMatch[1]) {
                    resolveOnce({
                        ...RESULT_CODES.FAILED,
                        error: 'Could not extract mirrored request from server response',
                        diff: '',
                        port: serverPort,
                        transport: 'udp'
                    });
                    return;
                }
                
                const mirroredRequest = bodyMatch[1].trim();
                
                // Ensure sipData is available for comparison
                if (!sipData) {
                    resolveOnce({
                        ...RESULT_CODES.FAILED,
                        error: 'Original SIP request data not available for comparison',
                        diff: '',
                        port: serverPort,
                        transport: 'udp'
                    });
                    return;
                }
                
                // Compare original with mirrored
                const differences = compareSipMessages(sipData.request, mirroredRequest);
                
                if (differences.length === 0) {
                    resolveOnce({
                        ...RESULT_CODES.FALSE,
                        diff: '',
                        port: serverPort,
                        transport: 'udp'
                    });
                } else {
                    resolveOnce({
                        ...RESULT_CODES.TRUE,
                        diff: createDiffSnippet(differences),
                        port: serverPort,
                        transport: 'udp'
                    });
                }
            } catch (err) {
                resolveOnce({
                    ...RESULT_CODES.FAILED,
                    error: `Error processing response: ${err.message}`,
                    diff: '',
                    port: serverPort,
                    transport: 'udp'
                });
            }
        });
        
        // Bind and send
        try {
            socket.bind(localPort, localIp, () => {
                try {
                    const actualLocalPort = socket.address().port;
                    sipData = buildSipInvite(localIp, actualLocalPort, serverIp, serverPort, 'UDP');
                    const buffer = Buffer.from(sipData.request, 'utf8');
                    
                    socket.send(buffer, 0, buffer.length, serverPort, serverIp, (err) => {
                        if (err) {
                            resolveOnce({
                                ...RESULT_CODES.FAILED,
                                error: `Failed to send request: ${err.message}`,
                                diff: '',
                                port: serverPort,
                                transport: 'udp'
                            });
                        }
                    });
                } catch (err) {
                    resolveOnce({
                        ...RESULT_CODES.FAILED,
                        error: `Error preparing request: ${err.message}`,
                        diff: '',
                        port: serverPort,
                        transport: 'udp'
                    });
                }
            });
        } catch (err) {
            resolveOnce({
                ...RESULT_CODES.FAILED,
                error: `Failed to bind socket: ${err.message}`,
                diff: '',
                port: serverPort,
                transport: 'udp'
            });
        }
    });
}

/**
 * Test ALG detection over TCP  
 */
function testTcp(localIp, localPort, serverIp, serverPort, timeout = 10000) {
    return new Promise((resolve) => {
        const socket = new net.Socket();
        let timeoutHandle;
        let receivedData = '';
        
        const cleanup = () => {
            clearTimeout(timeoutHandle);
            socket.destroy();
        };
        
        // Handle timeout
        timeoutHandle = setTimeout(() => {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: 'Timeout waiting for server response',
                diff: '',
                port: serverPort,
                transport: 'tcp'
            });
        }, timeout);
        
        // Handle errors
        socket.on('error', (err) => {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: `Socket error: ${err.message}`,
                diff: '',
                port: serverPort,
                transport: 'tcp'
            });
        });
        
        // Build SIP request
        const sipData = buildSipInvite(localIp, localPort || 0, serverIp, serverPort, 'TCP');
        
        // Handle incoming data
        socket.on('data', (data) => {
            receivedData += data.toString('utf8');
            
            // Check if we have a complete response (ends with \r\n\r\n or has body)
            if (receivedData.includes('\r\n\r\n')) {
                cleanup();
                
                // Extract body from SIP 200 OK response
                const bodyMatch = receivedData.match(/\r\n\r\n(.+)$/s);
                if (!bodyMatch || !bodyMatch[1]) {
                    resolve({
                        ...RESULT_CODES.FAILED,
                        error: 'Could not extract mirrored request from server response',
                        diff: '',
                        port: serverPort,
                        transport: 'tcp'
                    });
                    return;
                }
                
                const mirroredRequest = bodyMatch[1].trim();
                
                // Compare original with mirrored
                const differences = compareSipMessages(sipData.request, mirroredRequest);
                
                if (differences.length === 0) {
                    resolve({
                        ...RESULT_CODES.FALSE,
                        diff: '',
                        port: serverPort,
                        transport: 'tcp'
                    });
                } else {
                    resolve({
                        ...RESULT_CODES.TRUE,
                        diff: createDiffSnippet(differences),
                        port: serverPort,
                        transport: 'tcp'
                    });
                }
            }
        });
        
        // Connect and send
        socket.connect(serverPort, serverIp, () => {
            socket.write(sipData.request);
        });
    });
}

/**
 * Run ALG test for both UDP and TCP on multiple ports
 * 
 * @param {Object} options - Test options
 * @param {string} options.serverIp - Target server IP
 * @param {Array<number>} options.serverPorts - Target server ports (default: [5060, 5062])
 * @param {string} options.localIp - Local IP address (default: '0.0.0.0')
 * @param {number} options.localPort - Local port for UDP (default: 0, let OS assign)
 * @param {number} options.timeout - Timeout in milliseconds (default: 10000)
 * @returns {Promise<Object>} Test results for UDP and TCP on each port
 */
async function runAlgTest(options) {
    const {
        serverIp,
        serverPorts = [5060, 5062],
        localIp = '0.0.0.0',
        localPort = 0, // Use 0 to let OS assign a free port
        timeout = 10000
    } = options;
    
    console.log(`[ALG Test] Starting test to ${serverIp} on ports ${serverPorts.join(', ')}`);
    
    try {
        // Test all ports
        const allTests = [];
        for (const port of serverPorts) {
            allTests.push(
                testUdp(localIp, localPort, serverIp, port, timeout),
                testTcp(localIp, 0, serverIp, port, timeout)
            );
        }
        
        const results = await Promise.all(allTests);
        
        // Organize results by port
        const portResults = {};
        for (const result of results) {
            const port = result.port;
            const transport = result.transport;
            
            if (!portResults[port]) {
                portResults[port] = { udp: null, tcp: null };
            }
            
            portResults[port][transport] = result;
        }
        
        // Log individual results
        for (const [port, portResult] of Object.entries(portResults)) {
            console.log(`[ALG Test] Port ${port} UDP: ${portResult.udp.text} (code: ${portResult.udp.code})`);
            console.log(`[ALG Test] Port ${port} TCP: ${portResult.tcp.text} (code: ${portResult.tcp.code})`);
        }
        
        // Determine overall return code
        // If any test shows ALG (code 2), return 2
        // If all tests are FALSE (code 1), return 1
        // Otherwise return 4 (failed)
        let returnCode = RESULT_CODES.FALSE.code;
        let hasAnyFailed = false;
        
        for (const result of results) {
            if (result.code === RESULT_CODES.TRUE.code) {
                returnCode = RESULT_CODES.TRUE.code;
                break;
            } else if (result.code === RESULT_CODES.FAILED.code) {
                hasAnyFailed = true;
            }
        }
        
        if (returnCode !== RESULT_CODES.TRUE.code && hasAnyFailed) {
            returnCode = RESULT_CODES.FAILED.code;
        }
        
        return {
            ports: portResults,
            returnCode
        };
    } catch (error) {
        console.error(`[ALG Test] Error: ${error.message}`);
        throw error;
    }
}

module.exports = {
    runAlgTest,
    RESULT_CODES
};
