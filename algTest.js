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
        let receivedCount = 0;
        const responses = [];
        
        const cleanup = () => {
            clearTimeout(timeoutHandle);
            socket.close();
        };
        
        // Handle timeout
        timeoutHandle = setTimeout(() => {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: 'Timeout waiting for server response',
                diff: ''
            });
        }, timeout);
        
        // Handle errors
        socket.on('error', (err) => {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: `Socket error: ${err.message}`,
                diff: ''
            });
        });
        
        // Handle incoming messages
        socket.on('message', (msg, rinfo) => {
            try {
                const response = msg.toString('utf8');
                responses.push(response);
                receivedCount++;
                
                // We expect 2 responses from the sip-alg-detector daemon
                if (receivedCount >= 2) {
                    cleanup();
                    
                    // Try to extract and decode the mirrored request from responses
                    // The sip-alg-detector daemon sends the mirrored request in Base64-encoded bodies
                    let mirroredRequest = '';
                    
                    for (const resp of responses) {
                        // Look for Base64 content in the SIP response body
                        const bodyMatch = resp.match(/\r\n\r\n(.+)$/s);
                        if (bodyMatch && bodyMatch[1]) {
                            try {
                                const decoded = Buffer.from(bodyMatch[1].trim(), 'base64').toString('utf8');
                                mirroredRequest += decoded;
                            } catch (e) {
                                // Not valid Base64, might be regular response
                            }
                        }
                    }
                    
                    if (!mirroredRequest) {
                        resolve({
                            ...RESULT_CODES.FAILED,
                            error: 'Could not extract mirrored request from server responses',
                            diff: ''
                        });
                        return;
                    }
                    
                    // Compare original with mirrored
                    const differences = compareSipMessages(sipData.request, mirroredRequest);
                    
                    if (differences.length === 0) {
                        resolve({
                            ...RESULT_CODES.FALSE,
                            diff: ''
                        });
                    } else {
                        resolve({
                            ...RESULT_CODES.TRUE,
                            diff: createDiffSnippet(differences)
                        });
                    }
                }
            } catch (err) {
                cleanup();
                resolve({
                    ...RESULT_CODES.FAILED,
                    error: `Error processing response: ${err.message}`,
                    diff: ''
                });
            }
        });
        
        // Bind and send
        try {
            socket.bind(localPort, localIp, () => {
                const sipData = buildSipInvite(localIp, localPort, serverIp, serverPort, 'UDP');
                const buffer = Buffer.from(sipData.request, 'utf8');
                
                socket.send(buffer, 0, buffer.length, serverPort, serverIp, (err) => {
                    if (err) {
                        cleanup();
                        resolve({
                            ...RESULT_CODES.FAILED,
                            error: `Failed to send request: ${err.message}`,
                            diff: ''
                        });
                    }
                });
            });
        } catch (err) {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: `Failed to bind socket: ${err.message}`,
                diff: ''
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
        let responseCount = 0;
        
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
                diff: ''
            });
        }, timeout);
        
        // Handle errors
        socket.on('error', (err) => {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: `Socket error: ${err.message}`,
                diff: ''
            });
        });
        
        // Build SIP request
        const sipData = buildSipInvite(localIp, localPort || 0, serverIp, serverPort, 'TCP');
        
        // Handle incoming data
        socket.on('data', (data) => {
            receivedData += data.toString('utf8');
            
            // Count complete SIP messages (separated by \r\n\r\n)
            const messages = receivedData.split('\r\n\r\n\r\n');
            responseCount = messages.length - 1; // Last element might be incomplete
            
            // We expect 2 responses from the sip-alg-detector daemon
            if (responseCount >= 2) {
                cleanup();
                
                // Try to extract and decode the mirrored request from responses
                let mirroredRequest = '';
                
                for (let i = 0; i < responseCount; i++) {
                    const resp = messages[i];
                    // Look for Base64 content in the SIP response body
                    const bodyMatch = resp.match(/\r\n\r\n(.+)$/s);
                    if (bodyMatch && bodyMatch[1]) {
                        try {
                            const decoded = Buffer.from(bodyMatch[1].trim(), 'base64').toString('utf8');
                            mirroredRequest += decoded;
                        } catch (e) {
                            // Not valid Base64, might be regular response
                        }
                    }
                }
                
                if (!mirroredRequest) {
                    resolve({
                        ...RESULT_CODES.FAILED,
                        error: 'Could not extract mirrored request from server responses',
                        diff: ''
                    });
                    return;
                }
                
                // Compare original with mirrored
                const differences = compareSipMessages(sipData.request, mirroredRequest);
                
                if (differences.length === 0) {
                    resolve({
                        ...RESULT_CODES.FALSE,
                        diff: ''
                    });
                } else {
                    resolve({
                        ...RESULT_CODES.TRUE,
                        diff: createDiffSnippet(differences)
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
 * Run ALG test for both UDP and TCP
 * 
 * @param {Object} options - Test options
 * @param {string} options.serverIp - Target server IP
 * @param {number} options.serverPort - Target server port
 * @param {string} options.localIp - Local IP address (default: '0.0.0.0')
 * @param {number} options.localPort - Local port for UDP (default: 5060)
 * @param {number} options.timeout - Timeout in milliseconds (default: 10000)
 * @returns {Promise<Object>} Test results for UDP and TCP
 */
async function runAlgTest(options) {
    const {
        serverIp,
        serverPort = 5060,
        localIp = '0.0.0.0',
        localPort = 5060,
        timeout = 10000
    } = options;
    
    console.log(`[ALG Test] Starting test to ${serverIp}:${serverPort}`);
    
    try {
        // Run both tests in parallel
        const [udpResult, tcpResult] = await Promise.all([
            testUdp(localIp, localPort, serverIp, serverPort, timeout),
            testTcp(localIp, 0, serverIp, serverPort, timeout) // Use 0 for TCP to let OS assign port
        ]);
        
        console.log(`[ALG Test] UDP result: ${udpResult.text} (code: ${udpResult.code})`);
        console.log(`[ALG Test] TCP result: ${tcpResult.text} (code: ${tcpResult.code})`);
        
        // Determine overall return code
        // If either test shows ALG (code 2), return 2
        // If both are FALSE (code 1), return 1
        // Otherwise return 4 (failed)
        let returnCode = RESULT_CODES.FAILED.code;
        if (udpResult.code === RESULT_CODES.TRUE.code || tcpResult.code === RESULT_CODES.TRUE.code) {
            returnCode = RESULT_CODES.TRUE.code;
        } else if (udpResult.code === RESULT_CODES.FALSE.code && tcpResult.code === RESULT_CODES.FALSE.code) {
            returnCode = RESULT_CODES.FALSE.code;
        }
        
        return {
            udp: udpResult,
            tcp: tcpResult,
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
