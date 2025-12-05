#!/usr/bin/env node
/**
 * SIP ALG Client Tester
 * 
 * This is a standalone client application that tests SIP ALG from the client's PC
 * to the backend server. It performs the same SIP INVITE mirror test but runs
 * locally on the client machine instead of on the server.
 * 
 * Usage:
 *   node client-tester.js <server-ip> [port1,port2,...]
 *   
 * Example:
 *   node client-tester.js 193.105.36.15
 *   node client-tester.js 193.105.36.15 5060,5062
 */

const dgram = require('dgram');
const net = require('net');
const crypto = require('crypto');
const readline = require('readline');
const http = require('http');
const https = require('https');

// ANSI color codes for terminal output
const colors = {
    reset: '\x1b[0m',
    bright: '\x1b[1m',
    red: '\x1b[31m',
    green: '\x1b[32m',
    yellow: '\x1b[33m',
    blue: '\x1b[34m',
    magenta: '\x1b[35m',
    cyan: '\x1b[36m'
};

/**
 * Result codes
 */
const RESULT_CODES = {
    FALSE: { code: 1, text: 'FALSE' },      // No ALG detected
    TRUE: { code: 2, text: 'TRUE' },        // ALG detected
    DISABLED: { code: 3, text: 'disabled' },
    FAILED: { code: 4, text: 'test failed' }
};

/**
 * Generate random string
 */
function randomString(length) {
    return crypto.randomBytes(Math.ceil(length / 2))
        .toString('hex')
        .slice(0, length);
}

/**
 * Generate Call-ID
 */
function generateCallId() {
    return `${randomString(16)}@${randomString(8)}`;
}

/**
 * Generate branch parameter
 */
function generateBranch() {
    return `z9hG4bK-${randomString(16)}`;
}

/**
 * Generate tag
 */
function generateTag() {
    return randomString(10);
}

/**
 * Get local IP address
 */
function getLocalIp() {
    const os = require('os');
    const interfaces = os.networkInterfaces();
    
    for (const name of Object.keys(interfaces)) {
        for (const iface of interfaces[name]) {
            // Skip internal and non-IPv4 addresses
            if (iface.family === 'IPv4' && !iface.internal) {
                return iface.address;
            }
        }
    }
    
    return '0.0.0.0';
}

/**
 * Build SIP INVITE request
 */
function buildSipInvite(localIp, localPort, serverIp, serverPort, transport) {
    const callId = generateCallId();
    const branch = generateBranch();
    const fromTag = generateTag();
    const cseq = Math.floor(Math.random() * 1000) + 1;
    
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
 * Compare SIP messages
 */
function compareSipMessages(original, mirrored) {
    const differences = [];
    
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
 * Test UDP
 */
function testUdp(localIp, serverIp, serverPort, timeout = 10000) {
    return new Promise((resolve) => {
        const socket = dgram.createSocket('udp4');
        let timeoutHandle;
        let sipData = null;
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
        
        timeoutHandle = setTimeout(() => {
            resolveOnce({
                ...RESULT_CODES.FAILED,
                error: 'Timeout waiting for server response',
                port: serverPort,
                transport: 'udp'
            });
        }, timeout);
        
        socket.on('error', (err) => {
            resolveOnce({
                ...RESULT_CODES.FAILED,
                error: `Socket error: ${err.message}`,
                port: serverPort,
                transport: 'udp'
            });
        });
        
        socket.on('message', (msg, rinfo) => {
            try {
                const response = msg.toString('utf8');
                
                const bodyMatch = response.match(/\r\n\r\n(.+)$/s);
                if (!bodyMatch || !bodyMatch[1]) {
                    resolveOnce({
                        ...RESULT_CODES.FAILED,
                        error: 'Could not extract mirrored request from server response',
                        port: serverPort,
                        transport: 'udp'
                    });
                    return;
                }
                
                const mirroredRequest = bodyMatch[1].trim();
                
                if (!sipData) {
                    resolveOnce({
                        ...RESULT_CODES.FAILED,
                        error: 'Original SIP request data not available',
                        port: serverPort,
                        transport: 'udp'
                    });
                    return;
                }
                
                const differences = compareSipMessages(sipData.request, mirroredRequest);
                
                if (differences.length === 0) {
                    resolveOnce({
                        ...RESULT_CODES.FALSE,
                        port: serverPort,
                        transport: 'udp'
                    });
                } else {
                    resolveOnce({
                        ...RESULT_CODES.TRUE,
                        differences: differences.slice(0, 5),
                        port: serverPort,
                        transport: 'udp'
                    });
                }
            } catch (err) {
                resolveOnce({
                    ...RESULT_CODES.FAILED,
                    error: `Error processing response: ${err.message}`,
                    port: serverPort,
                    transport: 'udp'
                });
            }
        });
        
        try {
            socket.bind(0, '0.0.0.0', () => {
                try {
                    const localPort = socket.address().port;
                    sipData = buildSipInvite(localIp, localPort, serverIp, serverPort, 'UDP');
                    const buffer = Buffer.from(sipData.request, 'utf8');
                    
                    socket.send(buffer, 0, buffer.length, serverPort, serverIp, (err) => {
                        if (err) {
                            resolveOnce({
                                ...RESULT_CODES.FAILED,
                                error: `Failed to send request: ${err.message}`,
                                port: serverPort,
                                transport: 'udp'
                            });
                        }
                    });
                } catch (err) {
                    resolveOnce({
                        ...RESULT_CODES.FAILED,
                        error: `Error preparing request: ${err.message}`,
                        port: serverPort,
                        transport: 'udp'
                    });
                }
            });
        } catch (err) {
            resolveOnce({
                ...RESULT_CODES.FAILED,
                error: `Failed to bind socket: ${err.message}`,
                port: serverPort,
                transport: 'udp'
            });
        }
    });
}

/**
 * Test TCP
 */
function testTcp(localIp, serverIp, serverPort, timeout = 10000) {
    return new Promise((resolve) => {
        const socket = new net.Socket();
        let timeoutHandle;
        let receivedData = '';
        
        const cleanup = () => {
            clearTimeout(timeoutHandle);
            socket.destroy();
        };
        
        timeoutHandle = setTimeout(() => {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: 'Timeout waiting for server response',
                port: serverPort,
                transport: 'tcp'
            });
        }, timeout);
        
        socket.on('error', (err) => {
            cleanup();
            resolve({
                ...RESULT_CODES.FAILED,
                error: `Socket error: ${err.message}`,
                port: serverPort,
                transport: 'tcp'
            });
        });
        
        const sipData = buildSipInvite(localIp, 0, serverIp, serverPort, 'TCP');
        
        socket.on('data', (data) => {
            receivedData += data.toString('utf8');
            
            if (receivedData.includes('\r\n\r\n')) {
                cleanup();
                
                const bodyMatch = receivedData.match(/\r\n\r\n(.+)$/s);
                if (!bodyMatch || !bodyMatch[1]) {
                    resolve({
                        ...RESULT_CODES.FAILED,
                        error: 'Could not extract mirrored request from server response',
                        port: serverPort,
                        transport: 'tcp'
                    });
                    return;
                }
                
                const mirroredRequest = bodyMatch[1].trim();
                const differences = compareSipMessages(sipData.request, mirroredRequest);
                
                if (differences.length === 0) {
                    resolve({
                        ...RESULT_CODES.FALSE,
                        port: serverPort,
                        transport: 'tcp'
                    });
                } else {
                    resolve({
                        ...RESULT_CODES.TRUE,
                        differences: differences.slice(0, 5),
                        port: serverPort,
                        transport: 'tcp'
                    });
                }
            }
        });
        
        socket.connect(serverPort, serverIp, () => {
            socket.write(sipData.request);
        });
    });
}

/**
 * Print colored text
 */
function print(text, color = '') {
    console.log(`${color}${text}${colors.reset}`);
}

/**
 * Wait for user input (Windows CMD pause functionality)
 */
function waitForKeypress() {
    return new Promise((resolve) => {
        // Check if we're in an interactive terminal (not piped)
        if (!process.stdin.isTTY) {
            resolve();
            return;
        }
        
        print('\nPress any key to continue...', colors.cyan);
        
        try {
            process.stdin.setRawMode(true);
            process.stdin.resume();
            process.stdin.once('data', () => {
                try {
                    process.stdin.setRawMode(false);
                } catch (e) {
                    // Ignore errors when disabling raw mode
                }
                process.stdin.pause();
                resolve();
            });
        } catch (err) {
            // If setRawMode fails (e.g., on some platforms), fall back to readline
            console.log('(Press Enter to continue)');
            const rl = readline.createInterface({
                input: process.stdin,
                output: process.stdout
            });
            rl.once('line', () => {
                rl.close();
                resolve();
            });
        }
    });
}

/**
 * Prompt user for text input
 */
function promptUser(question) {
    return new Promise((resolve) => {
        const rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout,
            terminal: process.stdin.isTTY
        });
        
        rl.question(question, (answer) => {
            rl.close();
            resolve(answer.trim());
        });
    });
}

/**
 * Print results
 */
function printResults(results) {
    console.log('\n' + '='.repeat(70));
    print('  SIP ALG TEST RESULTS', colors.bright + colors.cyan);
    console.log('='.repeat(70) + '\n');
    
    let overallCode = RESULT_CODES.FALSE.code;
    let hasAnyFailed = false;
    
    for (const result of results) {
        const port = result.port;
        const transport = result.transport.toUpperCase();
        const status = result.text;
        
        let statusColor = colors.green;
        if (result.code === RESULT_CODES.TRUE.code) {
            statusColor = colors.red;
            overallCode = RESULT_CODES.TRUE.code;
        } else if (result.code === RESULT_CODES.FAILED.code) {
            statusColor = colors.yellow;
            hasAnyFailed = true;
        }
        
        print(`Port ${port} (${transport}):`, colors.bright);
        print(`  Status: ${status}`, statusColor);
        
        if (result.error) {
            print(`  Error: ${result.error}`, colors.yellow);
        }
        
        if (result.differences && result.differences.length > 0) {
            print('  Differences detected:', colors.yellow);
            for (const diff of result.differences) {
                print(`    Line ${diff.line}:`, colors.cyan);
                print(`      Original: ${diff.original}`, colors.reset);
                print(`      Mirrored:  ${diff.mirrored}`, colors.reset);
            }
        }
        
        console.log('');
    }
    
    if (overallCode !== RESULT_CODES.TRUE.code && hasAnyFailed) {
        overallCode = RESULT_CODES.FAILED.code;
    }
    
    console.log('='.repeat(70));
    print('  OVERALL RESULT', colors.bright);
    console.log('='.repeat(70));
    
    if (overallCode === RESULT_CODES.FALSE.code) {
        print('  ✓ No ALG Detected', colors.bright + colors.green);
        print('  Your router is not modifying SIP packets.', colors.green);
    } else if (overallCode === RESULT_CODES.TRUE.code) {
        print('  ✗ ALG Detected', colors.bright + colors.red);
        print('  Your router is modifying SIP packets!', colors.red);
        print('  This can cause VoIP issues. Consider disabling SIP ALG.', colors.yellow);
    } else {
        print('  ⚠ Test Failed', colors.bright + colors.yellow);
        print('  Could not complete the test. Check network/firewall.', colors.yellow);
    }
    
    console.log('='.repeat(70) + '\n');
}

/**
 * Send test results to server
 */
async function sendResultsToServer(serverIp, sessionId, results) {
    print('\nSending results to server...', colors.cyan);
    
    // Organize results by port (similar to server-side format)
    const portResults = {};
    for (const result of results) {
        const port = result.port;
        const transport = result.transport;
        
        if (!portResults[port]) {
            portResults[port] = { udp: null, tcp: null };
        }
        
        portResults[port][transport] = result;
    }
    
    // Determine overall return code
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
    
    const payload = JSON.stringify({
        sessionId: sessionId,
        result: {
            ports: portResults,
            returnCode: returnCode
        }
    });
    
    const options = {
        hostname: serverIp,
        port: 3000,
        path: '/api/test-result',
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Content-Length': Buffer.byteLength(payload)
        }
    };
    
    return new Promise((resolve, reject) => {
        // Use http for all cases (since we're always connecting to port 3000 which is http)
        const protocol = http;
        
        const req = protocol.request(options, (res) => {
            let data = '';
            
            res.on('data', (chunk) => {
                data += chunk;
            });
            
            res.on('end', () => {
                if (res.statusCode >= 200 && res.statusCode < 300) {
                    print('✓ Results sent successfully to web interface!', colors.green);
                    resolve();
                } else {
                    print(`⚠ Server responded with status ${res.statusCode}`, colors.yellow);
                    try {
                        const response = JSON.parse(data);
                        if (response.error) {
                            print(`  Error: ${response.error}`, colors.yellow);
                        }
                    } catch (e) {
                        // Ignore JSON parse errors
                    }
                    resolve(); // Still resolve to allow the program to continue
                }
            });
        });
        
        req.on('error', (error) => {
            print(`✗ Failed to send results to server: ${error.message}`, colors.red);
            print('  The test completed but results could not be reported.', colors.yellow);
            resolve(); // Still resolve to allow the program to continue
        });
        
        req.write(payload);
        req.end();
    });
}

/**
 * Generate random 4-digit session ID
 */
function generateSessionId() {
    return Math.floor(1000 + Math.random() * 9000).toString();
}

/**
 * Main function
 */
async function main() {
    const args = process.argv.slice(2);
    
    // Default server IP and port
    const DEFAULT_SERVER_IP = '193.105.36.15';
    const DEFAULT_WEB_PORT = '3000';
    
    // Check for --report argument
    let reportSessionId = null;
    let filteredArgs = [];
    for (let i = 0; i < args.length; i++) {
        if (args[i] === '--report' && i + 1 < args.length) {
            reportSessionId = args[i + 1];
            i++; // Skip the next argument as it's the session ID
        } else {
            filteredArgs.push(args[i]);
        }
    }
    
    if (filteredArgs[0] === '-h' || filteredArgs[0] === '--help') {
        console.log(`
${colors.bright}SIP ALG Client Tester${colors.reset}

${colors.cyan}Usage:${colors.reset}
  node client-tester.js [server-ip] [ports]
  node client-tester.js [server-ip] [ports] --report <report-id>

${colors.cyan}Arguments:${colors.reset}
  server-ip    IP address of the SIP test server (default: ${DEFAULT_SERVER_IP})
  ports        Comma-separated list of ports to test (default: 5060,5062)
  --report     Optional: Report ID from the web interface (will prompt if not provided)

${colors.cyan}Examples:${colors.reset}
  node client-tester.js
  node client-tester.js --report 1234
  node client-tester.js 193.105.36.15 --report 8492
  node client-tester.js 193.105.36.15 5060,5062 --report 8492

${colors.cyan}Description:${colors.reset}
  This tool tests if your router's SIP ALG is modifying SIP packets.
  It sends SIP INVITE requests from your PC to the test server and
  compares the mirrored response to detect any modifications.
  
  ${colors.yellow}IMPORTANT: You must first open the web interface to get a Report ID:${colors.reset}
  1. Open http://${DEFAULT_SERVER_IP}:${DEFAULT_WEB_PORT} in your browser
  2. Note the Report ID shown on the page (displayed in large numbers)
  3. Run this tool - you will be prompted to enter your Report ID
  4. Results will automatically appear in your browser

${colors.cyan}Requirements:${colors.reset}
  - Server must be running the SIP mirror service on the specified ports
  - Firewall must allow UDP/TCP traffic on test ports
  - Run this from the client PC you want to test
  - Browser must be open at the web interface to receive results
`);
        process.exit(0);
    }
    
    const serverIp = filteredArgs[0] || DEFAULT_SERVER_IP;
    const ports = filteredArgs[1] ? filteredArgs[1].split(',').map(p => parseInt(p.trim(), 10)) : [5060, 5062];
    const localIp = getLocalIp();
    
    // Prompt for Report ID if not provided via command line
    if (!reportSessionId) {
        print('\n' + '='.repeat(70), colors.cyan);
        print('  SIP ALG Client Tester', colors.bright + colors.cyan);
        print('='.repeat(70), colors.cyan);
        print('', colors.reset);
        print('Before starting the test, you need a Report ID from the web interface.', colors.yellow);
        print('', colors.reset);
        print(`  1. Open http://${serverIp}:${DEFAULT_WEB_PORT} in your browser`, colors.cyan);
        print('', colors.reset);
        print('  2. Look for the Report ID displayed on the page', colors.cyan);
        print('     (it will be shown as a large 4-digit number)', colors.cyan);
        print('', colors.reset);
        print('  3. Enter the Report ID below', colors.cyan);
        print('', colors.reset);
        
        // Prompt user for Report ID
        reportSessionId = await promptUser(colors.bright + colors.yellow + 'Enter your Report ID: ' + colors.reset);
        
        // Validate Report ID - ensure it's not empty after trimming
        if (!reportSessionId || !reportSessionId.trim()) {
            print('', colors.reset);
            print('ERROR: Report ID is required to run the test.', colors.red);
            print('', colors.reset);
            await waitForKeypress();
            process.exit(1);
        }
        
        // Update with trimmed value
        reportSessionId = reportSessionId.trim();
        
        print('', colors.reset);
        print(`Report ID "${reportSessionId}" received. Starting test...`, colors.green);
    }
    
    print('\nSIP ALG Client Tester', colors.bright + colors.cyan);
    print('='.repeat(70), colors.cyan);
    console.log('');
    print(`Local IP:  ${localIp}`, colors.bright);
    print(`Server IP: ${serverIp}`, colors.bright);
    print(`Test Ports: ${ports.join(', ')}`, colors.bright);
    console.log('');
    print(`Report ID: ${reportSessionId}`, colors.bright + colors.yellow);
    print('Results will be sent to the web interface.', colors.cyan);
    print(`Make sure your browser is open at: http://${serverIp}:${DEFAULT_WEB_PORT}`, colors.cyan);
    console.log('');
    print('Starting test...', colors.yellow);
    console.log('');
    
    const allTests = [];
    for (const port of ports) {
        allTests.push(
            testUdp(localIp, serverIp, port),
            testTcp(localIp, serverIp, port)
        );
    }
    
    try {
        const results = await Promise.all(allTests);
        printResults(results);
        
        // Always send results to server with the provided session ID
        await sendResultsToServer(serverIp, reportSessionId, results);
        
        // Wait for keypress before exiting (especially important for Windows .exe)
        await waitForKeypress();
        
        // Exit with appropriate code
        const hasAlg = results.some(r => r.code === RESULT_CODES.TRUE.code);
        const hasFailed = results.some(r => r.code === RESULT_CODES.FAILED.code);
        
        if (hasAlg) {
            process.exit(2);
        } else if (hasFailed) {
            process.exit(4);
        } else {
            process.exit(0);
        }
    } catch (error) {
        print(`\nError: ${error.message}`, colors.red);
        await waitForKeypress();
        process.exit(1);
    }
}

// Run if executed directly
if (require.main === module) {
    main();
}

module.exports = { testUdp, testTcp };
