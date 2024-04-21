#! /usr/bin/env node
const readline = require('readline');

// Create readline interface for reading from stdin
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});
const fs = require('fs/promises')

async function sendMessage(data) {
  // Serialize the message as JSON
  const jsonMessage = JSON.stringify(data);
  // Calculate the length of the JSON message
  const contentLength = Buffer.byteLength(jsonMessage, 'utf8');
  // Write the 'Content-Length' header and the JSON message to stdout
  const rpc = `Content-Length: ${contentLength}\r\n\r\n${jsonMessage}`;
  console.log(rpc)
  fs.appendFile('node-input.log', rpc, { encoding: 'utf8' })
}

process.stdin.on('data', async (line) => {
  try {
    // Parse incoming message from JSON
    const message = JSON.parse(line)
    
    await fs.appendFile('node-input.log', line, { encoding: 'utf8' })
  
    // Handle the message based on its type
    switch (message.method) {
      case 'initialize':
        // Handle 'textDocument/didOpen' message
        await sendMessage({ jsonrpc: '2.0', id: message.id, result: { capabilities: {} } })
        break;
    }
  } catch (_) {
    await fs.appendFile('node-input.log', line, { encoding: 'utf8' })
  }
});