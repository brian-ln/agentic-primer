/**
 * Example: Using Signal Hub Client in Node.js
 *
 * This demonstrates how to use the client in a Node.js environment
 * with ws WebSocket polyfill.
 *
 * To run:
 *   npm install ws
 *   node examples/node-usage.js
 */

import { SignalHubClient, generateBrowserAddress } from '../dist/index.js';
import { WebSocket } from 'ws';

// Polyfill WebSocket for Node.js
global.WebSocket = WebSocket;
global.crypto = crypto;

async function main() {
  console.log('Signal Hub Client - Node.js Example\n');

  // Create client
  const client = new SignalHubClient({
    url: process.env.SIGNAL_HUB_URL || 'ws://localhost:8787',
    jwt: process.env.SIGNAL_HUB_JWT || 'dev-token',
    autoReconnect: true,
    heartbeatInterval: 25000
  });

  // Set up event handlers
  client.on('connected', async (event) => {
    console.log('✓ Connected to Signal Hub');
    console.log(`  Session ID: ${event.sessionId}`);
    console.log(`  Server Version: ${event.serverVersion}`);
    console.log(`  Actor Identity: ${event.actorIdentity}\n`);

    try {
      // Register an actor
      const actorAddress = generateBrowserAddress('node-test');
      console.log(`Registering actor: ${actorAddress}`);

      await client.registerActor({
        address: actorAddress,
        capabilities: ['test', 'demo'],
        metadata: {
          runtime: 'node.js',
          version: process.version
        }
      });

      console.log('✓ Actor registered\n');

      // Send a test message
      console.log('Sending test message...');
      await client.send(
        actorAddress,
        '@(local/test-receiver)',
        'app:test',
        {
          message: 'Hello from Node.js!',
          timestamp: Date.now()
        }
      );

      console.log('✓ Message sent\n');

      // Wait a bit, then disconnect
      setTimeout(async () => {
        console.log('Disconnecting...');
        await client.disconnect();
        console.log('✓ Disconnected\n');
        process.exit(0);
      }, 2000);
    } catch (error) {
      console.error('✗ Error:', error.message);
      process.exit(1);
    }
  });

  client.on('message', (event) => {
    console.log(`\n📨 Received message:`);
    console.log(`  From: ${event.message.from}`);
    console.log(`  Type: ${event.message.type}`);
    console.log(`  Payload:`, JSON.stringify(event.message.payload, null, 2));
  });

  client.on('error', (event) => {
    console.error(`✗ Error: ${event.message}`);
    if (event.code) {
      console.error(`  Code: ${event.code}`);
    }
  });

  client.on('stateChange', (state) => {
    console.log(`State: ${state}`);
  });

  client.on('disconnected', () => {
    console.log('Disconnected from Signal Hub');
  });

  // Connect
  try {
    console.log('Connecting to Signal Hub...');
    await client.connect();
  } catch (error) {
    console.error('✗ Connection failed:', error.message);
    process.exit(1);
  }
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
