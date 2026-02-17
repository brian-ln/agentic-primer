#!/usr/bin/env bun
/**
 * Signal Hub Integration Example
 *
 * Demonstrates how to integrate Signal Hub client with SEAG actor system.
 * Shows local actors communicating with browser actors via Signal Hub.
 *
 * Setup:
 * 1. Set environment variables:
 *    - SIGNAL_HUB_URL=wss://your-signal-hub.workers.dev
 *    - SIGNAL_HUB_JWT=your-jwt-token
 * 2. Run: bun run examples/signal-hub-integration.ts
 */

import { SignalHubClient } from '../src/messaging/signal-hub/client.ts';
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

const SIGNAL_HUB_URL = process.env.SIGNAL_HUB_URL || 'ws://localhost:8787';
const SIGNAL_HUB_JWT = process.env.SIGNAL_HUB_JWT || 'test-jwt-token';

// ---------------------------------------------------------------------------
// Example: Simple Send/Receive
// ---------------------------------------------------------------------------

async function simpleExample() {
  console.log('=== Simple Send/Receive Example ===\n');

  const client = new SignalHubClient({
    url: SIGNAL_HUB_URL,
    jwt: SIGNAL_HUB_JWT,
    protocolVersion: '0.1.0',
  });

  // Set up event handlers
  client.on('connected', (sessionId) => {
    console.log(`✓ Connected to Signal Hub (session: ${sessionId})`);
  });

  client.on('error', (error) => {
    console.error(`✗ Error:`, error.message);
  });

  client.on('message', (msg: SharedMessage) => {
    console.log(`\n← Received message:`, {
      from: msg.from,
      type: msg.type,
      payload: msg.payload,
    });
  });

  // Connect
  console.log(`Connecting to ${SIGNAL_HUB_URL}...`);
  await client.connect();

  // Register an actor
  console.log(`\nRegistering actor: @(local/example-actor)`);
  await client.registerActor(
    '@(local/example-actor)' as CanonicalAddress,
    ['compute', 'demo'],
    { version: '1.0.0', environment: 'example' }
  );
  console.log(`✓ Actor registered`);

  // Send a message
  console.log(`\nSending message to @(browser/example-widget)...`);
  client.send({
    to: '@(browser/example-widget)' as CanonicalAddress,
    type: 'demo:greeting',
    payload: { message: 'Hello from SEAG!', timestamp: Date.now() },
    from: '@(local/example-actor)' as CanonicalAddress,
  });
  console.log(`✓ Message sent`);

  // Wait for potential responses
  console.log(`\nListening for messages (10 seconds)...`);
  await new Promise((resolve) => setTimeout(resolve, 10000));

  // Clean up
  console.log(`\nDisconnecting...`);
  await client.disconnect();
  console.log(`✓ Disconnected\n`);
}

// ---------------------------------------------------------------------------
// Example: Multi-Actor Registration
// ---------------------------------------------------------------------------

async function multiActorExample() {
  console.log('=== Multi-Actor Registration Example ===\n');

  const client = new SignalHubClient({
    url: SIGNAL_HUB_URL,
    jwt: SIGNAL_HUB_JWT,
  });

  client.on('connected', (sessionId) => {
    console.log(`✓ Connected (session: ${sessionId})`);
  });

  client.on('actorRegistered', (address) => {
    console.log(`  ✓ Registered: ${address}`);
  });

  await client.connect();

  // Register multiple actors
  console.log(`\nRegistering multiple actors:`);
  const actors = [
    { address: '@(local/inference-engine)', capabilities: ['inference', 'ml'] },
    { address: '@(local/data-processor)', capabilities: ['compute', 'etl'] },
    { address: '@(local/event-logger)', capabilities: ['logging', 'storage'] },
  ];

  for (const actor of actors) {
    await client.registerActor(
      actor.address as CanonicalAddress,
      actor.capabilities
    );
  }

  // List registered actors
  const registered = client.getRegisteredActors();
  console.log(`\nTotal registered actors: ${registered.length}`);
  console.log(registered);

  await client.disconnect();
}

// ---------------------------------------------------------------------------
// Example: Request/Response Pattern
// ---------------------------------------------------------------------------

async function requestResponseExample() {
  console.log('=== Request/Response Pattern Example ===\n');

  const client = new SignalHubClient({
    url: SIGNAL_HUB_URL,
    jwt: SIGNAL_HUB_JWT,
  });

  await client.connect();

  await client.registerActor(
    '@(local/requester)' as CanonicalAddress,
    ['compute']
  );

  console.log(`Sending request with acknowledgment...`);
  try {
    const ack = await client.sendWithAck({
      to: '@(browser/responder)' as CanonicalAddress,
      type: 'demo:request',
      payload: { query: 'status', timestamp: Date.now() },
      from: '@(local/requester)' as CanonicalAddress,
    });

    console.log(`✓ Delivery acknowledged:`, ack.payload);
  } catch (error) {
    console.error(`✗ Request failed:`, error);
  }

  await client.disconnect();
}

// ---------------------------------------------------------------------------
// Example: Reconnection & Resilience
// ---------------------------------------------------------------------------

async function reconnectionExample() {
  console.log('=== Reconnection & Resilience Example ===\n');

  const client = new SignalHubClient({
    url: SIGNAL_HUB_URL,
    jwt: SIGNAL_HUB_JWT,
    reconnect: {
      enabled: true,
      maxAttempts: 5,
      initialDelay: 1000,
      maxDelay: 10000,
      multiplier: 2,
    },
    messageQueue: {
      enabled: true,
      maxSize: 100,
      defaultTtl: 30000,
    },
  });

  client.on('connected', (sessionId) => {
    console.log(`✓ Connected (session: ${sessionId})`);
  });

  client.on('disconnected', (reason) => {
    console.log(`✗ Disconnected: ${reason}`);
  });

  client.on('reconnecting', (attempt) => {
    console.log(`⟳ Reconnecting (attempt ${attempt})...`);
  });

  client.on('message', (msg) => {
    console.log(`← Message received: ${msg.type}`);
  });

  await client.connect();
  await client.registerActor('@(local/resilient-actor)' as CanonicalAddress, ['compute']);

  console.log(`\nClient is now connected and will auto-reconnect on disconnect.`);
  console.log(`Try stopping/restarting Signal Hub to test reconnection.`);
  console.log(`Running for 60 seconds...\n`);

  // Send periodic heartbeats to demonstrate connection persistence
  const interval = setInterval(() => {
    const state = client.getState();
    console.log(`[${new Date().toISOString()}] State: ${state}`);

    if (state === 'connected') {
      client.send({
        to: '@(browser/monitor)' as CanonicalAddress,
        type: 'demo:ping',
        payload: { timestamp: Date.now() },
        from: '@(local/resilient-actor)' as CanonicalAddress,
      });
    }
  }, 5000);

  await new Promise((resolve) => setTimeout(resolve, 60000));
  clearInterval(interval);

  await client.disconnect();
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║        Signal Hub Integration Examples                    ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  console.log(`Configuration:`);
  console.log(`  Signal Hub URL: ${SIGNAL_HUB_URL}`);
  console.log(`  JWT: ${SIGNAL_HUB_JWT.substring(0, 20)}...`);
  console.log('');

  const examples = [
    { name: 'Simple Send/Receive', fn: simpleExample },
    { name: 'Multi-Actor Registration', fn: multiActorExample },
    { name: 'Request/Response Pattern', fn: requestResponseExample },
    { name: 'Reconnection & Resilience', fn: reconnectionExample },
  ];

  console.log('Select an example to run:');
  examples.forEach((ex, i) => {
    console.log(`  ${i + 1}. ${ex.name}`);
  });
  console.log('  0. Run all examples\n');

  // For automation, default to example 1
  const selection = parseInt(process.argv[2] || '1', 10);

  if (selection === 0) {
    // Run all examples sequentially
    for (const example of examples) {
      await example.fn();
      await new Promise((resolve) => setTimeout(resolve, 2000)); // Pause between examples
    }
  } else if (selection >= 1 && selection <= examples.length) {
    await examples[selection - 1].fn();
  } else {
    console.error('Invalid selection');
    process.exit(1);
  }

  console.log('\n✓ Examples complete\n');
}

// Run if executed directly
if (import.meta.main) {
  main().catch(console.error);
}
