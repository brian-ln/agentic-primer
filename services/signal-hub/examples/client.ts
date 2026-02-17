/**
 * Signal Hub Client Example
 *
 * Demonstrates connecting to Signal Hub, registering an actor,
 * and sending/receiving messages.
 *
 * Usage:
 *   1. Start signal-hub: pnpm dev
 *   2. Run client: tsx examples/client.ts
 */

import WebSocket from 'ws';
import type { SharedMessage, CanonicalAddress } from '../src/types';

const SIGNAL_HUB_URL = 'ws://localhost:8787/ws';
const ACTOR_ADDRESS = '@(browser/example-client)' as CanonicalAddress;
const HUB_ADDRESS = '@(cloudflare/signal-hub)' as CanonicalAddress;

/**
 * Create a SharedMessage
 */
function createMessage(
  type: string,
  payload: unknown,
  from: CanonicalAddress,
  to: CanonicalAddress,
  pattern: 'tell' | 'ask' = 'tell',
  metadata: Record<string, unknown> = {}
): SharedMessage {
  return {
    id: crypto.randomUUID(),
    from,
    to,
    type,
    payload,
    pattern,
    correlationId: null,
    timestamp: Date.now(),
    metadata,
    ttl: null,
    signature: null,
  };
}

/**
 * Signal Hub Client
 */
class SignalHubClient {
  private ws: WebSocket;
  private sessionId: string | null = null;
  private connected = false;

  constructor(url: string) {
    this.ws = new WebSocket(url);
  }

  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      this.ws.on('open', () => {
        console.log('WebSocket connected');

        // Send hub:connect
        const connectMsg = createMessage(
          'hub:connect',
          null,
          ACTOR_ADDRESS,
          HUB_ADDRESS,
          'ask',
          {
            protocolVersion: '0.1.0',
            capabilities: ['send', 'broadcast', 'subscribe'],
          }
        );

        this.ws.send(JSON.stringify(connectMsg));
      });

      this.ws.on('message', (data: Buffer) => {
        const msg = JSON.parse(data.toString()) as SharedMessage;

        if (msg.type === 'hub:connected') {
          this.sessionId = (msg.payload as any).sessionId;
          this.connected = true;
          console.log(`Connected! Session ID: ${this.sessionId}`);
          resolve();
        } else {
          this.handleMessage(msg);
        }
      });

      this.ws.on('error', (err) => {
        console.error('WebSocket error:', err);
        reject(err);
      });

      this.ws.on('close', () => {
        console.log('WebSocket closed');
        this.connected = false;
      });
    });
  }

  async register(): Promise<void> {
    const registerMsg = createMessage(
      'hub:register',
      {
        actorAddress: ACTOR_ADDRESS,
        capabilities: ['render', 'compute'],
        metadata: { type: 'example-client' },
        ttlSeconds: 300,
      },
      ACTOR_ADDRESS,
      HUB_ADDRESS,
      'ask',
      { renewOnHeartbeat: true }
    );

    this.ws.send(JSON.stringify(registerMsg));
  }

  async discover(pattern: string): Promise<void> {
    const discoverMsg = createMessage(
      'hub:discover',
      { pattern, limit: 50 },
      ACTOR_ADDRESS,
      HUB_ADDRESS,
      'ask'
    );

    this.ws.send(JSON.stringify(discoverMsg));
  }

  async subscribe(topic: string): Promise<void> {
    const subscribeMsg = createMessage(
      'hub:subscribe',
      { topic, durable: false },
      ACTOR_ADDRESS,
      HUB_ADDRESS,
      'ask'
    );

    this.ws.send(JSON.stringify(subscribeMsg));
  }

  async publish(topic: string, messageType: string, data: unknown): Promise<void> {
    const publishMsg = createMessage(
      'hub:publish',
      { topic, type: messageType, data },
      ACTOR_ADDRESS,
      HUB_ADDRESS,
      'tell'
    );

    this.ws.send(JSON.stringify(publishMsg));
  }

  async send(
    toAddress: CanonicalAddress,
    messageType: string,
    data: unknown
  ): Promise<void> {
    const sendMsg = createMessage(
      'hub:send',
      { type: messageType, data },
      ACTOR_ADDRESS,
      toAddress,
      'ask',
      { via: HUB_ADDRESS, requireAck: true }
    );

    this.ws.send(JSON.stringify(sendMsg));
  }

  private handleMessage(msg: SharedMessage): void {
    console.log(`\n[${msg.type}]`, JSON.stringify(msg.payload, null, 2));

    // Handle specific message types
    switch (msg.type) {
      case 'hub:registered':
        console.log('Actor registered successfully');
        break;

      case 'hub:discovered':
        const actors = (msg.payload as any).actors;
        console.log(`Discovered ${actors.length} actors`);
        break;

      case 'hub:subscribed':
        console.log('Subscribed to topic:', (msg.payload as any).topic);
        break;

      case 'hub:delivery_ack':
        console.log('Message delivered:', (msg.payload as any).messageId);
        break;

      case 'hub:error':
        console.error('Hub error:', (msg.payload as any).message);
        break;
    }
  }

  async heartbeat(): Promise<void> {
    const heartbeatMsg = createMessage(
      'hub:heartbeat',
      { timestamp: Date.now() },
      ACTOR_ADDRESS,
      HUB_ADDRESS,
      'tell'
    );

    this.ws.send(JSON.stringify(heartbeatMsg));
  }

  async disconnect(): Promise<void> {
    const disconnectMsg = createMessage(
      'hub:disconnect',
      { reason: 'client_requested' },
      ACTOR_ADDRESS,
      HUB_ADDRESS,
      'tell'
    );

    this.ws.send(JSON.stringify(disconnectMsg));

    setTimeout(() => {
      this.ws.close();
    }, 100);
  }
}

/**
 * Main demo
 */
async function main() {
  console.log('Signal Hub Client Demo\n');

  const client = new SignalHubClient(SIGNAL_HUB_URL);

  try {
    // 1. Connect
    console.log('1. Connecting to Signal Hub...');
    await client.connect();
    await sleep(500);

    // 2. Register actor
    console.log('\n2. Registering actor...');
    await client.register();
    await sleep(500);

    // 3. Discover actors
    console.log('\n3. Discovering actors matching @(browser/*)...');
    await client.discover('@(browser/*)');
    await sleep(500);

    // 4. Subscribe to topic
    console.log('\n4. Subscribing to topic "events"...');
    await client.subscribe('events');
    await sleep(500);

    // 5. Publish to topic
    console.log('\n5. Publishing to topic "events"...');
    await client.publish('events', 'event:created', {
      eventId: '123',
      name: 'Test Event',
    });
    await sleep(500);

    // 6. Send heartbeat
    console.log('\n6. Sending heartbeat...');
    await client.heartbeat();
    await sleep(500);

    // 7. Disconnect
    console.log('\n7. Disconnecting...');
    await client.disconnect();
  } catch (err) {
    console.error('Error:', err);
    process.exit(1);
  }
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

// Run demo
main();
