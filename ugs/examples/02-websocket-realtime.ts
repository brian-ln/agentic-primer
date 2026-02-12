#!/usr/bin/env bun
/**
 * Example 2: WebSocket Real-Time Updates
 *
 * Demonstrates WebSocketActor usage:
 * - Connect to WebSocket server
 * - Subscribe to events via ports
 * - Send and receive messages
 * - Handle connection lifecycle
 *
 * Run: bun run examples/02-websocket-realtime.ts
 */

import { Actor } from '../src/messaging/actor.ts';
import { MessageRouter } from '../src/messaging/router.ts';
import { GraphStore } from '../src/graph.ts';
import { address, type Message, type MessageResponse } from '../src/messaging/message.ts';
import { createResponse } from '../src/messaging/message.ts';
import { WebSocketActor } from '../src/system-actors/websocket.ts';
import { WebSocketServer } from 'ws';

// Demo WebSocket server
function startDemoServer(): Promise<number> {
  return new Promise((resolve) => {
    const wss = new WebSocketServer({ port: 0 });

    wss.on('listening', () => {
      const addr = wss.address();
      if (addr && typeof addr === 'object') {
        console.log(`Demo WebSocket server started on port ${addr.port}`);
        resolve(addr.port);
      }
    });

    wss.on('connection', (ws) => {
      console.log('[Server] Client connected');

      // Send welcome message
      ws.send(JSON.stringify({
        type: 'welcome',
        message: 'Connected to demo server'
      }));

      // Echo messages back
      ws.on('message', (data) => {
        const msg = JSON.parse(data.toString());
        console.log(`[Server] Received:`, msg);

        // Echo back with timestamp
        ws.send(JSON.stringify({
          type: 'echo',
          original: msg,
          timestamp: Date.now()
        }));
      });

      // Send periodic updates
      const interval = setInterval(() => {
        ws.send(JSON.stringify({
          type: 'update',
          data: `Server time: ${new Date().toISOString()}`
        }));
      }, 2000);

      ws.on('close', () => {
        console.log('[Server] Client disconnected');
        clearInterval(interval);
      });
    });
  });
}

// Real-time data processor actor
class RealtimeProcessor extends Actor {
  private connectionId?: string;
  private messageCount = 0;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'connect') {
      console.log('\n[Processor] Connecting to WebSocket...');

      // Connect to WebSocket
      const connectResp = await this.ask(
        address('/system/websocket'),
        'ws.connect',
        { url: message.payload.url }
      );

      if (!connectResp.success) {
        console.error('Connection failed:', connectResp.error);
        return createResponse(message, { error: connectResp.error });
      }

      this.connectionId = connectResp.payload.connectionId;
      console.log(`✓ Connected (ID: ${this.connectionId})`);

      // Subscribe to events
      const subscribeResp = await this.ask(
        address('/system/websocket'),
        'ws.subscribe',
        { connectionId: this.connectionId }
      );

      // Start listening to events
      this.listenToEvents(subscribeResp.payload.stream);

      // Send initial message
      await this.tell(
        address('/system/websocket'),
        'ws.send',
        {
          connectionId: this.connectionId,
          data: { action: 'subscribe', channel: 'updates' }
        }
      );

      return createResponse(message, { connected: true });
    }

    if (message.type === 'send-message') {
      if (!this.connectionId) {
        return createResponse(message, { error: 'Not connected' });
      }

      await this.tell(
        address('/system/websocket'),
        'ws.send',
        {
          connectionId: this.connectionId,
          data: message.payload.data
        }
      );

      return createResponse(message, { sent: true });
    }

    if (message.type === 'disconnect') {
      if (this.connectionId) {
        await this.tell(
          address('/system/websocket'),
          'ws.close',
          { connectionId: this.connectionId }
        );
        console.log('\n[Processor] Disconnected');
      }

      return createResponse(message, { disconnected: true });
    }

    return createResponse(message, { error: 'Unknown message type' });
  }

  private async listenToEvents(stream: AsyncIterator<any>): Promise<void> {
    for await (const event of stream) {
      if (event.type === 'open') {
        console.log('\n[Processor] WebSocket connection opened');
      }

      if (event.type === 'message') {
        this.messageCount++;
        console.log(`\n[Processor] Message #${this.messageCount}:`, event.data);
      }

      if (event.type === 'error') {
        console.error('\n[Processor] WebSocket error:', event.error);
      }

      if (event.type === 'close') {
        console.log(`\n[Processor] Connection closed (code: ${event.code})`);
        break;
      }
    }
  }
}

// Main execution
async function main() {
  console.log('=== WebSocket Real-Time Example ===\n');

  // Start demo server
  const port = await startDemoServer();

  // Setup
  const store = new GraphStore();
  const router = new MessageRouter(store);

  // Register WebSocket actor
  const wsActor = new WebSocketActor('ws', router, {
    allowedHosts: ['localhost', '127.0.0.1'],
    maxConnections: 5,
    reconnect: {
      enabled: true,
      maxAttempts: 3,
      backoff: 'exponential'
    }
  });
  router.registerActor('/system/websocket', wsActor);

  // Register processor
  const processor = new RealtimeProcessor('processor', router);
  router.registerActor('/realtime/processor', processor);

  // Connect and start receiving updates
  await processor.ask(
    address('/realtime/processor'),
    'connect',
    { url: `ws://localhost:${port}` }
  );

  // Wait for a few messages
  await new Promise(resolve => setTimeout(resolve, 3000));

  // Send a custom message
  console.log('\n[Main] Sending custom message...');
  await processor.ask(
    address('/realtime/processor'),
    'send-message',
    {
      data: {
        type: 'custom',
        message: 'Hello from client!',
        timestamp: Date.now()
      }
    }
  );

  // Wait for echo
  await new Promise(resolve => setTimeout(resolve, 2000));

  // Disconnect
  await processor.ask(
    address('/realtime/processor'),
    'disconnect',
    {}
  );

  console.log('\n=== Example Complete ===');
  process.exit(0);
}

main().catch(console.error);
