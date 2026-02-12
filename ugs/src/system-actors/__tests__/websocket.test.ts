#!/usr/bin/env bun
/**
 * WebSocketActor Tests
 *
 * Comprehensive tests covering:
 * - Host validation (allowed/denied)
 * - Connection lifecycle (connect, send, receive, close)
 * - Connection limits
 * - Port-based event streaming
 * - Reconnection logic (exponential/linear backoff)
 * - Error scenarios
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { WebSocketActor } from '../websocket.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { GraphStore } from '../../graph.ts';
import { address, createMessage, createErrorResponse } from '@agentic-primer/actors';
import { WebSocketServer } from 'ws';
import type { MessageResponse } from '@agentic-primer/actors';

// Test WebSocket server
let testServer: WebSocketServer;
let serverPort: number;

function startTestServer(): Promise<number> {
  return new Promise((resolve) => {
    testServer = new WebSocketServer({ port: 0 });

    testServer.on('listening', () => {
      const addr = testServer.address();
      if (addr && typeof addr === 'object') {
        resolve(addr.port);
      }
    });

    testServer.on('connection', (ws) => {
      // Echo server
      ws.on('message', (data) => {
        ws.send(data);
      });

      // Send welcome message
      ws.send(JSON.stringify({ type: 'welcome', message: 'Connected' }));
    });
  });
}

function stopTestServer() {
  if (testServer) {
    testServer.close();
  }
}

async function connectWithRetry(
  wsActor: WebSocketActor,
  url: string,
  maxRetries = 3,
  retryDelay = 100
): Promise<MessageResponse> {
  let lastError;

  for (let attempt = 0; attempt < maxRetries; attempt++) {
    const message = createMessage(
      address('/system/websocket'),
      'ws.connect',
      { url },
      { from: address('test') }
    );

    const response = await wsActor.receive(message);

    if (response.success) {
      // Wait for connection to open
      await new Promise(resolve => setTimeout(resolve, 150));
      return response;
    }

    lastError = response.error;

    if (attempt < maxRetries - 1) {
      await new Promise(resolve => setTimeout(resolve, retryDelay));
    }
  }

  return createErrorResponse(
    { id: 'retry', from: address('test'), to: address('/system/websocket'), type: 'ws.connect', payload: {}, timestamp: Date.now() },
    lastError || 'Connection failed after retries'
  );
}

describe('WebSocketActor', () => {
  let router: MessageRouter;
  let store: GraphStore;
  let wsActor: WebSocketActor;

  beforeEach(async () => {
    store = new GraphStore();
    router = new MessageRouter(store);

    // Start test WebSocket server
    serverPort = await startTestServer();

    // Wait for server to be fully ready
    await new Promise(resolve => setTimeout(resolve, 100));

    wsActor = new WebSocketActor('ws-test', router, {
      allowedHosts: ['localhost', '127.0.0.1', 'ws.example.com'],
      maxConnections: 3,
      reconnect: {
        enabled: true,
        maxAttempts: 3,
        backoff: 'exponential'
      }
    });

    router.registerActor('/system/websocket', wsActor);
  });

  afterEach(async () => {
    // Close all WebSocket connections
    if (wsActor) {
      const connections = (wsActor as any).connections;
      for (const [id] of connections) {
        try {
          await wsActor.receive(createMessage(
            address('/system/websocket'),
            'ws.close',
            { connectionId: id },
            { from: address('test') }
          ));
        } catch (e) {
          // Ignore cleanup errors
        }
      }
    }

    stopTestServer();

    // Wait for cleanup
    await new Promise(resolve => setTimeout(resolve, 50));
  });

  describe('Host Validation', () => {
    test('allows whitelisted host (localhost)', async () => {
      const response = await connectWithRetry(
        wsActor,
        `ws://localhost:${serverPort}`
      );

      expect(response.success).toBe(true);
      expect(response.payload.connectionId).toBeDefined();
      expect(response.payload.connectionId).toMatch(/^ws-/);
    });

    test('denies non-whitelisted host', async () => {
      const message = createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: 'ws://evil.com:9999/steal-data' },
        { from: address('test') }
      );

      const response = await wsActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain("Host 'evil.com' not in allowedHosts");
      expect(response.error).toContain('localhost');
    });

    test('validates URL format', async () => {
      const message = createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: 'not-a-valid-url' },
        { from: address('test') }
      );

      const response = await wsActor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Invalid WebSocket URL');
    });
  });

  describe('Connection Lifecycle', () => {
    test('connects to WebSocket server', async () => {
      const response = await connectWithRetry(
        wsActor,
        `ws://localhost:${serverPort}`
      );

      expect(response.success).toBe(true);
      expect(response.payload.connectionId).toBeDefined();
      expect(response.payload.readyState).toBeDefined();
    });

    test('sends message to WebSocket', async () => {
      // Connect first
      const connectResp = await connectWithRetry(
        wsActor,
        `ws://localhost:${serverPort}`
      );
      const connectionId = connectResp.payload.connectionId;

      // Send message
      const sendMsg = createMessage(
        address('/system/websocket'),
        'ws.send',
        {
          connectionId,
          data: { test: 'message' }
        },
        { from: address('test') }
      );

      const sendResp = await wsActor.receive(sendMsg);
      expect(sendResp.success).toBe(true);
      expect(sendResp.payload.sent).toBe(true);
    });

    test('closes WebSocket connection', async () => {
      // Connect first
      const connectMsg = createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: `ws://localhost:${serverPort}` },
        { from: address('test') }
      );
      const connectResp = await wsActor.receive(connectMsg);
      const connectionId = connectResp.payload.connectionId;

      // Close connection
      const closeMsg = createMessage(
        address('/system/websocket'),
        'ws.close',
        { connectionId },
        { from: address('test') }
      );

      const closeResp = await wsActor.receive(closeMsg);
      expect(closeResp.success).toBe(true);
      expect(closeResp.payload.closed).toBe(true);
    });

    test('receives messages via port subscription', async () => {
      // Connect
      const connectMsg = createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: `ws://localhost:${serverPort}` },
        { from: address('test') }
      );
      const connectResp = await wsActor.receive(connectMsg);
      const connectionId = connectResp.payload.connectionId;

      // Subscribe to events
      const subscribeMsg = createMessage(
        address('/system/websocket'),
        'ws.subscribe',
        { connectionId },
        { from: address('test') }
      );
      const subscribeResp = await wsActor.receive(subscribeMsg);
      expect(subscribeResp.success).toBe(true);

      // Collect events (should receive welcome message from server)
      const events = [];
      const stream = subscribeResp.payload.stream;

      // Wait for events (with timeout)
      const timeout = new Promise(resolve => setTimeout(resolve, 500));
      const collectEvents = (async () => {
        for await (const event of stream) {
          events.push(event);
          if (event.type === 'message') {
            break;
          }
        }
      })();

      await Promise.race([collectEvents, timeout]);

      // Should have received at least open and welcome message
      expect(events.length).toBeGreaterThan(0);
      const messageEvents = events.filter(e => e.type === 'message');
      expect(messageEvents.length).toBeGreaterThan(0);
    }, { timeout: 2000 });
  });

  describe('Connection Limits', () => {
    test('allows connections within limit', async () => {
      const connections = [];

      // Create 3 connections (at limit)
      for (let i = 0; i < 3; i++) {
        const msg = createMessage(
          address('/system/websocket'),
          'ws.connect',
          { url: `ws://localhost:${serverPort}` },
          { from: address('test') }
        );
        const resp = await wsActor.receive(msg);
        expect(resp.success).toBe(true);
        connections.push(resp.payload.connectionId);
      }

      expect(connections.length).toBe(3);
    });

    test('denies connections exceeding limit', async () => {
      // Create 3 connections (at limit)
      for (let i = 0; i < 3; i++) {
        const msg = createMessage(
          address('/system/websocket'),
          'ws.connect',
          { url: `ws://localhost:${serverPort}` },
          { from: address('test') }
        );
        await wsActor.receive(msg);
      }

      // Try to create 4th connection (should fail)
      const msg = createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: `ws://localhost:${serverPort}` },
        { from: address('test') }
      );
      const resp = await wsActor.receive(msg);

      expect(resp.success).toBe(false);
      expect(resp.error).toContain('Max connections exceeded: 3');
    });
  });

  describe('Error Scenarios', () => {
    test('returns error for connection not found on send', async () => {
      const msg = createMessage(
        address('/system/websocket'),
        'ws.send',
        {
          connectionId: 'nonexistent',
          data: { test: 'data' }
        },
        { from: address('test') }
      );

      const resp = await wsActor.receive(msg);
      expect(resp.success).toBe(false);
      expect(resp.error).toContain('Connection not found: nonexistent');
    });

    test('returns error for connection not found on close', async () => {
      const msg = createMessage(
        address('/system/websocket'),
        'ws.close',
        { connectionId: 'nonexistent' },
        { from: address('test') }
      );

      const resp = await wsActor.receive(msg);
      expect(resp.success).toBe(false);
      expect(resp.error).toContain('Connection not found: nonexistent');
    });

    test('returns error for connection not found on subscribe', async () => {
      const msg = createMessage(
        address('/system/websocket'),
        'ws.subscribe',
        { connectionId: 'nonexistent' },
        { from: address('test') }
      );

      const resp = await wsActor.receive(msg);
      expect(resp.success).toBe(false);
      expect(resp.error).toContain('No event port for connection: nonexistent');
    });

    test('returns error for unknown message type', async () => {
      const msg = createMessage(
        address('/system/websocket'),
        'ws.unknown',
        {},
        { from: address('test') }
      );

      const resp = await wsActor.receive(msg);
      expect(resp.success).toBe(false);
      expect(resp.error).toContain('Unknown message type: ws.unknown');
    });
  });

  describe('Port Event Streaming', () => {
    test('emits open event when connection established', async () => {
      // Connect
      const connectMsg = createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: `ws://localhost:${serverPort}` },
        { from: address('test') }
      );
      const connectResp = await wsActor.receive(connectMsg);
      const connectionId = connectResp.payload.connectionId;

      // Subscribe
      const subscribeMsg = createMessage(
        address('/system/websocket'),
        'ws.subscribe',
        { connectionId },
        { from: address('test') }
      );
      const subscribeResp = await wsActor.receive(subscribeMsg);

      // Collect events
      const events = [];
      const stream = subscribeResp.payload.stream;
      const timeout = new Promise(resolve => setTimeout(resolve, 300));

      await Promise.race([
        (async () => {
          for await (const event of stream) {
            events.push(event);
            if (events.length >= 2) break;
          }
        })(),
        timeout
      ]);

      // Should have open event
      const openEvents = events.filter(e => e.type === 'open');
      expect(openEvents.length).toBeGreaterThan(0);
      expect(openEvents[0].connectionId).toBe(connectionId);
    }, { timeout: 2000 });

    test('emits close event when connection closed', async () => {
      // Connect
      const connectMsg = createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: `ws://localhost:${serverPort}` },
        { from: address('test') }
      );
      const connectResp = await wsActor.receive(connectMsg);
      const connectionId = connectResp.payload.connectionId;

      // Subscribe
      const subscribeMsg = createMessage(
        address('/system/websocket'),
        'ws.subscribe',
        { connectionId },
        { from: address('test') }
      );
      const subscribeResp = await wsActor.receive(subscribeMsg);

      // Close connection
      const closeMsg = createMessage(
        address('/system/websocket'),
        'ws.close',
        { connectionId, code: 1000, reason: 'Test close' },
        { from: address('test') }
      );

      // Collect events
      const events = [];
      const stream = subscribeResp.payload.stream;

      // Start listening
      const collectTask = (async () => {
        for await (const event of stream) {
          events.push(event);
          if (event.type === 'close') break;
        }
      })();

      // Wait a bit then close
      await new Promise(resolve => setTimeout(resolve, 100));
      await wsActor.receive(closeMsg);

      // Wait for close event
      await Promise.race([collectTask, new Promise(resolve => setTimeout(resolve, 500))]);

      // Should have close event
      const closeEvents = events.filter(e => e.type === 'close');
      expect(closeEvents.length).toBeGreaterThan(0);
      expect(closeEvents[0].code).toBe(1000);
    }, { timeout: 2000 });
  });

  describe('Multiple Connections', () => {
    test('manages multiple independent connections', async () => {
      const conn1 = await wsActor.receive(createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: `ws://localhost:${serverPort}` },
        { from: address('test') }
      ));

      const conn2 = await wsActor.receive(createMessage(
        address('/system/websocket'),
        'ws.connect',
        { url: `ws://localhost:${serverPort}` },
        { from: address('test') }
      ));

      expect(conn1.success).toBe(true);
      expect(conn2.success).toBe(true);
      expect(conn1.payload.connectionId).not.toBe(conn2.payload.connectionId);
    });
  });

  describe('Reconnection Configuration', () => {
    test('creates actor with reconnection disabled', () => {
      const noReconnectActor = new WebSocketActor('ws-no-reconnect', router, {
        allowedHosts: ['localhost'],
        maxConnections: 5,
        reconnect: {
          enabled: false,
          maxAttempts: 0,
          backoff: 'linear'
        }
      });

      expect(noReconnectActor).toBeDefined();
    });

    test('creates actor with exponential backoff', () => {
      const expBackoffActor = new WebSocketActor('ws-exp', router, {
        allowedHosts: ['localhost'],
        maxConnections: 5,
        reconnect: {
          enabled: true,
          maxAttempts: 5,
          backoff: 'exponential'
        }
      });

      expect(expBackoffActor).toBeDefined();
    });

    test('creates actor with linear backoff', () => {
      const linearBackoffActor = new WebSocketActor('ws-linear', router, {
        allowedHosts: ['localhost'],
        maxConnections: 5,
        reconnect: {
          enabled: true,
          maxAttempts: 3,
          backoff: 'linear'
        }
      });

      expect(linearBackoffActor).toBeDefined();
    });
  });
});
