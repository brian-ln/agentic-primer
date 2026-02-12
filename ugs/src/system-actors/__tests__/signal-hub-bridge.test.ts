#!/usr/bin/env bun
/**
 * SignalHubBridgeActor Integration Tests
 *
 * Tests the bridge between simplify actors and brianln.ai Signal Hub:
 * - Outbound: emit-signal → HTTP POST to webhook
 * - Inbound: WebSocket notification → broadcast to subscribers
 * - Subscription management
 * - Connection lifecycle
 * - Reconnection logic
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { SignalHubBridgeActor } from '../signal-hub-bridge.ts';
import type { SignalHubBridgeConfig } from '../signal-hub-bridge.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { GraphStore } from '../../graph.ts';
import { Actor } from '../../messaging/actor.ts';
import { address, createMessage } from '@agentic-primer/actors';
import type { Message, MessageResponse, Address } from '@agentic-primer/actors';

// ---------------------------------------------------------------------------
// Mock webhook server (Bun.serve)
// ---------------------------------------------------------------------------

let webhookServer: ReturnType<typeof Bun.serve> | null = null;
let webhookRequests: { body: any; headers: Record<string, string> }[] = [];
let webhookResponseStatus = 202;
let webhookResponseBody: any = { status: 'accepted', signalId: 'test-signal-id' };

function startWebhookServer(): Promise<number> {
  webhookRequests = [];
  return new Promise((resolve) => {
    webhookServer = Bun.serve({
      port: 0,
      async fetch(req) {
        const body = await req.json();
        const headers: Record<string, string> = {};
        req.headers.forEach((value, key) => {
          headers[key] = value;
        });
        webhookRequests.push({ body, headers });

        return new Response(JSON.stringify(webhookResponseBody), {
          status: webhookResponseStatus,
          headers: { 'Content-Type': 'application/json' },
        });
      },
    });
    resolve(webhookServer.port);
  });
}

function stopWebhookServer() {
  if (webhookServer) {
    webhookServer.stop(true);
    webhookServer = null;
  }
}

// ---------------------------------------------------------------------------
// Mock WebSocket server (Bun.serve with websocket)
// ---------------------------------------------------------------------------

let wsServer: ReturnType<typeof Bun.serve> | null = null;
let wsClients: Set<any> = new Set();

function startWsServer(): Promise<number> {
  wsClients = new Set();
  return new Promise((resolve) => {
    wsServer = Bun.serve({
      port: 0,
      fetch(req, server) {
        if (server.upgrade(req)) {
          return;
        }
        return new Response('Expected WebSocket', { status: 400 });
      },
      websocket: {
        open(ws) {
          wsClients.add(ws);
        },
        message(ws, message) {
          // Echo for testing
          ws.send(message);
        },
        close(ws) {
          wsClients.delete(ws);
        },
      },
    });
    resolve(wsServer.port);
  });
}

function broadcastFromServer(data: any) {
  const json = JSON.stringify(data);
  for (const ws of wsClients) {
    ws.send(json);
  }
}

function stopWsServer() {
  if (wsServer) {
    wsServer.stop(true);
    wsServer = null;
  }
}

// ---------------------------------------------------------------------------
// Subscriber actor (receives notifications from bridge)
// ---------------------------------------------------------------------------

class TestSubscriberActor extends Actor {
  received: Message[] = [];

  constructor(id: string, router: MessageRouter) {
    super(id, router);
  }

  async receive(message: Message): Promise<MessageResponse> {
    this.received.push(message);
    return {
      id: crypto.randomUUID(),
      correlationId: message.id,
      from: this.address,
      to: message.from!,
      success: true,
      payload: { ack: true },
      timestamp: Date.now(),
    };
  }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('SignalHubBridgeActor', () => {
  let router: MessageRouter;
  let store: GraphStore;
  let bridge: SignalHubBridgeActor;
  let webhookPort: number;
  let wsPort: number;

  beforeEach(async () => {
    store = new GraphStore();
    router = new MessageRouter(store);

    webhookPort = await startWebhookServer();
    wsPort = await startWsServer();

    webhookResponseStatus = 202;
    webhookResponseBody = { status: 'accepted', signalId: 'test-signal-id' };

    const config: SignalHubBridgeConfig = {
      webhookUrl: `http://localhost:${webhookPort}/ingest/webhook`,
      wsUrl: `ws://localhost:${wsPort}`,
      authToken: 'test-token-123',
      autoConnect: false,
      reconnect: {
        enabled: false,
        maxAttempts: 3,
        baseDelayMs: 100,
      },
    };

    bridge = new SignalHubBridgeActor(router, config);
    router.registerActor('bridges/signal-hub', bridge);
  });

  afterEach(() => {
    bridge.stop();
    stopWebhookServer();
    stopWsServer();
  });

  // =========================================================================
  // Outbound: emit-signal → HTTP POST
  // =========================================================================

  describe('emit-signal (outbound HTTP POST)', () => {
    test('sends signal to webhook and returns signalId', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'emit-signal', {
        source: 'simplify',
        type: 'notification',
        sender: 'test-actor',
        subject: 'Test signal from simplify',
        body: 'Integration test payload',
        metadata: { test: true },
      }, { from: address('test'), pattern: 'ask' });

      const response = await bridge.receive(msg);

      expect(response.success).toBe(true);
      expect(response.payload.sent).toBe(true);
      expect(response.payload.signalId).toBe('test-signal-id');

      // Verify webhook received the correct payload
      expect(webhookRequests).toHaveLength(1);
      const req = webhookRequests[0];
      expect(req.body.source).toBe('simplify');
      expect(req.body.subject).toBe('Test signal from simplify');
      expect(req.body.sender).toBe('test-actor');
      expect(req.body.type).toBe('notification');
      expect(req.body.body).toBe('Integration test payload');
      expect(req.body.metadata.test).toBe(true);

      // Verify auth header
      expect(req.headers['authorization']).toBe('Bearer test-token-123');
    });

    test('returns error when webhook returns non-200', async () => {
      webhookResponseStatus = 500;
      webhookResponseBody = { error: 'Internal Server Error' };

      const msg = createMessage(address('bridges/signal-hub'), 'emit-signal', {
        source: 'github',
        type: 'alert',
        sender: 'ci',
        subject: 'Build failed',
      }, { from: address('test'), pattern: 'ask' });

      const response = await bridge.receive(msg);

      expect(response.success).toBe(false);
      expect(response.error).toContain('Webhook POST failed: 500');
    });

    test('returns error when signal missing required fields', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'emit-signal', {
        type: 'notification',
        sender: 'test',
        // missing source and subject
      }, { from: address('test'), pattern: 'ask' });

      const response = await bridge.receive(msg);

      expect(response.success).toBe(false);
      expect(response.error).toContain('source and subject');
    });

    test('defaults type and sender when not provided', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'emit-signal', {
        source: 'test-source',
        subject: 'Minimal signal',
      }, { from: address('test'), pattern: 'ask' });

      const response = await bridge.receive(msg);
      expect(response.success).toBe(true);

      const req = webhookRequests[0];
      expect(req.body.type).toBe('notification');
      expect(req.body.sender).toBe('simplify');
      expect(req.body.body).toBe('');
    });
  });

  // =========================================================================
  // Inbound: WebSocket notification → subscriber broadcast
  // =========================================================================

  describe('inbound WebSocket notifications', () => {
    test('broadcasts notification to subscribers', async () => {
      // Register a subscriber
      const subscriber = new TestSubscriberActor('test/subscriber', router);
      router.registerActor('test/subscriber', subscriber);

      // Subscribe to bridge
      const subMsg = createMessage(address('bridges/signal-hub'), 'subscribe', {}, {
        from: address('test/subscriber'),
        pattern: 'ask',
      });
      const subResponse = await bridge.receive(subMsg);
      expect(subResponse.success).toBe(true);
      expect(subResponse.payload.subscribed).toBe(true);

      // Connect WebSocket
      const connectMsg = createMessage(address('bridges/signal-hub'), 'connect', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      await bridge.receive(connectMsg);

      // Wait for WebSocket to connect
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Simulate Brain DO pushing a notification
      broadcastFromServer({
        type: 'new_signal',
        signal: {
          id: 'signal-001',
          source: 'github',
          sender: 'dependabot',
          subject: 'Security alert: lodash vulnerability',
          urgency: 'critical',
          category: 'security',
          timestamp: new Date().toISOString(),
        },
      });

      // Wait for message propagation
      await new Promise((resolve) => setTimeout(resolve, 200));

      // Subscriber should have received the notification
      expect(subscriber.received.length).toBeGreaterThanOrEqual(1);
      const notification = subscriber.received.find(
        (m) => m.type === 'signal-hub-notification'
      );
      expect(notification).toBeDefined();
      expect(notification!.payload.type).toBe('new_signal');
      expect(notification!.payload.signal.id).toBe('signal-001');
      expect(notification!.payload.signal.urgency).toBe('critical');
      expect(notification!.from).toBe(address('bridges/signal-hub'));
    });

    test('does not broadcast when no subscribers', async () => {
      // Connect WebSocket without subscribing anyone
      const connectMsg = createMessage(address('bridges/signal-hub'), 'connect', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      await bridge.receive(connectMsg);
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Push notification — should not throw
      broadcastFromServer({
        type: 'briefing_ready',
        date: '2026-02-08',
      });

      await new Promise((resolve) => setTimeout(resolve, 100));
      // No crash = pass
    });
  });

  // =========================================================================
  // Subscription management
  // =========================================================================

  describe('subscription management', () => {
    test('subscribe adds subscriber', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'subscribe', {}, {
        from: address('test/actor-1'),
        pattern: 'ask',
      });

      const response = await bridge.receive(msg);
      expect(response.success).toBe(true);
      expect(response.payload.subscribed).toBe(true);
      expect(response.payload.subscriberCount).toBe(1);
    });

    test('unsubscribe removes subscriber', async () => {
      // Subscribe first
      const subMsg = createMessage(address('bridges/signal-hub'), 'subscribe', {}, {
        from: address('test/actor-1'),
        pattern: 'ask',
      });
      await bridge.receive(subMsg);

      // Unsubscribe
      const unsubMsg = createMessage(address('bridges/signal-hub'), 'unsubscribe', {}, {
        from: address('test/actor-1'),
        pattern: 'ask',
      });
      const response = await bridge.receive(unsubMsg);
      expect(response.success).toBe(true);
      expect(response.payload.subscribed).toBe(false);
      expect(response.payload.subscriberCount).toBe(0);
    });

    test('multiple subscribers tracked correctly', async () => {
      for (const id of ['a', 'b', 'c']) {
        const msg = createMessage(address('bridges/signal-hub'), 'subscribe', {}, {
          from: address(`test/${id}`),
          pattern: 'ask',
        });
        await bridge.receive(msg);
      }

      const statusMsg = createMessage(address('bridges/signal-hub'), 'status', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      const response = await bridge.receive(statusMsg);
      expect(response.payload.subscriberCount).toBe(3);
    });

    test('subscribe without from address returns error', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'subscribe', {}, {
        pattern: 'ask',
        // No from address
      });
      // Remove from to simulate missing sender
      delete (msg as any).from;

      const response = await bridge.receive(msg);
      expect(response.success).toBe(false);
      expect(response.error).toContain('from address');
    });
  });

  // =========================================================================
  // Connection lifecycle
  // =========================================================================

  describe('connection lifecycle', () => {
    test('connect establishes WebSocket connection', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'connect', {}, {
        from: address('test'),
        pattern: 'ask',
      });

      const response = await bridge.receive(msg);

      // Wait for connection to establish
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Check status
      const statusMsg = createMessage(address('bridges/signal-hub'), 'status', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      const status = await bridge.receive(statusMsg);
      expect(status.payload.connected).toBe(true);
    });

    test('disconnect closes WebSocket connection', async () => {
      // Connect first
      const connectMsg = createMessage(address('bridges/signal-hub'), 'connect', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      await bridge.receive(connectMsg);
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Disconnect
      const disconnectMsg = createMessage(address('bridges/signal-hub'), 'disconnect', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      const response = await bridge.receive(disconnectMsg);
      expect(response.success).toBe(true);
      expect(response.payload.connected).toBe(false);
    });

    test('status reports current state', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'status', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      const response = await bridge.receive(msg);

      expect(response.success).toBe(true);
      expect(response.payload.connected).toBe(false);
      expect(response.payload.subscriberCount).toBe(0);
      expect(response.payload.webhookUrl).toContain('localhost');
      expect(response.payload.reconnectAttempts).toBe(0);
    });

    test('autoConnect connects on start', async () => {
      const autoConfig: SignalHubBridgeConfig = {
        webhookUrl: `http://localhost:${webhookPort}/ingest/webhook`,
        wsUrl: `ws://localhost:${wsPort}`,
        autoConnect: true,
        reconnect: { enabled: false, maxAttempts: 0, baseDelayMs: 100 },
      };

      const autoBridge = new SignalHubBridgeActor(router, autoConfig);
      await autoBridge.start();
      await new Promise((resolve) => setTimeout(resolve, 200));

      const statusMsg = createMessage(address('bridges/signal-hub'), 'status', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      const status = await autoBridge.receive(statusMsg);
      expect(status.payload.connected).toBe(true);

      await autoBridge.stop();
    });

    test('stop cleans up all resources', async () => {
      // Connect and subscribe
      const connectMsg = createMessage(address('bridges/signal-hub'), 'connect', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      await bridge.receive(connectMsg);
      await new Promise((resolve) => setTimeout(resolve, 100));

      const subMsg = createMessage(address('bridges/signal-hub'), 'subscribe', {}, {
        from: address('test/sub'),
        pattern: 'ask',
      });
      await bridge.receive(subMsg);

      // Stop should clean up
      await bridge.stop();

      const statusMsg = createMessage(address('bridges/signal-hub'), 'status', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      const status = await bridge.receive(statusMsg);
      expect(status.payload.connected).toBe(false);
      expect(status.payload.subscriberCount).toBe(0);
    });
  });

  // =========================================================================
  // Unknown message type
  // =========================================================================

  describe('error handling', () => {
    test('unknown message type returns error', async () => {
      const msg = createMessage(address('bridges/signal-hub'), 'unknown-type', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      const response = await bridge.receive(msg);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Unknown message type');
    });
  });

  // =========================================================================
  // End-to-end: simplify actor → webhook → Brain DO → WebSocket → subscriber
  // =========================================================================

  describe('end-to-end bridge flow', () => {
    test('outbound signal reaches webhook, inbound notification reaches subscriber', async () => {
      // 1. Register subscriber
      const subscriber = new TestSubscriberActor('test/e2e-subscriber', router);
      router.registerActor('test/e2e-subscriber', subscriber);

      const subMsg = createMessage(address('bridges/signal-hub'), 'subscribe', {}, {
        from: address('test/e2e-subscriber'),
        pattern: 'ask',
      });
      await bridge.receive(subMsg);

      // 2. Connect WebSocket
      const connectMsg = createMessage(address('bridges/signal-hub'), 'connect', {}, {
        from: address('test'),
        pattern: 'ask',
      });
      await bridge.receive(connectMsg);
      await new Promise((resolve) => setTimeout(resolve, 100));

      // 3. Emit signal outbound
      const emitMsg = createMessage(address('bridges/signal-hub'), 'emit-signal', {
        source: 'simplify',
        type: 'notification',
        sender: 'inference-engine',
        subject: 'Analysis complete: 42 patterns detected',
        body: 'Full analysis results available',
        metadata: { patternCount: 42, confidence: 0.95 },
      }, { from: address('test'), pattern: 'ask' });

      const emitResponse = await bridge.receive(emitMsg);
      expect(emitResponse.success).toBe(true);
      expect(emitResponse.payload.signalId).toBe('test-signal-id');

      // Verify webhook received it
      expect(webhookRequests).toHaveLength(1);
      expect(webhookRequests[0].body.subject).toBe('Analysis complete: 42 patterns detected');

      // 4. Simulate Brain DO pushing back a notification (as if it processed the signal)
      broadcastFromServer({
        type: 'new_signal',
        signal: {
          id: 'test-signal-id',
          source: 'simplify',
          sender: 'inference-engine',
          subject: 'Analysis complete: 42 patterns detected',
          urgency: 'high',
          category: 'analysis',
          timestamp: new Date().toISOString(),
        },
      });

      // 5. Wait for notification to propagate
      await new Promise((resolve) => setTimeout(resolve, 200));

      // 6. Verify subscriber received the inbound notification
      const notification = subscriber.received.find(
        (m) => m.type === 'signal-hub-notification'
      );
      expect(notification).toBeDefined();
      expect(notification!.payload.signal.id).toBe('test-signal-id');
      expect(notification!.payload.signal.source).toBe('simplify');
      expect(notification!.payload.signal.urgency).toBe('high');
    });
  });
});
