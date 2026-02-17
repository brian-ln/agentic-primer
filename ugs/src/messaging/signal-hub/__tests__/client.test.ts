#!/usr/bin/env bun
/**
 * SignalHubClient Tests
 *
 * Tests Signal Hub protocol implementation:
 * - Connection lifecycle (hub:connect, hub:connected, hub:disconnect)
 * - Actor registration (hub:register, hub:registered)
 * - Message routing (hub:send)
 * - Heartbeat (hub:heartbeat, hub:heartbeat_ack)
 * - Reconnection with exponential backoff
 * - Message queueing during disconnect
 */

import { describe, test, expect, beforeEach, afterEach, mock } from 'bun:test';
import { SignalHubClient } from '../client.ts';
import type { SignalHubConfig } from '../types.ts';
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';

// ---------------------------------------------------------------------------
// Mock WebSocket Server
// ---------------------------------------------------------------------------

let wsServer: ReturnType<typeof Bun.serve> | null = null;
let wsClients: Set<any> = new Set();
let receivedMessages: SharedMessage[] = [];

function startMockSignalHub(): Promise<number> {
  wsClients = new Set();
  receivedMessages = [];

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
          const msg = JSON.parse(message as string) as SharedMessage;
          receivedMessages.push(msg);

          // Handle protocol messages
          handleProtocolMessage(ws, msg);
        },
        close(ws) {
          wsClients.delete(ws);
        },
      },
    });
    resolve(wsServer.port);
  });
}

function stopMockSignalHub() {
  if (wsServer) {
    wsServer.stop(true);
    wsServer = null;
  }
}

function handleProtocolMessage(ws: any, msg: SharedMessage): void {
  switch (msg.type) {
    case 'hub:connect':
      // Respond with hub:connected
      const connectedMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: '@(cloudflare/signal-hub)' as CanonicalAddress,
        to: msg.from,
        type: 'hub:connected',
        payload: null,
        pattern: 'tell',
        correlationId: msg.id,
        timestamp: Date.now(),
        metadata: {
          sessionId: 'test-session-123',
          serverVersion: '0.1.0',
          maxMessageSize: 1048576,
          heartbeatInterval: 25000,
          capabilities: {
            maxActorsPerInstance: 50000,
            supportsBackpressure: true,
            supportedContentTypes: ['application/json'],
          },
        },
        ttl: null,
        signature: null,
      };
      ws.send(JSON.stringify(connectedMsg));
      break;

    case 'hub:register':
      // Respond with hub:registered
      const registeredMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: '@(cloudflare/signal-hub)' as CanonicalAddress,
        to: msg.from,
        type: 'hub:registered',
        payload: null,
        pattern: 'tell',
        correlationId: msg.id,
        timestamp: Date.now(),
        metadata: {
          expiresAt: Date.now() + 300000,
          renewalToken: 'renewal-token-123',
          version: 1,
        },
        ttl: null,
        signature: null,
      };
      ws.send(JSON.stringify(registeredMsg));
      break;

    case 'hub:heartbeat':
      // Respond with hub:heartbeat_ack
      const ackMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: '@(cloudflare/signal-hub)' as CanonicalAddress,
        to: msg.from,
        type: 'hub:heartbeat_ack',
        payload: null,
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        metadata: {},
        ttl: null,
        signature: null,
      };
      ws.send(JSON.stringify(ackMsg));
      break;

    case 'hub:send':
      // Echo back delivery_ack if ask pattern
      if (msg.pattern === 'ask') {
        const deliveryAckMsg: SharedMessage = {
          id: crypto.randomUUID(),
          from: '@(cloudflare/signal-hub)' as CanonicalAddress,
          to: msg.from,
          type: 'hub:delivery_ack',
          payload: { delivered: true },
          pattern: 'tell',
          correlationId: msg.id,
          timestamp: Date.now(),
          metadata: {},
          ttl: null,
          signature: null,
        };
        ws.send(JSON.stringify(deliveryAckMsg));
      }
      break;

    case 'hub:disconnect':
      // Close connection
      ws.close(1000, 'Client disconnect');
      break;
  }
}

function sendMessageToClient(msg: SharedMessage): void {
  for (const ws of wsClients) {
    ws.send(JSON.stringify(msg));
  }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('SignalHubClient', () => {
  let port: number;
  let config: SignalHubConfig;

  beforeEach(async () => {
    port = await startMockSignalHub();
    config = {
      url: `ws://localhost:${port}`,
      jwt: 'test-jwt-token',
      protocolVersion: '0.1.0',
      heartbeatInterval: 1000, // Short interval for testing
      reconnect: {
        enabled: false, // Disable reconnect for most tests
        maxAttempts: 3,
        initialDelay: 100,
        maxDelay: 1000,
        multiplier: 2,
      },
      messageQueue: {
        enabled: true,
        maxSize: 100,
        defaultTtl: 5000,
      },
    };
  });

  afterEach(() => {
    stopMockSignalHub();
  });

  // ---------------------------------------------------------------------------
  // Connection Lifecycle
  // ---------------------------------------------------------------------------

  describe('connection lifecycle', () => {
    test('connect sends hub:connect and receives hub:connected', async () => {
      const client = new SignalHubClient(config);

      const connectedPromise = new Promise<string>((resolve) => {
        client.on('connected', resolve);
      });

      await client.connect();
      const sessionId = await connectedPromise;

      expect(sessionId).toBe('test-session-123');
      expect(client.getState()).toBe('connected');
      expect(client.getSessionId()).toBe('test-session-123');

      // Verify hub:connect was sent
      const connectMsg = receivedMessages.find((m) => m.type === 'hub:connect');
      expect(connectMsg).toBeDefined();
      expect(connectMsg!.metadata.protocolVersion).toBe('0.1.0');
      expect(connectMsg!.metadata.authToken).toBe('test-jwt-token');

      await client.disconnect();
    });

    test('disconnect sends hub:disconnect and closes WebSocket', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      const disconnectedPromise = new Promise<string>((resolve) => {
        client.on('disconnected', resolve);
      });

      await client.disconnect();
      const reason = await disconnectedPromise;

      expect(client.getState()).toBe('disconnected');
      expect(reason).toContain('disconnect');

      // Verify hub:disconnect was sent
      const disconnectMsg = receivedMessages.find((m) => m.type === 'hub:disconnect');
      expect(disconnectMsg).toBeDefined();
    });

    test('connection info populated after connect', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      const connectionInfo = client.getConnectionInfo();
      expect(connectionInfo).toBeDefined();
      expect(connectionInfo!.sessionId).toBe('test-session-123');
      expect(connectionInfo!.serverVersion).toBe('0.1.0');
      expect(connectionInfo!.maxMessageSize).toBe(1048576);
      expect(connectionInfo!.heartbeatInterval).toBe(25000);

      await client.disconnect();
    });
  });

  // ---------------------------------------------------------------------------
  // Actor Registration
  // ---------------------------------------------------------------------------

  describe('actor registration', () => {
    test('registerActor sends hub:register and receives hub:registered', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      const registeredPromise = new Promise<CanonicalAddress>((resolve) => {
        client.on('actorRegistered', resolve);
      });

      await client.registerActor(
        '@(local/test-actor)' as CanonicalAddress,
        ['compute', 'inference'],
        { version: '1.0.0' }
      );

      const actorAddress = await registeredPromise;
      expect(actorAddress).toBe('@(local/test-actor)');

      // Verify hub:register was sent
      const registerMsg = receivedMessages.find((m) => m.type === 'hub:register');
      expect(registerMsg).toBeDefined();
      expect(registerMsg!.from).toBe('@(local/test-actor)');
      expect(registerMsg!.metadata.capabilities).toEqual(['compute', 'inference']);
      expect(registerMsg!.metadata.metadata).toEqual({ version: '1.0.0' });

      // Verify actor is in registered actors list
      const registeredActors = client.getRegisteredActors();
      expect(registeredActors).toContain('@(local/test-actor)');

      await client.disconnect();
    });

    test('unregisterActor removes actor from registry', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      await client.registerActor('@(local/test-actor)' as CanonicalAddress, ['compute']);

      const unregisteredPromise = new Promise<CanonicalAddress>((resolve) => {
        client.on('actorUnregistered', resolve);
      });

      await client.unregisterActor('@(local/test-actor)' as CanonicalAddress);

      const actorAddress = await unregisteredPromise;
      expect(actorAddress).toBe('@(local/test-actor)');

      // Verify actor removed from registry
      const registeredActors = client.getRegisteredActors();
      expect(registeredActors).not.toContain('@(local/test-actor)');

      await client.disconnect();
    });

    test('registerActor fails when not connected', async () => {
      const client = new SignalHubClient(config);

      await expect(
        client.registerActor('@(local/test-actor)' as CanonicalAddress, ['compute'])
      ).rejects.toThrow('not connected');
    });
  });

  // ---------------------------------------------------------------------------
  // Message Routing
  // ---------------------------------------------------------------------------

  describe('message routing', () => {
    test('send routes message via hub:send', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      client.send({
        to: '@(browser/widget-123)' as CanonicalAddress,
        type: 'app:message',
        payload: { data: 'hello world' },
        from: '@(local/test-actor)' as CanonicalAddress,
      });

      // Wait for message to be sent
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Verify hub:send was sent
      const sendMsg = receivedMessages.find((m) => m.type === 'hub:send');
      expect(sendMsg).toBeDefined();
      expect(sendMsg!.payload).toEqual({
        to: '@(browser/widget-123)',
        type: 'app:message',
        data: { data: 'hello world' },
      });
      expect(sendMsg!.from).toBe('@(local/test-actor)');

      await client.disconnect();
    });

    test('sendWithAck waits for delivery acknowledgment', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      const response = await client.sendWithAck({
        to: '@(browser/widget-123)' as CanonicalAddress,
        type: 'app:request',
        payload: { query: 'status' },
        from: '@(local/test-actor)' as CanonicalAddress,
      });

      expect(response.type).toBe('hub:delivery_ack');
      expect(response.payload).toEqual({ delivered: true });

      await client.disconnect();
    });

    test('incoming message triggers message event', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      const messagePromise = new Promise<SharedMessage>((resolve) => {
        client.on('message', resolve);
      });

      // Simulate Signal Hub sending a message
      const incomingMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: '@(browser/widget-123)' as CanonicalAddress,
        to: '@(local/test-actor)' as CanonicalAddress,
        type: 'app:notification',
        payload: { event: 'update' },
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        metadata: {},
        ttl: null,
        signature: null,
      };
      sendMessageToClient(incomingMsg);

      const receivedMsg = await messagePromise;
      expect(receivedMsg.type).toBe('app:notification');
      expect(receivedMsg.from).toBe('@(browser/widget-123)');
      expect(receivedMsg.payload).toEqual({ event: 'update' });

      await client.disconnect();
    });
  });

  // ---------------------------------------------------------------------------
  // Heartbeat
  // ---------------------------------------------------------------------------

  describe('heartbeat', () => {
    test('sends hub:heartbeat at configured interval', async () => {
      const client = new SignalHubClient({
        ...config,
        heartbeatInterval: 500, // 500ms for testing
      });

      await client.connect();

      // Clear received messages
      receivedMessages = [];

      // Wait for at least 2 heartbeats
      await new Promise((resolve) => setTimeout(resolve, 1200));

      // Verify heartbeats were sent
      const heartbeats = receivedMessages.filter((m) => m.type === 'hub:heartbeat');
      expect(heartbeats.length).toBeGreaterThanOrEqual(2);

      await client.disconnect();
    });

    test('renews actor registrations with heartbeat', async () => {
      const client = new SignalHubClient({
        ...config,
        heartbeatInterval: 500,
      });

      await client.connect();
      await client.registerActor('@(local/test-actor)' as CanonicalAddress, ['compute']);

      // Clear received messages
      receivedMessages = [];

      // Wait for heartbeat cycle
      await new Promise((resolve) => setTimeout(resolve, 600));

      // Verify hub:renew was sent
      const renewMsg = receivedMessages.find((m) => m.type === 'hub:renew');
      expect(renewMsg).toBeDefined();
      expect(renewMsg!.from).toBe('@(local/test-actor)');

      await client.disconnect();
    });
  });

  // ---------------------------------------------------------------------------
  // Message Queue
  // ---------------------------------------------------------------------------

  describe('message queue', () => {
    test('queues messages when disconnected', async () => {
      const client = new SignalHubClient(config);

      // Send message while disconnected
      client.send({
        to: '@(browser/widget-123)' as CanonicalAddress,
        type: 'app:message',
        payload: { queued: true },
      });

      // Connect and wait for flush
      await client.connect();
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Verify message was sent after connection
      const sendMsg = receivedMessages.find((m) => m.type === 'hub:send');
      expect(sendMsg).toBeDefined();
      expect(sendMsg!.payload).toEqual({
        to: '@(browser/widget-123)',
        type: 'app:message',
        data: { queued: true },
      });

      await client.disconnect();
    });

    test('drops expired messages from queue', async () => {
      const client = new SignalHubClient({
        ...config,
        messageQueue: {
          enabled: true,
          maxSize: 100,
          defaultTtl: 100, // Very short TTL
        },
      });

      // Send message while disconnected
      client.send({
        to: '@(browser/widget-123)' as CanonicalAddress,
        type: 'app:message',
        payload: { expired: true },
      });

      // Wait for message to expire
      await new Promise((resolve) => setTimeout(resolve, 200));

      // Connect (should not send expired message)
      await client.connect();
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Verify expired message was not sent
      const sendMsg = receivedMessages.find(
        (m) => m.type === 'hub:send' && (m.payload as any).data?.expired
      );
      expect(sendMsg).toBeUndefined();

      await client.disconnect();
    });
  });

  // ---------------------------------------------------------------------------
  // Reconnection
  // ---------------------------------------------------------------------------

  describe('reconnection', () => {
    test('automatically reconnects after disconnect', async () => {
      const client = new SignalHubClient({
        ...config,
        reconnect: {
          enabled: true,
          maxAttempts: 3,
          initialDelay: 100,
          maxDelay: 1000,
          multiplier: 2,
        },
      });

      await client.connect();

      let reconnectAttempted = false;
      client.on('reconnecting', () => {
        reconnectAttempted = true;
      });

      let reconnected = false;
      let reconnectCount = 0;
      client.on('connected', () => {
        reconnectCount++;
        if (reconnectCount > 1) {
          reconnected = true;
        }
      });

      // Simulate server closing connection
      for (const ws of wsClients) {
        ws.close(1006, 'Abnormal closure');
      }

      // Wait for reconnection to complete
      await new Promise((resolve) => setTimeout(resolve, 500));

      expect(reconnectAttempted).toBe(true);
      expect(reconnected).toBe(true);
      expect(client.getState()).toBe('connected');

      await client.disconnect();
    });

    test('re-registers actors after reconnection', async () => {
      const client = new SignalHubClient({
        ...config,
        reconnect: {
          enabled: true,
          maxAttempts: 3,
          initialDelay: 100,
          maxDelay: 1000,
          multiplier: 2,
        },
      });

      await client.connect();
      await client.registerActor('@(local/test-actor)' as CanonicalAddress, ['compute']);

      let reregistered = false;
      let registerCount = 0;
      client.on('actorRegistered', () => {
        registerCount++;
        if (registerCount > 1) {
          reregistered = true;
        }
      });

      // Clear received messages
      receivedMessages = [];

      // Simulate server closing connection
      for (const ws of wsClients) {
        ws.close(1006, 'Abnormal closure');
      }

      // Wait for reconnection and re-registration
      await new Promise((resolve) => setTimeout(resolve, 500));

      expect(reregistered).toBe(true);

      // Verify hub:register was sent again after reconnect
      const registerMsg = receivedMessages.find((m) => m.type === 'hub:register');
      expect(registerMsg).toBeDefined();

      await client.disconnect();
    });

    test('exponential backoff between reconnect attempts', async () => {
      const client = new SignalHubClient({
        ...config,
        reconnect: {
          enabled: true,
          maxAttempts: 5,
          initialDelay: 100,
          maxDelay: 1000,
          multiplier: 2,
        },
      });

      const attempts: number[] = [];
      const attemptTimes: number[] = [];

      client.on('reconnecting', (attempt) => {
        attempts.push(attempt);
        attemptTimes.push(Date.now());
      });

      // Simulate failed connections by stopping the server
      stopMockSignalHub();

      await client.connect().catch(() => {
        // Expected to fail
      });

      // Wait for multiple reconnect attempts
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Verify exponential backoff
      expect(attempts.length).toBeGreaterThanOrEqual(2);

      if (attemptTimes.length >= 3) {
        const delay1 = attemptTimes[1] - attemptTimes[0];
        const delay2 = attemptTimes[2] - attemptTimes[1];

        // Second delay should be roughly 2x the first (exponential)
        expect(delay2).toBeGreaterThan(delay1);
      }
    });
  });

  // ---------------------------------------------------------------------------
  // Error Handling
  // ---------------------------------------------------------------------------

  describe('error handling', () => {
    test('emits error event on hub:error', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      const errorPromise = new Promise<Error>((resolve) => {
        client.on('error', resolve);
      });

      // Simulate Signal Hub sending error
      const errorMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: '@(cloudflare/signal-hub)' as CanonicalAddress,
        to: '@(local/signal-hub-client)' as CanonicalAddress,
        type: 'hub:error',
        payload: 'Test error message',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        metadata: { code: 'test_error' },
        ttl: null,
        signature: null,
      };
      sendMessageToClient(errorMsg);

      const error = await errorPromise;
      expect(error.message).toContain('Test error message');

      await client.disconnect();
    });

    test('disconnects on hub:unauthorized', async () => {
      const client = new SignalHubClient(config);
      await client.connect();

      const disconnectedPromise = new Promise<string>((resolve) => {
        client.on('disconnected', resolve);
      });

      // Simulate unauthorized error
      const unauthorizedMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: '@(cloudflare/signal-hub)' as CanonicalAddress,
        to: '@(local/signal-hub-client)' as CanonicalAddress,
        type: 'hub:unauthorized',
        payload: 'Invalid token',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        metadata: {},
        ttl: null,
        signature: null,
      };
      sendMessageToClient(unauthorizedMsg);

      await disconnectedPromise;
      expect(client.getState()).toBe('disconnected');
    });
  });
});
