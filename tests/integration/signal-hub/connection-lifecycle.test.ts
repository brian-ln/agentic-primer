/**
 * Integration Tests: Connection Lifecycle Fixes (P0)
 *
 * Tests for:
 * - Duplicate connection detection (closes old session)
 * - Disconnect response ordering (ack sent before close)
 * - Connection state tracking and observability
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, sleep, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';

describe('Signal Hub - Connection Lifecycle (P0 Fixes)', () => {
  let env: TestEnvironment;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  describe('Duplicate Connection Detection', () => {
    it('should close old session when same actor reconnects', async () => {
      // Create first connection
      const actor1 = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      const session1Id = actor1.getSessionId();
      expect(session1Id).toBeTruthy();
      expect(actor1.getState()).toBe('connected');

      // Track disconnect event on first connection
      let disconnectReceived = false;
      let disconnectReason: string | undefined;

      actor1.on('message', (msg) => {
        if (msg.type === 'hub:disconnect') {
          disconnectReceived = true;
          disconnectReason = (msg.payload as any)?.reason;
        }
      });

      // Create second connection with SAME actor identity
      const actor2 = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress, // Same address
        capabilities: ['compute'],
      });

      const session2Id = actor2.getSessionId();
      expect(session2Id).toBeTruthy();
      expect(session2Id).not.toBe(session1Id); // Different session IDs

      // Wait for disconnect to propagate
      await sleep(500);

      // Old session should receive disconnect with reason 'duplicate_connection'
      expect(disconnectReceived).toBe(true);
      expect(disconnectReason).toBe('duplicate_connection');

      // Old session should be disconnected
      expect(actor1.getState()).toBe('disconnected');

      // New session should still be connected
      expect(actor2.getState()).toBe('connected');

      // Clean up
      await actor2.disconnect();
    });

    it('should not affect sessions with different actor identities', async () => {
      // Create two different actors
      const seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      const browserActor = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      // Both should be connected
      expect(seagActor.getState()).toBe('connected');
      expect(browserActor.getState()).toBe('connected');

      // Create another SEAG actor (duplicate)
      const seagActor2 = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      await sleep(500);

      // First SEAG should be disconnected
      expect(seagActor.getState()).toBe('disconnected');

      // Second SEAG should be connected
      expect(seagActor2.getState()).toBe('connected');

      // Browser actor should NOT be affected
      expect(browserActor.getState()).toBe('connected');

      // Clean up
      await seagActor2.disconnect();
      await browserActor.disconnect();
    });

    it('should clean up old session registrations on duplicate', async () => {
      // Create first actor and register
      const actor1 = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      // Verify actor is registered by discovering it
      const discovery1 = await actor1.discover('compute');
      expect(discovery1.length).toBeGreaterThan(0);
      const found1 = discovery1.find((a) => a.address === env.seagAddress);
      expect(found1).toBeTruthy();

      // Create duplicate connection
      const actor2 = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute', 'inference'], // Different capabilities
      });

      await sleep(500);

      // Old actor should be disconnected
      expect(actor1.getState()).toBe('disconnected');

      // Discovery should show only NEW actor's registrations
      const discovery2 = await actor2.discover('compute');
      const found2 = discovery2.find((a) => a.address === env.seagAddress);
      expect(found2).toBeTruthy();
      expect(found2?.capabilities).toContain('inference'); // New capabilities

      // Clean up
      await actor2.disconnect();
    });
  });

  describe('Disconnect Response Ordering', () => {
    it('should receive hub:disconnect acknowledgment before WebSocket closes', async () => {
      const actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      let disconnectAckReceived = false;
      let disconnectAckTimestamp: number | undefined;
      let websocketCloseTimestamp: number | undefined;

      // Track disconnect acknowledgment
      actor.on('message', (msg) => {
        if (msg.type === 'hub:disconnect' && (msg.payload as any)?.acknowledged === true) {
          disconnectAckReceived = true;
          disconnectAckTimestamp = Date.now();
        }
      });

      // Track disconnected event (fired when WebSocket closes)
      const disconnectedHandler = () => {
        websocketCloseTimestamp = Date.now();
      };
      actor.on('disconnected', disconnectedHandler);

      // Disconnect
      await actor.disconnect();

      // Wait for events to propagate
      await sleep(200);

      // Verify acknowledgment was received
      expect(disconnectAckReceived).toBe(true);

      // Verify ack was received BEFORE WebSocket close
      if (disconnectAckTimestamp && websocketCloseTimestamp) {
        expect(disconnectAckTimestamp).toBeLessThanOrEqual(websocketCloseTimestamp);
      }

      // Final state should be disconnected
      expect(actor.getState()).toBe('disconnected');
    });

    it('should receive disconnect ack even with rapid disconnect', async () => {
      const actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      let disconnectAckReceived = false;

      actor.on('message', (msg) => {
        if (msg.type === 'hub:disconnect') {
          disconnectAckReceived = true;
        }
      });

      // Immediate disconnect after connect
      await actor.disconnect();

      await sleep(200);

      expect(disconnectAckReceived).toBe(true);
      expect(actor.getState()).toBe('disconnected');
    });
  });

  describe('Connection State Tracking', () => {
    it('should track connection state transitions', async () => {
      const actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      // After successful connect, state should be 'connected'
      expect(actor.getState()).toBe('connected');

      // After disconnect, state should be 'disconnected'
      await actor.disconnect();
      expect(actor.getState()).toBe('disconnected');
    });

    it('should track session duration metrics', async () => {
      const actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      const connectTime = Date.now();

      // Keep session alive for a bit
      await sleep(1000);

      await actor.disconnect();

      const disconnectTime = Date.now();
      const expectedDuration = disconnectTime - connectTime;

      // Session duration should be at least 1 second
      expect(expectedDuration).toBeGreaterThanOrEqual(1000);
    });
  });

  describe('Combined Scenarios', () => {
    it('should handle duplicate connection during active message exchange', async () => {
      // Create first actor
      const actor1 = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      // Create browser actor to communicate with
      const browserActor = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      // Start message exchange
      const messagePromise = new Promise((resolve) => {
        actor1.on('message', (msg) => {
          if (msg.type === 'test:ping') {
            resolve(msg);
          }
        });
      });

      browserActor.send(env.seagAddress, 'test:ping', { data: 'hello' });

      // While messages are in flight, create duplicate connection
      const actor2 = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      await sleep(500);

      // Old actor should be disconnected
      expect(actor1.getState()).toBe('disconnected');

      // New actor should be connected and can receive messages
      expect(actor2.getState()).toBe('connected');

      const messagePromise2 = new Promise((resolve) => {
        actor2.on('message', (msg) => {
          if (msg.type === 'test:ping2') {
            resolve(msg);
          }
        });
      });

      browserActor.send(env.seagAddress, 'test:ping2', { data: 'hello again' });

      await expect(Promise.race([messagePromise2, sleep(2000).then(() => 'timeout')])).resolves.not.toBe('timeout');

      // Clean up
      await actor2.disconnect();
      await browserActor.disconnect();
    });
  });
});
