/**
 * Integration Tests: Session Identity Verification
 *
 * Tests that session.actorIdentity correctly matches registered addresses
 * and is used (not session.sessionId) for subscriptions and message routing.
 *
 * **Purpose:** Prevent session identity mismatch bugs where temporary session IDs
 * are used instead of registered actor addresses.
 *
 * **Background:**
 * - Sessions have two identities: sessionId (temporary) and actorIdentity (registered)
 * - actorIdentity is null until hub:register completes
 * - Subscriptions and message routing MUST use actorIdentity, not sessionId
 *
 * **Related:** See WEBSOCKET_PATTERNS.md § Pattern 1: Session Identity Mismatch
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, sleep, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';
import type { CanonicalAddress } from '@agentic-primer/protocols';

describe('Signal Hub - Session Identity', () => {
  let env: TestEnvironment;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  describe('Registration Identity', () => {
    let actor: SeagActorWrapper;

    afterEach(async () => {
      if (actor) await actor.disconnect();
    });

    it('should set session.actorIdentity to registered address', async () => {
      // Connect (creates session with null actorIdentity)
      actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      // After registration, actorIdentity should match registered address
      // We verify this indirectly by checking that the actor can receive messages
      // (which requires correct actorIdentity in registry)

      // Create another actor to send message to first actor
      const sender = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      try {
        // Set up message listener on first actor
        const messagePromise = actor.waitForMessage(
          msg => msg.type === 'test:message',
          5000
        );

        // Send message to first actor using registered address
        await sender.send(env.seagAddress, 'test:message', { data: 'identity test' });

        // If actorIdentity is correct, message will be delivered
        const received = await messagePromise;
        expect(received.type).toBe('test:message');
        expect(received.to).toBe(env.seagAddress);
        expect(received.payload).toEqual({ data: 'identity test' });
      } finally {
        await sender.disconnect();
      }
    });

    it('should use actorIdentity (not sessionId) for subscriptions', async () => {
      // Create subscriber
      const subscriber = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      try {
        // Subscribe to topic
        await subscriber.subscribe('identity-test-topic');

        // Create publisher
        const publisher = await createBrowserActor({
          url: env.hub.url,
          jwt: env.browserJwt,
          actorAddress: env.browserAddress,
          capabilities: ['ui'],
        });

        try {
          // Set up message listener
          const messagePromise = subscriber.waitForMessage(
            msg => msg.type === 'test:topic:message',
            5000
          );

          // Publish to topic
          await publisher.publish('identity-test-topic', 'test:topic:message', {
            data: 'subscription identity test'
          });

          // If subscription used correct actorIdentity, message will be delivered
          const received = await messagePromise;
          expect(received.type).toBe('test:topic:message');
          expect(received.to).toBe(env.seagAddress); // Should be actorIdentity, not sessionId
          expect(received.payload).toEqual({ data: 'subscription identity test' });
          expect(received.metadata?.topic).toBe('identity-test-topic');
        } finally {
          await publisher.disconnect();
        }
      } finally {
        await subscriber.disconnect();
      }
    });

    it('should maintain correct actorIdentity after reconnection', async () => {
      // Create actor and subscribe
      actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      await actor.subscribe('reconnect-test-topic');

      // Force reconnect (simulates network hiccup)
      await actor.reconnect();

      // Wait for reconnection to complete
      await sleep(500);

      // Create publisher
      const publisher = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      try {
        // Set up message listener
        const messagePromise = actor.waitForMessage(
          msg => msg.type === 'test:reconnect:message',
          5000
        );

        // Publish to topic
        await publisher.publish('reconnect-test-topic', 'test:reconnect:message', {
          data: 'after reconnect'
        });

        // Should still receive message with correct actorIdentity
        const received = await messagePromise;
        expect(received.type).toBe('test:reconnect:message');
        expect(received.to).toBe(env.seagAddress);
        expect(received.payload).toEqual({ data: 'after reconnect' });
      } finally {
        await publisher.disconnect();
      }
    });
  });

  describe('Cross-Runtime Identity', () => {
    let seagActor: SeagActorWrapper;
    let browserActor: BrowserActorWrapper;

    beforeEach(async () => {
      [seagActor, browserActor] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: env.seagJwt,
          actorAddress: env.seagAddress,
          capabilities: ['compute'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: env.browserJwt,
          actorAddress: env.browserAddress,
          capabilities: ['ui'],
        }),
      ]);
    });

    afterEach(async () => {
      if (seagActor) await seagActor.disconnect();
      if (browserActor) await browserActor.disconnect();
    });

    it('should route messages between runtimes using actorIdentity', async () => {
      // SEAG → Browser
      const browserMessagePromise = browserActor.waitForMessage(
        msg => msg.type === 'seag:to:browser',
        5000
      );

      await seagActor.send(env.browserAddress, 'seag:to:browser', {
        source: 'seag',
        destination: 'browser'
      });

      const browserReceived = await browserMessagePromise;
      expect(browserReceived.from).toBe(env.seagAddress);
      expect(browserReceived.to).toBe(env.browserAddress);
      expect(browserReceived.payload).toEqual({
        source: 'seag',
        destination: 'browser'
      });

      // Browser → SEAG
      const seagMessagePromise = seagActor.waitForMessage(
        msg => msg.type === 'browser:to:seag',
        5000
      );

      await browserActor.send(env.seagAddress, 'browser:to:seag', {
        source: 'browser',
        destination: 'seag'
      });

      const seagReceived = await seagMessagePromise;
      expect(seagReceived.from).toBe(env.browserAddress);
      expect(seagReceived.to).toBe(env.seagAddress);
      expect(seagReceived.payload).toEqual({
        source: 'browser',
        destination: 'seag'
      });
    });

    it('should deliver pub/sub messages using subscriber actorIdentity', async () => {
      // Both actors subscribe to same topic
      await Promise.all([
        seagActor.subscribe('cross-runtime-topic'),
        browserActor.subscribe('cross-runtime-topic'),
      ]);

      // SEAG publishes
      const browserMessagePromise = browserActor.waitForMessage(
        msg => msg.type === 'cross:runtime:event',
        5000
      );
      const seagMessagePromise = seagActor.waitForMessage(
        msg => msg.type === 'cross:runtime:event',
        5000
      );

      await seagActor.publish('cross-runtime-topic', 'cross:runtime:event', {
        publisher: 'seag'
      });

      // Both should receive with correct 'to' addresses
      const [browserReceived, seagReceived] = await Promise.all([
        browserMessagePromise,
        seagMessagePromise,
      ]);

      // Browser should receive message addressed to browserAddress
      expect(browserReceived.to).toBe(env.browserAddress);
      expect(browserReceived.from).toBe(env.seagAddress);
      expect(browserReceived.payload).toEqual({ publisher: 'seag' });

      // SEAG should receive its own publication (addressed to seagAddress)
      expect(seagReceived.to).toBe(env.seagAddress);
      expect(seagReceived.from).toBe(env.seagAddress);
      expect(seagReceived.payload).toEqual({ publisher: 'seag' });
    });
  });

  describe('Identity Edge Cases', () => {
    let actor: SeagActorWrapper;

    afterEach(async () => {
      if (actor) await actor.disconnect();
    });

    it('should handle multiple subscriptions with same actorIdentity', async () => {
      actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      // Subscribe to multiple topics
      await Promise.all([
        actor.subscribe('topic-a'),
        actor.subscribe('topic-b'),
        actor.subscribe('topic-c'),
      ]);

      // Create publisher
      const publisher = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      try {
        // Publish to each topic
        await Promise.all([
          publisher.publish('topic-a', 'event:a', { topic: 'a' }),
          publisher.publish('topic-b', 'event:b', { topic: 'b' }),
          publisher.publish('topic-c', 'event:c', { topic: 'c' }),
        ]);

        // Wait for all messages
        const [msgA, msgB, msgC] = await Promise.all([
          actor.waitForMessage(msg => msg.type === 'event:a', 5000),
          actor.waitForMessage(msg => msg.type === 'event:b', 5000),
          actor.waitForMessage(msg => msg.type === 'event:c', 5000),
        ]);

        // All should be addressed to same actorIdentity
        expect(msgA.to).toBe(env.seagAddress);
        expect(msgB.to).toBe(env.seagAddress);
        expect(msgC.to).toBe(env.seagAddress);

        // But from different topics
        expect(msgA.metadata?.topic).toBe('topic-a');
        expect(msgB.metadata?.topic).toBe('topic-b');
        expect(msgC.metadata?.topic).toBe('topic-c');
      } finally {
        await publisher.disconnect();
      }
    });

    it('should reject messages to unregistered address', async () => {
      actor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      // Try to send to non-existent address with ask pattern
      const fakeAddress = 'runtime:fake/actor-999' as CanonicalAddress;

      // Use sendWithAck to trigger ask pattern and get error response
      try {
        const response = await actor.sendWithAck(fakeAddress, 'test:message', { data: 'should fail' });

        // Should receive hub:unknown_actor response
        expect(response.type).toBe('hub:unknown_actor');
        expect(response.payload).toMatchObject({
          actorAddress: fakeAddress,
          message: expect.stringContaining('not registered'),
        });
      } catch (error) {
        // Client might throw on unknown_actor response
        expect((error as Error).message).toContain('not registered');
      }
    });
  });
});
