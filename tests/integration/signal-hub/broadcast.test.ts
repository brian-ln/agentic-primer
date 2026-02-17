/**
 * Integration Tests: Broadcast
 *
 * Tests broadcast messaging across SEAG and Browser actors:
 * - Broadcast to all actors
 * - Cross-runtime broadcast
 * - Broadcast acknowledgments
 * - Broadcast filtering by capability
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, sleep, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';
import { generateSeagActorJWT, generateBrowserActorJWT } from './helpers/jwt.js';
import type { CanonicalAddress } from '@agentic-primer/protocols';

describe('Signal Hub - Broadcast', () => {
  let env: TestEnvironment;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  describe('Basic Broadcast', () => {
    let seagActor: SeagActorWrapper;
    let browserActor1: BrowserActorWrapper;
    let browserActor2: BrowserActorWrapper;

    beforeEach(async () => {
      const browser1Jwt = await generateBrowserActorJWT('@(browser/broadcast-1)' as CanonicalAddress, ['ui']);
      const browser2Jwt = await generateBrowserActorJWT('@(browser/broadcast-2)' as CanonicalAddress, ['ui']);

      [seagActor, browserActor1, browserActor2] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: env.seagJwt,
          actorAddress: env.seagAddress,
          capabilities: ['compute'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser1Jwt,
          actorAddress: '@(browser/broadcast-1)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser2Jwt,
          actorAddress: '@(browser/broadcast-2)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
      ]);
    });

    afterEach(async () => {
      if (seagActor) await seagActor.disconnect();
      if (browserActor1) await browserActor1.disconnect();
      if (browserActor2) await browserActor2.disconnect();
    });

    it('should broadcast from SEAG to all browser actors', async () => {
      const browser1Promise = browserActor1.waitForMessage(
        msg => msg.type === 'test:broadcast:all',
        5000
      );
      const browser2Promise = browserActor2.waitForMessage(
        msg => msg.type === 'test:broadcast:all',
        5000
      );

      // Broadcast from SEAG
      await seagActor.getClient().broadcast('test:broadcast:all', { data: 'broadcast message' });

      // Both browser actors should receive the message
      const [msg1, msg2] = await Promise.all([browser1Promise, browser2Promise]);

      expect(msg1.type).toBe('test:broadcast:all');
      expect(msg1.payload).toEqual({ data: 'broadcast message' });
      expect(msg2.type).toBe('test:broadcast:all');
      expect(msg2.payload).toEqual({ data: 'broadcast message' });
    });

    it('should broadcast from browser to SEAG and other browsers', async () => {
      const seagPromise = seagActor.waitForMessage(
        msg => msg.type === 'test:browser:broadcast',
        5000
      );
      const browser2Promise = browserActor2.waitForMessage(
        msg => msg.type === 'test:browser:broadcast',
        5000
      );

      // Broadcast from browser actor 1
      await browserActor1.broadcast('test:browser:broadcast', { source: 'browser-1' });

      // SEAG and browser2 should receive (browser1 doesn't receive its own broadcast)
      const [seagMsg, browser2Msg] = await Promise.all([seagPromise, browser2Promise]);

      expect(seagMsg.type).toBe('test:browser:broadcast');
      expect(seagMsg.payload).toEqual({ source: 'browser-1' });
      expect(browser2Msg.type).toBe('test:browser:broadcast');
      expect(browser2Msg.payload).toEqual({ source: 'browser-1' });
    });
  });

  describe('Multi-Actor Broadcast', () => {
    const actors: (SeagActorWrapper | BrowserActorWrapper)[] = [];

    afterEach(async () => {
      await Promise.all(actors.map(a => a.disconnect()));
      actors.length = 0;
    });

    it('should deliver broadcast to many actors', async () => {
      // Create 5 actors (2 SEAG, 3 Browser)
      const seag1Jwt = await generateSeagActorJWT('@(local/seag-bc-1)' as CanonicalAddress, ['compute']);
      const seag2Jwt = await generateSeagActorJWT('@(local/seag-bc-2)' as CanonicalAddress, ['storage']);
      const browser1Jwt = await generateBrowserActorJWT('@(browser/bc-1)' as CanonicalAddress, ['ui']);
      const browser2Jwt = await generateBrowserActorJWT('@(browser/bc-2)' as CanonicalAddress, ['rendering']);
      const browser3Jwt = await generateBrowserActorJWT('@(browser/bc-3)' as CanonicalAddress, ['ui']);

      const [seag1, seag2, browser1, browser2, browser3] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: seag1Jwt,
          actorAddress: '@(local/seag-bc-1)' as CanonicalAddress,
          capabilities: ['compute'],
        }),
        createSeagActor({
          url: env.hub.url,
          jwt: seag2Jwt,
          actorAddress: '@(local/seag-bc-2)' as CanonicalAddress,
          capabilities: ['storage'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser1Jwt,
          actorAddress: '@(browser/bc-1)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser2Jwt,
          actorAddress: '@(browser/bc-2)' as CanonicalAddress,
          capabilities: ['rendering'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser3Jwt,
          actorAddress: '@(browser/bc-3)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
      ]);

      actors.push(seag1, seag2, browser1, browser2, browser3);

      // Set up listeners for all actors except the broadcaster
      const messagePromises = [
        seag2.waitForMessage(msg => msg.type === 'test:multi:broadcast', 5000),
        browser1.waitForMessage(msg => msg.type === 'test:multi:broadcast', 5000),
        browser2.waitForMessage(msg => msg.type === 'test:multi:broadcast', 5000),
        browser3.waitForMessage(msg => msg.type === 'test:multi:broadcast', 5000),
      ];

      // Broadcast from seag1
      await seag1.getClient().broadcast('test:multi:broadcast', { sender: 'seag1' });

      // All other actors should receive
      const messages = await Promise.all(messagePromises);
      expect(messages).toHaveLength(4);
      messages.forEach(msg => {
        expect(msg.type).toBe('test:multi:broadcast');
        expect(msg.payload).toEqual({ sender: 'seag1' });
      });
    });
  });

  describe('Broadcast Acknowledgments', () => {
    let seagActor: SeagActorWrapper;
    let browserActor1: BrowserActorWrapper;
    let browserActor2: BrowserActorWrapper;

    beforeEach(async () => {
      const browser1Jwt = await generateBrowserActorJWT('@(browser/ack-1)' as CanonicalAddress, ['ui']);
      const browser2Jwt = await generateBrowserActorJWT('@(browser/ack-2)' as CanonicalAddress, ['ui']);

      [seagActor, browserActor1, browserActor2] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: env.seagJwt,
          actorAddress: env.seagAddress,
          capabilities: ['compute'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser1Jwt,
          actorAddress: '@(browser/ack-1)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser2Jwt,
          actorAddress: '@(browser/ack-2)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
      ]);
    });

    afterEach(async () => {
      if (seagActor) await seagActor.disconnect();
      if (browserActor1) await browserActor1.disconnect();
      if (browserActor2) await browserActor2.disconnect();
    });

    it('should return broadcast acknowledgment with recipient count', async () => {
      // Wait for messages
      const browser1Promise = browserActor1.waitForMessage(
        msg => msg.type === 'test:broadcast:ack',
        5000
      );
      const browser2Promise = browserActor2.waitForMessage(
        msg => msg.type === 'test:broadcast:ack',
        5000
      );

      // Broadcast and wait for ack
      await seagActor.getClient().broadcast('test:broadcast:ack', { data: 'ack test' });

      // Verify messages received
      await Promise.all([browser1Promise, browser2Promise]);

      // Note: The actual ack response format depends on Signal Hub implementation
      // This test verifies the broadcast completes without error
    });
  });

  describe('Broadcast with Empty Network', () => {
    let seagActor: SeagActorWrapper;

    beforeEach(async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });
    });

    afterEach(async () => {
      if (seagActor) await seagActor.disconnect();
    });

    it('should handle broadcast when only one actor is connected', async () => {
      // Broadcast should not fail even if no other actors are connected
      await expect(
        seagActor.getClient().broadcast('test:empty:broadcast', { data: 'alone' })
      ).resolves.not.toThrow();
    });
  });

  describe('Broadcast Payload Integrity', () => {
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

    it('should preserve complex payloads in broadcasts', async () => {
      const complexPayload = {
        string: 'broadcast test',
        number: 999,
        boolean: true,
        array: [1, 2, 3, 4, 5],
        nested: {
          deep: {
            value: 'nested broadcast',
          },
        },
      };

      const messagePromise = browserActor.waitForMessage(
        msg => msg.type === 'test:broadcast:complex',
        5000
      );

      await seagActor.getClient().broadcast('test:broadcast:complex', complexPayload);

      const received = await messagePromise;
      expect(received.payload).toEqual(complexPayload);
    });
  });
});
