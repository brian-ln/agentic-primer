/**
 * Integration Tests: Cross-Runtime Messaging
 *
 * Tests message routing between SEAG and Browser actors:
 * - SEAG → Browser messaging
 * - Browser → SEAG messaging
 * - Fire-and-forget (tell pattern)
 * - Request-response (ask pattern with ack)
 * - Message payload integrity
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, sleep, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';
import type { CanonicalAddress } from '@agentic-primer/protocols';

describe('Signal Hub - Cross-Runtime Messaging', () => {
  let env: TestEnvironment;
  let seagActor: SeagActorWrapper;
  let browserActor: BrowserActorWrapper;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  beforeEach(async () => {
    // Create fresh actors for each test
    [seagActor, browserActor] = await Promise.all([
      createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute', 'inference'],
      }),
      createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui', 'interaction'],
      }),
    ]);
  });

  afterEach(async () => {
    if (seagActor) await seagActor.disconnect();
    if (browserActor) await browserActor.disconnect();
  });

  describe('SEAG → Browser Messaging', () => {
    it('should send message from SEAG to browser (tell pattern)', async () => {
      const messagePromise = browserActor.waitForMessage(
        msg => msg.type === 'test:ping',
        5000
      );

      // Send message from SEAG
      seagActor.send(env.browserAddress, 'test:ping', { data: 'hello from SEAG' });

      // Wait for message to arrive
      const receivedMsg = await messagePromise;

      expect(receivedMsg.type).toBe('test:ping');
      expect(receivedMsg.payload).toEqual({ data: 'hello from SEAG' });
      expect(receivedMsg.from).toBe(env.seagAddress);
    });

    it('should send message from SEAG to browser (ask pattern with ack)', async () => {
      const messagePromise = browserActor.waitForMessage(
        msg => msg.type === 'test:request',
        5000
      );

      // Send with acknowledgment
      const ackPromise = seagActor.sendWithAck(
        env.browserAddress,
        'test:request',
        { data: 'request from SEAG' }
      );

      // Wait for message to arrive
      const receivedMsg = await messagePromise;
      expect(receivedMsg.type).toBe('test:request');
      expect(receivedMsg.payload).toEqual({ data: 'request from SEAG' });

      // Wait for acknowledgment
      const ack = await ackPromise;
      expect(ack.type).toBe('hub:delivery_ack');
    });

    it('should preserve payload integrity', async () => {
      const complexPayload = {
        string: 'test',
        number: 42,
        boolean: true,
        null: null,
        array: [1, 2, 3],
        nested: {
          deeply: {
            nested: {
              value: 'works',
            },
          },
        },
      };

      const messagePromise = browserActor.waitForMessage(
        msg => msg.type === 'test:complex',
        5000
      );

      seagActor.send(env.browserAddress, 'test:complex', complexPayload);

      const receivedMsg = await messagePromise;
      expect(receivedMsg.payload).toEqual(complexPayload);
    });

    it('should handle large payloads', async () => {
      const largePayload = {
        data: 'x'.repeat(10000), // 10KB string
        array: Array.from({ length: 1000 }, (_, i) => ({ id: i, value: `item-${i}` })),
      };

      const messagePromise = browserActor.waitForMessage(
        msg => msg.type === 'test:large',
        5000
      );

      seagActor.send(env.browserAddress, 'test:large', largePayload);

      const receivedMsg = await messagePromise;
      expect(receivedMsg.payload).toEqual(largePayload);
    });
  });

  describe('Browser → SEAG Messaging', () => {
    it('should send message from browser to SEAG (tell pattern)', async () => {
      const messagePromise = seagActor.waitForMessage(
        msg => msg.type === 'test:ping',
        5000
      );

      // Send message from browser
      browserActor.send(env.seagAddress, 'test:ping', { data: 'hello from browser' });

      // Wait for message to arrive
      const receivedMsg = await messagePromise;

      expect(receivedMsg.type).toBe('test:ping');
      expect(receivedMsg.payload).toEqual({ data: 'hello from browser' });
      expect(receivedMsg.from).toBe(env.browserAddress);
    });

    it('should send message from browser to SEAG (ask pattern with ack)', async () => {
      const messagePromise = seagActor.waitForMessage(
        msg => msg.type === 'test:request',
        5000
      );

      // Send with acknowledgment
      const ackPromise = browserActor.sendWithAck(
        env.seagAddress,
        'test:request',
        { data: 'request from browser' }
      );

      // Wait for message to arrive
      const receivedMsg = await messagePromise;
      expect(receivedMsg.type).toBe('test:request');
      expect(receivedMsg.payload).toEqual({ data: 'request from browser' });

      // Wait for acknowledgment
      await ackPromise; // Should complete without error
    });

    it('should preserve payload integrity', async () => {
      const complexPayload = {
        string: 'browser test',
        number: 123,
        boolean: false,
        undefined: null, // undefined becomes null in JSON
        array: ['a', 'b', 'c'],
        nested: {
          level1: {
            level2: {
              value: 'deep value',
            },
          },
        },
      };

      const messagePromise = seagActor.waitForMessage(
        msg => msg.type === 'test:complex',
        5000
      );

      browserActor.send(env.seagAddress, 'test:complex', complexPayload);

      const receivedMsg = await messagePromise;
      expect(receivedMsg.payload).toEqual(complexPayload);
    });
  });

  describe('Bidirectional Messaging', () => {
    it('should support full roundtrip communication', async () => {
      // SEAG sends to browser
      const browserMessagePromise = browserActor.waitForMessage(
        msg => msg.type === 'test:roundtrip:step1',
        5000
      );

      seagActor.send(env.browserAddress, 'test:roundtrip:step1', { step: 1 });

      const msg1 = await browserMessagePromise;
      expect(msg1.payload).toEqual({ step: 1 });

      // Browser responds to SEAG
      const seagMessagePromise = seagActor.waitForMessage(
        msg => msg.type === 'test:roundtrip:step2',
        5000
      );

      browserActor.send(env.seagAddress, 'test:roundtrip:step2', { step: 2 });

      const msg2 = await seagMessagePromise;
      expect(msg2.payload).toEqual({ step: 2 });

      // SEAG completes roundtrip
      const browserFinalPromise = browserActor.waitForMessage(
        msg => msg.type === 'test:roundtrip:complete',
        5000
      );

      seagActor.send(env.browserAddress, 'test:roundtrip:complete', { step: 3 });

      const msg3 = await browserFinalPromise;
      expect(msg3.payload).toEqual({ step: 3 });
    });

    it('should handle concurrent bidirectional messages', async () => {
      // Set up promises for messages in both directions
      const browserMessages = Promise.all([
        browserActor.waitForMessage(msg => msg.type === 'test:concurrent:1', 5000),
        browserActor.waitForMessage(msg => msg.type === 'test:concurrent:2', 5000),
        browserActor.waitForMessage(msg => msg.type === 'test:concurrent:3', 5000),
      ]);

      const seagMessages = Promise.all([
        seagActor.waitForMessage(msg => msg.type === 'test:concurrent:a', 5000),
        seagActor.waitForMessage(msg => msg.type === 'test:concurrent:b', 5000),
        seagActor.waitForMessage(msg => msg.type === 'test:concurrent:c', 5000),
      ]);

      // Send concurrent messages
      seagActor.send(env.browserAddress, 'test:concurrent:1', { id: 1 });
      browserActor.send(env.seagAddress, 'test:concurrent:a', { id: 'a' });
      seagActor.send(env.browserAddress, 'test:concurrent:2', { id: 2 });
      browserActor.send(env.seagAddress, 'test:concurrent:b', { id: 'b' });
      seagActor.send(env.browserAddress, 'test:concurrent:3', { id: 3 });
      browserActor.send(env.seagAddress, 'test:concurrent:c', { id: 'c' });

      // All messages should arrive
      const [browserReceived, seagReceived] = await Promise.all([browserMessages, seagMessages]);

      expect(browserReceived).toHaveLength(3);
      expect(seagReceived).toHaveLength(3);
    });
  });

  describe('Message Ordering', () => {
    it('should maintain message order within a connection', async () => {
      const messageCount = 10;
      const receivedMessages: number[] = [];

      // Set up handler to collect message order
      const handler = (msg: any) => {
        if (msg.type === 'test:order') {
          receivedMessages.push(msg.payload.sequence);
        }
      };

      browserActor.on('message', handler);

      // Send messages in sequence
      for (let i = 0; i < messageCount; i++) {
        seagActor.send(env.browserAddress, 'test:order', { sequence: i });
      }

      // Wait for all messages to arrive
      await sleep(1000);

      // Messages should arrive in order
      expect(receivedMessages).toHaveLength(messageCount);
      expect(receivedMessages).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    });
  });

  describe('Message Metadata', () => {
    it('should preserve message metadata', async () => {
      const messagePromise = browserActor.waitForMessage(
        msg => msg.type === 'test:metadata',
        5000
      );

      seagActor.send(
        env.browserAddress,
        'test:metadata',
        { data: 'test' },
        {
          custom: 'value',
          requestId: 'req-123',
          priority: 'high',
        }
      );

      const receivedMsg = await messagePromise;
      expect(receivedMsg.metadata).toMatchObject({
        custom: 'value',
        requestId: 'req-123',
        priority: 'high',
      });
    });
  });
});
