/**
 * Integration Tests: Pub/Sub
 *
 * Tests topic-based publish/subscribe messaging:
 * - Subscribe to topics
 * - Publish to topics
 * - Cross-runtime pub/sub
 * - Multiple subscribers
 * - Unsubscribe
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, sleep, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';
import { generateSeagActorJWT, generateBrowserActorJWT } from './helpers/jwt.js';
import type { CanonicalAddress } from '@agentic-primer/protocols';

describe('Signal Hub - Pub/Sub', () => {
  let env: TestEnvironment;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  describe('Basic Pub/Sub', () => {
    let publisherSeag: SeagActorWrapper;
    let subscriberBrowser: BrowserActorWrapper;

    beforeEach(async () => {
      [publisherSeag, subscriberBrowser] = await Promise.all([
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
      if (publisherSeag) await publisherSeag.disconnect();
      if (subscriberBrowser) await subscriberBrowser.disconnect();
    });

    it('should subscribe to topic and receive published messages', async () => {
      // Subscribe to topic
      await subscriberBrowser.subscribe('test-topic');

      // Set up message listener
      const messagePromise = subscriberBrowser.waitForMessage(
        msg => msg.type === 'test:topic:message',
        5000
      );

      // Publish to topic
      await publisherSeag.getClient().publish('test-topic', 'test:topic:message', { data: 'topic message' });

      // Subscriber should receive message
      const received = await messagePromise;
      expect(received.type).toBe('test:topic:message');
      expect(received.payload).toEqual({ data: 'topic message' });
    });

    it('should not receive messages from unsubscribed topics', async () => {
      // Subscribe to one topic
      await subscriberBrowser.subscribe('topic-a');

      let receivedFromB = false;
      subscriberBrowser.getClient().onMessage((msg: any) => {
        if (msg.type === 'test:topic:b:message') {
          receivedFromB = true;
        }
      });

      // Publish to different topic
      await publisherSeag.getClient().publish('topic-b', 'test:topic:b:message', { data: 'should not receive' });

      // Wait to ensure message would have arrived if it was going to
      await sleep(1000);

      // Should not have received message from topic-b
      expect(receivedFromB).toBe(false);
    });

    it('should receive messages only after subscription', async () => {
      let messageCount = 0;
      subscriberBrowser.getClient().onMessage((msg: any) => {
        if (msg.type === 'test:before:after') {
          messageCount++;
        }
      });

      // Publish before subscription
      await publisherSeag.getClient().publish('delayed-topic', 'test:before:after', { when: 'before' });
      await sleep(500);

      // Subscribe
      await subscriberBrowser.subscribe('delayed-topic');
      await sleep(100);

      // Publish after subscription
      const messagePromise = subscriberBrowser.waitForMessage(
        msg => msg.type === 'test:before:after',
        5000
      );

      await publisherSeag.getClient().publish('delayed-topic', 'test:before:after', { when: 'after' });

      const received = await messagePromise;

      // Should only receive the message sent after subscription
      expect(received.payload).toEqual({ when: 'after' });
      expect(messageCount).toBe(1); // Only the 'after' message
    });
  });

  describe('Multiple Subscribers', () => {
    const actors: (SeagActorWrapper | BrowserActorWrapper)[] = [];

    afterEach(async () => {
      await Promise.all(actors.map(a => a.disconnect()));
      actors.length = 0;
    });

    it('should deliver topic messages to all subscribers', async () => {
      // Create publisher and 3 subscribers
      const publisherJwt = await generateSeagActorJWT('@(local/publisher)' as CanonicalAddress, ['compute']);
      const sub1Jwt = await generateBrowserActorJWT('@(browser/sub-1)' as CanonicalAddress, ['ui']);
      const sub2Jwt = await generateBrowserActorJWT('@(browser/sub-2)' as CanonicalAddress, ['ui']);
      const sub3Jwt = await generateSeagActorJWT('@(local/sub-3)' as CanonicalAddress, ['storage']);

      const [publisher, sub1, sub2, sub3] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: publisherJwt,
          actorAddress: '@(local/publisher)' as CanonicalAddress,
          capabilities: ['compute'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: sub1Jwt,
          actorAddress: '@(browser/sub-1)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: sub2Jwt,
          actorAddress: '@(browser/sub-2)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createSeagActor({
          url: env.hub.url,
          jwt: sub3Jwt,
          actorAddress: '@(local/sub-3)' as CanonicalAddress,
          capabilities: ['storage'],
        }),
      ]);

      actors.push(publisher, sub1, sub2, sub3);

      // All subscribers subscribe to same topic
      await Promise.all([
        sub1.subscribe('shared-topic'),
        sub2.subscribe('shared-topic'),
        sub3.subscribe('shared-topic'),
      ]);

      // Set up message listeners
      const messagePromises = [
        sub1.waitForMessage(msg => msg.type === 'test:shared:topic', 5000),
        sub2.waitForMessage(msg => msg.type === 'test:shared:topic', 5000),
        sub3.waitForMessage(msg => msg.type === 'test:shared:topic', 5000),
      ];

      // Publish once
      await publisher.getClient().publish('shared-topic', 'test:shared:topic', { data: 'shared message' });

      // All subscribers should receive
      const messages = await Promise.all(messagePromises);
      expect(messages).toHaveLength(3);
      messages.forEach(msg => {
        expect(msg.type).toBe('test:shared:topic');
        expect(msg.payload).toEqual({ data: 'shared message' });
      });
    });

    it('should support multiple topics with different subscriber sets', async () => {
      const pub1Jwt = await generateSeagActorJWT('@(local/pub-multi-1)' as CanonicalAddress, ['compute']);
      const sub1Jwt = await generateBrowserActorJWT('@(browser/sub-multi-1)' as CanonicalAddress, ['ui']);
      const sub2Jwt = await generateBrowserActorJWT('@(browser/sub-multi-2)' as CanonicalAddress, ['ui']);

      const [pub1, sub1, sub2] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: pub1Jwt,
          actorAddress: '@(local/pub-multi-1)' as CanonicalAddress,
          capabilities: ['compute'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: sub1Jwt,
          actorAddress: '@(browser/sub-multi-1)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: sub2Jwt,
          actorAddress: '@(browser/sub-multi-2)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
      ]);

      actors.push(pub1, sub1, sub2);

      // sub1 subscribes to topic-x, sub2 subscribes to topic-y
      await sub1.subscribe('topic-x');
      await sub2.subscribe('topic-y');

      // Set up listeners
      const sub1Promise = sub1.waitForMessage(msg => msg.type === 'test:topic:x', 5000);
      const sub2Promise = sub2.waitForMessage(msg => msg.type === 'test:topic:y', 5000);

      // Publish to both topics
      await pub1.getClient().publish('topic-x', 'test:topic:x', { topic: 'x' });
      await pub1.getClient().publish('topic-y', 'test:topic:y', { topic: 'y' });

      // Each subscriber should receive only their topic's message
      const [msg1, msg2] = await Promise.all([sub1Promise, sub2Promise]);

      expect(msg1.type).toBe('test:topic:x');
      expect(msg1.payload).toEqual({ topic: 'x' });
      expect(msg2.type).toBe('test:topic:y');
      expect(msg2.payload).toEqual({ topic: 'y' });
    });
  });

  describe('Unsubscribe', () => {
    let publisher: SeagActorWrapper;
    let subscriber: BrowserActorWrapper;

    beforeEach(async () => {
      [publisher, subscriber] = await Promise.all([
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
      if (publisher) await publisher.disconnect();
      if (subscriber) await subscriber.disconnect();
    });

    it('should stop receiving messages after unsubscribe', async () => {
      // Subscribe
      await subscriber.subscribe('unsub-topic');

      // Receive first message
      const firstPromise = subscriber.waitForMessage(
        msg => msg.type === 'test:unsub:first',
        5000
      );

      await publisher.getClient().publish('unsub-topic', 'test:unsub:first', { msg: 'first' });
      await firstPromise;

      // Unsubscribe
      await subscriber.unsubscribe('unsub-topic');
      await sleep(500);

      // Track if second message arrives
      let receivedSecond = false;
      subscriber.getClient().onMessage((msg: any) => {
        if (msg.type === 'test:unsub:second') {
          receivedSecond = true;
        }
      });

      // Publish second message
      await publisher.getClient().publish('unsub-topic', 'test:unsub:second', { msg: 'second' });
      await sleep(1000);

      // Should not receive second message
      expect(receivedSecond).toBe(false);
    });
  });

  describe('Cross-Runtime Pub/Sub', () => {
    let seagPublisher: SeagActorWrapper;
    let browserPublisher: BrowserActorWrapper;
    let seagSubscriber: SeagActorWrapper;
    let browserSubscriber: BrowserActorWrapper;

    beforeEach(async () => {
      const seagPubJwt = await generateSeagActorJWT('@(local/seag-pub)' as CanonicalAddress, ['compute']);
      const browserPubJwt = await generateBrowserActorJWT('@(browser/browser-pub)' as CanonicalAddress, ['ui']);
      const seagSubJwt = await generateSeagActorJWT('@(local/seag-sub)' as CanonicalAddress, ['storage']);
      const browserSubJwt = await generateBrowserActorJWT('@(browser/browser-sub)' as CanonicalAddress, ['rendering']);

      [seagPublisher, browserPublisher, seagSubscriber, browserSubscriber] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: seagPubJwt,
          actorAddress: '@(local/seag-pub)' as CanonicalAddress,
          capabilities: ['compute'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browserPubJwt,
          actorAddress: '@(browser/browser-pub)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createSeagActor({
          url: env.hub.url,
          jwt: seagSubJwt,
          actorAddress: '@(local/seag-sub)' as CanonicalAddress,
          capabilities: ['storage'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browserSubJwt,
          actorAddress: '@(browser/browser-sub)' as CanonicalAddress,
          capabilities: ['rendering'],
        }),
      ]);

      // Both subscribers subscribe to the same topic
      await Promise.all([
        seagSubscriber.subscribe('cross-runtime-topic'),
        browserSubscriber.subscribe('cross-runtime-topic'),
      ]);
    });

    afterEach(async () => {
      if (seagPublisher) await seagPublisher.disconnect();
      if (browserPublisher) await browserPublisher.disconnect();
      if (seagSubscriber) await seagSubscriber.disconnect();
      if (browserSubscriber) await browserSubscriber.disconnect();
    });

    it('should deliver SEAG publisher messages to both SEAG and browser subscribers', async () => {
      const seagPromise = seagSubscriber.waitForMessage(
        msg => msg.type === 'test:cross:seag:pub',
        5000
      );
      const browserPromise = browserSubscriber.waitForMessage(
        msg => msg.type === 'test:cross:seag:pub',
        5000
      );

      await seagPublisher.getClient().publish(
        'cross-runtime-topic',
        'test:cross:seag:pub',
        { from: 'seag' }
      );

      const [seagMsg, browserMsg] = await Promise.all([seagPromise, browserPromise]);

      expect(seagMsg.payload).toEqual({ from: 'seag' });
      expect(browserMsg.payload).toEqual({ from: 'seag' });
    });

    it('should deliver browser publisher messages to both SEAG and browser subscribers', async () => {
      const seagPromise = seagSubscriber.waitForMessage(
        msg => msg.type === 'test:cross:browser:pub',
        5000
      );
      const browserPromise = browserSubscriber.waitForMessage(
        msg => msg.type === 'test:cross:browser:pub',
        5000
      );

      await browserPublisher.publish(
        'cross-runtime-topic',
        'test:cross:browser:pub',
        { from: 'browser' }
      );

      const [seagMsg, browserMsg] = await Promise.all([seagPromise, browserPromise]);

      expect(seagMsg.payload).toEqual({ from: 'browser' });
      expect(browserMsg.payload).toEqual({ from: 'browser' });
    });
  });
});
