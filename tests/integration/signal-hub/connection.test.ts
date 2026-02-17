/**
 * Integration Tests: Connection Lifecycle
 *
 * Tests full connection flow for both SEAG and Browser actors:
 * - Connection establishment
 * - Authentication via JWT
 * - Session management
 * - Heartbeat
 * - Graceful disconnect
 * - Reconnection
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, sleep, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';
import type { CanonicalAddress } from '@agentic-primer/protocols';

describe('Signal Hub - Connection Lifecycle', () => {
  let env: TestEnvironment;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  describe('SEAG Actor Connection', () => {
    let seagActor: SeagActorWrapper;

    afterEach(async () => {
      if (seagActor) {
        await seagActor.disconnect();
      }
    });

    it('should connect and receive hub:connected with session info', async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute', 'inference'],
      });

      expect(seagActor.getState()).toBe('connected');
      expect(seagActor.getSessionId()).toBeTruthy();
    });

    it('should authenticate via JWT', async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      expect(seagActor.getSessionId()).toBeTruthy();
    });

    it('should disconnect gracefully', async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      const sessionId = seagActor.getSessionId();
      expect(sessionId).toBeTruthy();

      await seagActor.disconnect();

      expect(seagActor.getState()).toBe('disconnected');
      expect(seagActor.getSessionId()).toBeNull();
    });
  });

  describe('Browser Actor Connection', () => {
    let browserActor: BrowserActorWrapper;

    afterEach(async () => {
      if (browserActor) {
        await browserActor.disconnect();
      }
    });

    it('should connect and receive hub:connected with session info', async () => {
      browserActor = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui', 'interaction'],
      });

      expect(browserActor.getState()).toBe('connected');
      expect(browserActor.getSessionId()).toBeTruthy();
    });

    it('should authenticate via JWT', async () => {
      browserActor = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      expect(browserActor.getSessionId()).toBeTruthy();
    });

    it('should disconnect gracefully', async () => {
      browserActor = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      const sessionId = browserActor.getSessionId();
      expect(sessionId).toBeTruthy();

      await browserActor.disconnect();

      expect(browserActor.getState()).toBe('disconnected');
      expect(browserActor.getSessionId()).toBeNull();
    });
  });

  describe('Concurrent Connections', () => {
    let seagActor: SeagActorWrapper;
    let browserActor: BrowserActorWrapper;

    afterEach(async () => {
      if (seagActor) await seagActor.disconnect();
      if (browserActor) await browserActor.disconnect();
    });

    it('should allow multiple actors to connect simultaneously', async () => {
      const [seag, browser] = await Promise.all([
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

      seagActor = seag;
      browserActor = browser;

      expect(seagActor.getState()).toBe('connected');
      expect(browserActor.getState()).toBe('connected');
      expect(seagActor.getSessionId()).toBeTruthy();
      expect(browserActor.getSessionId()).toBeTruthy();
      expect(seagActor.getSessionId()).not.toBe(browserActor.getSessionId());
    });

    it('should maintain independent sessions for different actors', async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      browserActor = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui'],
      });

      const seagSession = seagActor.getSessionId();
      const browserSession = browserActor.getSessionId();

      expect(seagSession).not.toBe(browserSession);

      // Disconnect one actor shouldn't affect the other
      await seagActor.disconnect();

      expect(seagActor.getState()).toBe('disconnected');
      expect(browserActor.getState()).toBe('connected');
      expect(browserActor.getSessionId()).toBe(browserSession);
    });
  });

  describe('Heartbeat', () => {
    let seagActor: SeagActorWrapper;

    afterEach(async () => {
      if (seagActor) {
        await seagActor.disconnect();
      }
    });

    it('should maintain connection with heartbeat', async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
      });

      const initialSessionId = seagActor.getSessionId();

      // Wait for heartbeat interval (25s for client, 30s for server)
      await sleep(26000);

      // Connection should still be alive
      expect(seagActor.getState()).toBe('connected');
      expect(seagActor.getSessionId()).toBe(initialSessionId);
    }, 30000); // Extend timeout for heartbeat test
  });
});
