/**
 * Integration Tests: Actor Discovery
 *
 * Tests actor registration and discovery across SEAG and Browser runtimes:
 * - Actor registration with capabilities
 * - Discovery by capability
 * - List all actors
 * - Cross-runtime visibility
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { setupTestEnvironment, teardownTestEnvironment, sleep, type TestEnvironment } from './setup.js';
import { createSeagActor, type SeagActorWrapper } from './helpers/seag-actor.js';
import { createBrowserActor, type BrowserActorWrapper } from './helpers/browser-actor.js';
import { generateSeagActorJWT, generateBrowserActorJWT } from './helpers/jwt.js';
import type { CanonicalAddress } from '@agentic-primer/protocols';

describe('Signal Hub - Actor Discovery', () => {
  let env: TestEnvironment;

  beforeAll(async () => {
    env = await setupTestEnvironment();
  });

  afterAll(async () => {
    await teardownTestEnvironment(env);
  });

  describe('Actor Registration', () => {
    let seagActor: SeagActorWrapper;
    let browserActor: BrowserActorWrapper;

    afterEach(async () => {
      if (seagActor) await seagActor.disconnect();
      if (browserActor) await browserActor.disconnect();
    });

    it('should register SEAG actor with capabilities', async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute', 'inference', 'storage'],
      });

      expect(seagActor.getState()).toBe('connected');
      // Registration happens in createSeagActor
    });

    it('should register browser actor with capabilities', async () => {
      browserActor = await createBrowserActor({
        url: env.hub.url,
        jwt: env.browserJwt,
        actorAddress: env.browserAddress,
        capabilities: ['ui', 'interaction', 'rendering'],
      });

      expect(browserActor.getState()).toBe('connected');
      // Registration happens in createBrowserActor
    });

    it('should register actors with metadata', async () => {
      seagActor = await createSeagActor({
        url: env.hub.url,
        jwt: env.seagJwt,
        actorAddress: env.seagAddress,
        capabilities: ['compute'],
        metadata: {
          version: '1.0.0',
          environment: 'test',
          region: 'us-west',
        },
      });

      expect(seagActor.getState()).toBe('connected');
    });
  });

  describe('Cross-Runtime Discovery', () => {
    let seagActor: SeagActorWrapper;
    let browserActor: BrowserActorWrapper;

    beforeEach(async () => {
      // Register both actors
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

    it('should discover browser actor from SEAG', async () => {
      const actors = await seagActor.discover();

      // Should find at least the browser actor
      expect(actors.length).toBeGreaterThanOrEqual(1);
      const browserActorInfo = actors.find(a => a.address === env.browserAddress);
      expect(browserActorInfo).toBeDefined();
      expect(browserActorInfo?.capabilities).toContain('ui');
    });

    it('should discover SEAG actor from browser', async () => {
      const actors = await browserActor.discover();

      // Should find at least the SEAG actor
      expect(actors.length).toBeGreaterThanOrEqual(1);
      const seagActorInfo = actors.find(a => a.address === env.seagAddress);
      expect(seagActorInfo).toBeDefined();
      expect(seagActorInfo?.capabilities).toContain('compute');
    });

    it('should discover by capability filter', async () => {
      const uiActors = await seagActor.discover('ui');
      expect(uiActors.length).toBeGreaterThanOrEqual(1);
      expect(uiActors.every(a => a.capabilities.includes('ui'))).toBe(true);

      const computeActors = await browserActor.discover('compute');
      expect(computeActors.length).toBeGreaterThanOrEqual(1);
      expect(computeActors.every(a => a.capabilities.includes('compute'))).toBe(true);
    });
  });

  describe('Multiple Actors Discovery', () => {
    const actors: (SeagActorWrapper | BrowserActorWrapper)[] = [];

    afterEach(async () => {
      await Promise.all(actors.map(a => a.disconnect()));
      actors.length = 0;
    });

    it('should discover all registered actors', async () => {
      // Create multiple actors with different capabilities
      const seag1Jwt = await generateSeagActorJWT('@(local/seag-1)' as CanonicalAddress, ['compute']);
      const seag2Jwt = await generateSeagActorJWT('@(local/seag-2)' as CanonicalAddress, ['storage']);
      const browser1Jwt = await generateBrowserActorJWT('@(browser/browser-1)' as CanonicalAddress, ['ui']);
      const browser2Jwt = await generateBrowserActorJWT('@(browser/browser-2)' as CanonicalAddress, ['rendering']);

      const [seag1, seag2, browser1, browser2] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: seag1Jwt,
          actorAddress: '@(local/seag-1)' as CanonicalAddress,
          capabilities: ['compute'],
        }),
        createSeagActor({
          url: env.hub.url,
          jwt: seag2Jwt,
          actorAddress: '@(local/seag-2)' as CanonicalAddress,
          capabilities: ['storage'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser1Jwt,
          actorAddress: '@(browser/browser-1)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browser2Jwt,
          actorAddress: '@(browser/browser-2)' as CanonicalAddress,
          capabilities: ['rendering'],
        }),
      ]);

      actors.push(seag1, seag2, browser1, browser2);

      // Discover all from first actor
      const allActors = await seag1.discover();

      // Should find at least the 3 other actors (may include more from other tests)
      expect(allActors.length).toBeGreaterThanOrEqual(3);

      // Verify specific actors are present
      const addresses = allActors.map(a => a.address);
      expect(addresses).toContain('@(local/seag-2)');
      expect(addresses).toContain('@(browser/browser-1)');
      expect(addresses).toContain('@(browser/browser-2)');
    });

    it('should filter actors by capability', async () => {
      const seag1Jwt = await generateSeagActorJWT('@(local/seag-filter)' as CanonicalAddress, ['compute', 'ml']);
      const seag2Jwt = await generateSeagActorJWT('@(local/seag-filter-2)' as CanonicalAddress, ['storage', 'ml']);
      const browserJwt = await generateBrowserActorJWT('@(browser/browser-filter)' as CanonicalAddress, ['ui']);

      const [seag1, seag2, browser] = await Promise.all([
        createSeagActor({
          url: env.hub.url,
          jwt: seag1Jwt,
          actorAddress: '@(local/seag-filter)' as CanonicalAddress,
          capabilities: ['compute', 'ml'],
        }),
        createSeagActor({
          url: env.hub.url,
          jwt: seag2Jwt,
          actorAddress: '@(local/seag-filter-2)' as CanonicalAddress,
          capabilities: ['storage', 'ml'],
        }),
        createBrowserActor({
          url: env.hub.url,
          jwt: browserJwt,
          actorAddress: '@(browser/browser-filter)' as CanonicalAddress,
          capabilities: ['ui'],
        }),
      ]);

      actors.push(seag1, seag2, browser);

      // Discover actors with 'ml' capability
      const mlActors = await browser.discover('ml');
      expect(mlActors.length).toBeGreaterThanOrEqual(2);
      expect(mlActors.every(a => a.capabilities.includes('ml'))).toBe(true);

      // Verify both ML actors are found
      const mlAddresses = mlActors.map(a => a.address);
      expect(mlAddresses).toContain('@(local/seag-filter)');
      expect(mlAddresses).toContain('@(local/seag-filter-2)');
    });
  });

  describe('Actor Unregistration', () => {
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

    it('should remove actor from discovery after disconnect', async () => {
      // Verify both actors are discoverable
      const actorsBefore = await seagActor.discover();
      expect(actorsBefore.find(a => a.address === env.browserAddress)).toBeDefined();

      // Disconnect browser actor
      await browserActor.disconnect();

      // Give some time for deregistration
      await sleep(500);

      // Browser actor should no longer be discoverable
      const actorsAfter = await seagActor.discover();
      expect(actorsAfter.find(a => a.address === env.browserAddress)).toBeUndefined();
    });
  });
});
