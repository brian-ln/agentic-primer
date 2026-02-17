/**
 * Integration Test Setup for Signal Hub
 *
 * Provides test environment setup and teardown utilities.
 */

import { startSignalHub, type SignalHubInstance } from './helpers/signal-hub.js';
import { generateSeagActorJWT, generateBrowserActorJWT } from './helpers/jwt.js';
import type { CanonicalAddress } from '@agentic-primer/protocols';

export interface TestEnvironment {
  hub: SignalHubInstance;
  seagJwt: string;
  browserJwt: string;
  seagAddress: CanonicalAddress;
  browserAddress: CanonicalAddress;
}

/**
 * Setup test environment with Signal Hub instance and test JWTs
 */
export async function setupTestEnvironment(): Promise<TestEnvironment> {
  // Start Signal Hub instance
  const hub = await startSignalHub();

  // Generate test actor addresses
  const seagAddress = '@(local/test-seag)' as CanonicalAddress;
  const browserAddress = '@(browser/test-browser)' as CanonicalAddress;

  // Generate JWTs
  const seagJwt = await generateSeagActorJWT(seagAddress, ['compute', 'inference']);
  const browserJwt = await generateBrowserActorJWT(browserAddress, ['ui', 'interaction']);

  return {
    hub,
    seagJwt,
    browserJwt,
    seagAddress,
    browserAddress,
  };
}

/**
 * Teardown test environment
 */
export async function teardownTestEnvironment(env: TestEnvironment): Promise<void> {
  await env.hub.stop();
}

/**
 * Helper to wait for a condition with timeout
 */
export async function waitFor(
  condition: () => boolean | Promise<boolean>,
  timeout: number = 5000,
  interval: number = 100
): Promise<void> {
  const startTime = Date.now();

  while (Date.now() - startTime < timeout) {
    const result = await condition();
    if (result) {
      return;
    }
    await new Promise(resolve => setTimeout(resolve, interval));
  }

  throw new Error(`Timeout waiting for condition (${timeout}ms)`);
}

/**
 * Helper to wait for specific duration
 */
export async function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}
