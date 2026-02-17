/**
 * Signal Hub Test Instance Manager
 *
 * Manages local Signal Hub instance for integration tests using Miniflare.
 */

import { Miniflare } from 'miniflare';
import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export interface SignalHubInstance {
  url: string;
  port: number;
  miniflare: Miniflare;
  stop: () => Promise<void>;
}

/**
 * Start a local Signal Hub instance for testing
 */
export async function startSignalHub(): Promise<SignalHubInstance> {
  const signalHubDir = join(__dirname, '../../../../services/signal-hub');

  // Read wrangler.toml to extract configuration
  const wranglerPath = join(signalHubDir, 'wrangler.toml');
  const wranglerToml = readFileSync(wranglerPath, 'utf-8');

  const mf = new Miniflare({
    scriptPath: join(signalHubDir, 'src/index.ts'),
    modules: true,
    modulesRules: [
      { type: 'ESModule', include: ['**/*.ts'], fallthrough: true },
    ],
    compatibilityDate: '2024-12-01',
    compatibilityFlags: ['nodejs_compat'],
    durableObjects: {
      SIGNAL_HUB: 'SignalHub',
    },
    bindings: {
      PROTOCOL_VERSION: '0.1.0',
      MAX_MESSAGE_SIZE: '1048576',
      HEARTBEAT_INTERVAL: '30000',
      ACTOR_REGISTRY_LIMIT: '50000',
      DEFAULT_ACTOR_TTL: '300000',
      MAX_ACTOR_TTL: '3600000',
      BROADCAST_SYNC_THRESHOLD: '100',
      JWT_SECRET: 'dev-secret-change-in-production',
      AUTH_ENABLED: 'false',
    },
    // Use random port for parallel test execution
    port: 0,
  });

  // Get assigned port
  const url = await mf.ready;
  const port = new URL(url).port;

  return {
    url: `ws://localhost:${port}/ws`,
    port: parseInt(port),
    miniflare: mf,
    stop: async () => {
      await mf.dispose();
    },
  };
}

/**
 * Helper to wait for Signal Hub to be ready
 */
export async function waitForSignalHub(url: string, maxAttempts: number = 10): Promise<boolean> {
  for (let i = 0; i < maxAttempts; i++) {
    try {
      const httpUrl = url.replace('ws://', 'http://').replace('/ws', '/health');
      const response = await fetch(httpUrl);
      if (response.ok) {
        return true;
      }
    } catch {
      // Ignore connection errors, retry
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  return false;
}
