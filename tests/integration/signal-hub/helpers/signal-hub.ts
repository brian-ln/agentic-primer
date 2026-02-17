/**
 * Signal Hub Test Instance Manager
 *
 * Manages local Signal Hub instance for integration tests using wrangler dev.
 */

import { spawn } from 'child_process';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const TEST_PORT = 9000;

export interface SignalHubInstance {
  url: string;
  port: number;
  process: any;
  stop: () => Promise<void>;
}

/**
 * Start a local Signal Hub instance for testing using wrangler dev
 */
export async function startSignalHub(): Promise<SignalHubInstance> {
  const signalHubDir = join(__dirname, '../../../../services/signal-hub');

  // Spawn wrangler dev on test port
  const wranglerProcess = spawn('pnpm', ['wrangler', 'dev', '--port', TEST_PORT.toString()], {
    cwd: signalHubDir,
    stdio: ['ignore', 'pipe', 'pipe'],
    env: {
      ...process.env,
      AUTH_ENABLED: 'false',
    },
  });

  // Wait for server to be ready
  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error('Signal Hub failed to start within 10 seconds'));
    }, 10000);

    wranglerProcess.stdout.on('data', (data: Buffer) => {
      const output = data.toString();
      if (output.includes('Ready on') || output.includes(`localhost:${TEST_PORT}`)) {
        clearTimeout(timeout);
        resolve();
      }
    });

    wranglerProcess.stderr.on('data', (data: Buffer) => {
      console.error('Signal Hub stderr:', data.toString());
    });

    wranglerProcess.on('error', (error) => {
      clearTimeout(timeout);
      reject(error);
    });
  });

  // Give it a moment to fully initialize
  await new Promise(resolve => setTimeout(resolve, 1000));

  return {
    url: `ws://localhost:${TEST_PORT}/ws`,
    port: TEST_PORT,
    process: wranglerProcess,
    stop: async () => {
      wranglerProcess.kill('SIGTERM');
      // Wait for process to exit
      await new Promise<void>((resolve) => {
        wranglerProcess.on('exit', () => resolve());
        setTimeout(() => {
          wranglerProcess.kill('SIGKILL');
          resolve();
        }, 2000);
      });
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
