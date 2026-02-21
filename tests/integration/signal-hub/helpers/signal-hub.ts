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

  console.log('[startSignalHub] Starting wrangler dev...', { port: TEST_PORT, cwd: signalHubDir });

  // Spawn wrangler dev on test port with AUTH_ENABLED=false
  const wranglerProcess = spawn('npx', [
    'wrangler',
    'dev',
    '--port', TEST_PORT.toString(),
    '--var', 'AUTH_ENABLED:false',
  ], {
    cwd: signalHubDir,
    stdio: ['ignore', 'pipe', 'pipe'],
    env: { ...process.env, FORCE_COLOR: '0', NODE_ENV: 'test' }, // Disable color codes and set test env
    shell: false,
    detached: true, // Create new process group so we can kill the whole tree
  });

  console.log('[startSignalHub] Process spawned, PID:', wranglerProcess.pid);

  // Set encoding on streams for proper text handling
  wranglerProcess.stdout?.setEncoding('utf8');
  wranglerProcess.stderr?.setEncoding('utf8');

  // Collect stderr for error detection
  const outputChunks: string[] = [];
  wranglerProcess.stderr.on('data', (data: Buffer | string) => {
    const text = typeof data === 'string' ? data : data.toString();
    outputChunks.push(text);
    if (text.includes('Address already in use')) {
      console.error(`[startSignalHub] Port ${TEST_PORT} is already in use`);
    }
  });
  wranglerProcess.stdout.on('data', (data: Buffer | string) => {
    const text = typeof data === 'string' ? data : data.toString();
    outputChunks.push(text);
  });

  wranglerProcess.on('error', (error) => {
    console.error('[startSignalHub] Process error:', error);
  });

  // Wait for server to be ready by polling the health endpoint.
  // wrangler prints "Ready on" BEFORE Miniflare actually binds the port, so
  // we can't rely on stdout. Instead, poll HTTP until we get a 200 response.
  // This also triggers the "⎔ Reloading local server" compile cycle, so
  // WebSocket upgrades succeed immediately after this completes.
  const STARTUP_TIMEOUT_MS = 30_000;
  const POLL_INTERVAL_MS = 500;
  const startTime = Date.now();

  console.log(`[startSignalHub] Polling http://localhost:${TEST_PORT}/health until ready...`);

  while (true) {
    const elapsed = Date.now() - startTime;

    if (elapsed >= STARTUP_TIMEOUT_MS) {
      const combinedOutput = outputChunks.join('');
      if (combinedOutput.includes('Address already in use')) {
        throw new Error(`Port ${TEST_PORT} is already in use. Please stop any existing wrangler instances.`);
      }
      throw new Error(`Signal Hub failed to start within ${STARTUP_TIMEOUT_MS / 1000}s`);
    }

    // Check if wrangler process exited early
    if (wranglerProcess.exitCode !== null && wranglerProcess.exitCode !== 0) {
      const combinedOutput = outputChunks.join('');
      const errorMsg = combinedOutput.includes('Address already in use')
        ? `Port ${TEST_PORT} is already in use`
        : `Wrangler process exited with code ${wranglerProcess.exitCode}`;
      throw new Error(errorMsg);
    }

    try {
      const resp = await fetch(`http://localhost:${TEST_PORT}/health`);
      if (resp.ok) {
        console.log(`[startSignalHub] ✓ Server ready and warmed up on port ${TEST_PORT}`);
        break;
      }
    } catch {
      // Not ready yet — connection refused or network error
    }

    await new Promise(resolve => setTimeout(resolve, POLL_INTERVAL_MS));
  }

  // Extra settle time: after the health check triggers the Miniflare "Reloading"
  // cycle, wait for it to fully stabilize before WebSocket connections are attempted.
  await new Promise(resolve => setTimeout(resolve, 2000));

  return {
    url: `ws://localhost:${TEST_PORT}/ws`,
    port: TEST_PORT,
    process: wranglerProcess,
    stop: async () => {
      console.log('[stopSignalHub] Stopping wrangler, PID:', wranglerProcess.pid);

      // Kill the process group to ensure all child processes are terminated
      if (wranglerProcess.pid) {
        try {
          // Kill the entire process group (negative PID)
          process.kill(-wranglerProcess.pid, 'SIGTERM');
        } catch (err) {
          // Process might have already exited
          console.log('[stopSignalHub] SIGTERM failed:', err);
        }
      }

      wranglerProcess.kill('SIGTERM');

      // Wait for process to exit
      await new Promise<void>((resolve) => {
        let exitHandlerSet = false;
        wranglerProcess.on('exit', () => {
          if (!exitHandlerSet) {
            exitHandlerSet = true;
            resolve();
          }
        });

        setTimeout(() => {
          if (wranglerProcess.pid) {
            try {
              // Force kill the entire process group
              process.kill(-wranglerProcess.pid, 'SIGKILL');
            } catch {
              // Ignore errors
            }
          }
          wranglerProcess.kill('SIGKILL');
          if (!exitHandlerSet) {
            exitHandlerSet = true;
            resolve();
          }
        }, 2000);
      });

      // Additional cleanup: wait a bit for port to be released
      await new Promise(resolve => setTimeout(resolve, 500));
      console.log('[stopSignalHub] Cleanup complete');
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
