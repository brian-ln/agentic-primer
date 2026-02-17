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

  // Wait for server to be ready
  await new Promise<void>((resolve, reject) => {
    let outputReceived = false;
    const outputChunks: string[] = [];
    let resolved = false;

    const timeout = setTimeout(() => {
      if (resolved) return;
      console.error('[startSignalHub] TIMEOUT - Process did not emit "Ready on" within 10 seconds');
      console.error('[startSignalHub] Output received:', outputReceived);
      console.error('[startSignalHub] Total chunks:', outputChunks.length);
      console.error('[startSignalHub] Combined output:', outputChunks.join('').substring(0, 500));
      reject(new Error('Signal Hub failed to start within 10 seconds'));
    }, 10000);

    const checkReady = (source: string) => (data: Buffer | string) => {
      outputReceived = true;
      const output = typeof data === 'string' ? data : data.toString();
      outputChunks.push(output);

      // Check for port conflict error
      if (output.includes('Address already in use')) {
        clearTimeout(timeout);
        resolved = true;
        reject(new Error(`Port ${TEST_PORT} is already in use. Please stop any existing wrangler instances.`));
        return;
      }

      // Check for any other error messages (only log actual errors)
      if (output.includes('ERROR') && !resolved) {
        console.error(`[startSignalHub] [${source}] Error detected:`, output.substring(0, 200));
      }

      if (output.includes('Ready on') || output.includes(`localhost:${TEST_PORT}`)) {
        console.log(`[startSignalHub] ✓ Server ready on port ${TEST_PORT}`);
        clearTimeout(timeout);
        resolved = true;
        resolve();
      }
    };

    wranglerProcess.stdout.on('data', checkReady('stdout'));
    wranglerProcess.stderr.on('data', checkReady('stderr'));

    wranglerProcess.on('error', (error) => {
      if (resolved) return;
      console.error('[startSignalHub] Process error:', error);
      clearTimeout(timeout);
      resolved = true;
      reject(error);
    });

    wranglerProcess.on('exit', (code, signal) => {
      if (resolved) return;
      if (code !== null && code !== 0) {
        const combinedOutput = outputChunks.join('');
        const errorMsg = combinedOutput.includes('Address already in use')
          ? `Port ${TEST_PORT} is already in use`
          : `Wrangler process exited with code ${code}`;

        console.error('[startSignalHub] Process exited prematurely:', { code, signal });
        console.error('[startSignalHub] Output:', combinedOutput.substring(0, 500));
        clearTimeout(timeout);
        resolved = true;
        reject(new Error(errorMsg));
      }
    });
  });

  // Give it a moment to fully initialize
  await new Promise(resolve => setTimeout(resolve, 1000));

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
