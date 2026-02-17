#!/usr/bin/env node
/**
 * Debug script to test wrangler output capture
 */
import { spawn } from 'child_process';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const TEST_PORT = 9001; // Use different port to avoid conflict
const signalHubDir = join(__dirname, '../../../services/signal-hub');

console.log('Starting wrangler with stdio pipes...');
console.log(`Working directory: ${signalHubDir}`);
console.log(`Port: ${TEST_PORT}`);

const wranglerProcess = spawn('npx', [
  'wrangler',
  'dev',
  '--port', TEST_PORT.toString(),
  '--var', 'AUTH_ENABLED:false',
], {
  cwd: signalHubDir,
  stdio: ['ignore', 'pipe', 'pipe'],
  env: process.env,
});

console.log('\n=== Listening for output ===\n');

wranglerProcess.stdout.on('data', (data) => {
  console.log('[STDOUT]', data.toString().trim());
});

wranglerProcess.stderr.on('data', (data) => {
  console.log('[STDERR]', data.toString().trim());
});

wranglerProcess.on('error', (error) => {
  console.error('[ERROR]', error);
});

wranglerProcess.on('exit', (code, signal) => {
  console.log(`[EXIT] code=${code}, signal=${signal}`);
});

// Stop after 15 seconds
setTimeout(() => {
  console.log('\n=== Stopping wrangler ===\n');
  wranglerProcess.kill('SIGTERM');
  setTimeout(() => {
    process.exit(0);
  }, 2000);
}, 15000);
