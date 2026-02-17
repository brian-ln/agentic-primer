#!/usr/bin/env node
/**
 * Debug script to test spawn behavior with different stdio configurations
 */
import { spawn } from 'child_process';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const TEST_PORT = 9002;
const signalHubDir = join(__dirname, '../../../services/signal-hub');

console.log('Testing different stdio configurations...\n');

async function testConfig(name, stdioConfig) {
  console.log(`\n=== Testing: ${name} ===`);

  return new Promise((resolve) => {
    const proc = spawn('npx', [
      'wrangler',
      'dev',
      '--port', TEST_PORT.toString(),
      '--var', 'AUTH_ENABLED:false',
    ], {
      cwd: signalHubDir,
      stdio: stdioConfig,
      env: process.env,
    });

    let foundReady = false;
    const timeout = setTimeout(() => {
      console.log(`❌ TIMEOUT - "Ready on" not detected within 8 seconds`);
      proc.kill('SIGTERM');
      resolve(false);
    }, 8000);

    const checkReady = (data) => {
      const output = data.toString();
      console.log(`  Output chunk: ${output.substring(0, 100)}...`);
      if (output.includes('Ready on') || output.includes(`localhost:${TEST_PORT}`)) {
        console.log(`✅ SUCCESS - Found "Ready on" message!`);
        clearTimeout(timeout);
        foundReady = true;
        proc.kill('SIGTERM');
        setTimeout(() => resolve(true), 500);
      }
    };

    if (proc.stdout) {
      proc.stdout.on('data', (data) => {
        console.log('  [stdout event]');
        checkReady(data);
      });
    }

    if (proc.stderr) {
      proc.stderr.on('data', (data) => {
        console.log('  [stderr event]');
        checkReady(data);
      });
    }

    proc.on('error', (error) => {
      console.log(`  [error event]: ${error.message}`);
    });

    proc.on('exit', () => {
      if (!foundReady) {
        clearTimeout(timeout);
        resolve(false);
      }
    });
  });
}

// Test configurations
(async () => {
  const configs = [
    ['inherit (current)', ['ignore', 'inherit', 'inherit']],
    ['pipe (test setup)', ['ignore', 'pipe', 'pipe']],
    ['pipe with setEncoding', ['ignore', 'pipe', 'pipe']],
  ];

  for (const [name, stdio] of configs) {
    await testConfig(name, stdio);
    await new Promise(r => setTimeout(r, 2000)); // Wait between tests
  }

  console.log('\n=== Tests Complete ===\n');
  process.exit(0);
})();
