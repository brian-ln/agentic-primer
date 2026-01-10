#!/usr/bin/env bun
/**
 * CLI for Event System
 *
 * Commands:
 * - daemon start|stop|status
 * - emit <event-json>
 * - help
 */

import { resolve } from 'node:path';
import { spawn } from 'node:child_process';
import { writeFile, readFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';

const PID_FILE = '.daemon.pid';
const LOG_FILE = 'daemon.log';

/**
 * Parse CLI arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    return { command: 'help' };
  }

  const [command, ...rest] = args;

  return {
    command,
    args: rest
  };
}

/**
 * Show help text
 */
function showHelp() {
  console.log(`
Event System CLI

USAGE:
  event-system <command> [arguments]

COMMANDS:
  daemon start          Start the daemon
  daemon stop           Stop the daemon
  daemon status         Show daemon status
  emit <json>           Emit an event
  help                  Show this help

EXAMPLES:
  # Start daemon
  event-system daemon start

  # Check status
  event-system daemon status

  # Emit event
  event-system emit '{"type":"test","data":{"message":"hello"}}'

  # Stop daemon
  event-system daemon stop
  `);
}

/**
 * Start daemon
 */
async function startDaemon() {
  // Check if already running
  if (existsSync(PID_FILE)) {
    const pid = parseInt(await readFile(PID_FILE, 'utf-8'), 10);

    try {
      process.kill(pid, 0); // Check if process exists
      console.error('❌ Daemon already running (PID: ' + pid + ')');
      process.exit(1);
    } catch (e) {
      // Process doesn't exist, remove stale PID file
      console.log('⚠️  Removing stale PID file');
    }
  }

  console.log('🚀 Starting daemon...');

  const daemonPath = resolve('./src/daemon.js');
  const logStream = Bun.file(LOG_FILE).writer();

  const daemon = spawn('bun', ['run', daemonPath], {
    detached: true,
    stdio: ['ignore', 'pipe', 'pipe']
  });

  // Pipe stdout/stderr to log file
  daemon.stdout?.on('data', (data) => {
    logStream.write(data);
  });

  daemon.stderr?.on('data', (data) => {
    logStream.write(data);
  });

  daemon.on('error', (err) => {
    console.error('❌ Failed to start daemon:', err.message);
    process.exit(1);
  });

  // Save PID
  await writeFile(PID_FILE, daemon.pid.toString());

  daemon.unref();

  console.log('✅ Daemon started (PID: ' + daemon.pid + ')');
  console.log('📝 Logs: ' + LOG_FILE);
}

/**
 * Stop daemon
 */
async function stopDaemon() {
  if (!existsSync(PID_FILE)) {
    console.error('❌ Daemon not running (no PID file)');
    process.exit(1);
  }

  const pid = parseInt(await readFile(PID_FILE, 'utf-8'), 10);

  try {
    process.kill(pid, 0); // Check if process exists

    console.log('🛑 Stopping daemon (PID: ' + pid + ')...');

    process.kill(pid, 'SIGTERM');

    // Wait for process to exit
    let attempts = 0;
    while (attempts < 10) {
      try {
        process.kill(pid, 0);
        await new Promise(resolve => setTimeout(resolve, 100));
        attempts++;
      } catch (e) {
        // Process exited
        break;
      }
    }

    if (attempts === 10) {
      console.log('⚠️  Daemon did not stop gracefully, forcing...');
      process.kill(pid, 'SIGKILL');
    }

    // Remove PID file
    await Bun.write(PID_FILE, '');

    console.log('✅ Daemon stopped');
  } catch (e) {
    console.error('❌ Daemon not running (PID: ' + pid + ' not found)');
    await Bun.write(PID_FILE, ''); // Clean up stale PID file
    process.exit(1);
  }
}

/**
 * Show daemon status
 */
async function showStatus() {
  if (!existsSync(PID_FILE)) {
    console.log('Status: stopped');
    return;
  }

  const pid = parseInt(await readFile(PID_FILE, 'utf-8'), 10);

  try {
    process.kill(pid, 0); // Check if process exists
    console.log('Status: running');
    console.log('PID: ' + pid);
  } catch (e) {
    console.log('Status: stopped (stale PID file)');
  }
}

/**
 * Emit event
 */
async function emitEvent(eventJson) {
  if (!eventJson) {
    console.error('❌ Missing event JSON');
    console.log('Usage: event-system emit \'{"type":"test","data":{}}\'');
    process.exit(1);
  }

  try {
    const event = JSON.parse(eventJson);

    // Check if daemon is running
    if (!existsSync(PID_FILE)) {
      console.error('❌ Daemon not running. Start it first with: event-system daemon start');
      process.exit(1);
    }

    // For MVP, write directly to events.jsonl
    // In full version, this would go through HTTP API
    const { appendEvent } = await import('./actors/event-log.js');

    const enrichedEvent = {
      ...event,
      id: generateId(),
      timestamp: new Date().toISOString(),
      source: 'cli',
      depth: 0
    };

    await appendEvent(enrichedEvent);

    console.log('✅ Event emitted:', enrichedEvent.id);
  } catch (e) {
    console.error('❌ Failed to emit event:', e.message);
    process.exit(1);
  }
}

/**
 * Generate simple ID (ULID would be better)
 */
function generateId() {
  return Date.now().toString(36) + Math.random().toString(36).substr(2);
}

/**
 * Main CLI handler
 */
async function main() {
  const { command, args } = parseArgs();

  try {
    switch (command) {
      case 'daemon':
        const daemonCmd = args[0];
        switch (daemonCmd) {
          case 'start':
            await startDaemon();
            break;
          case 'stop':
            await stopDaemon();
            break;
          case 'status':
            await showStatus();
            break;
          default:
            console.error('❌ Unknown daemon command:', daemonCmd);
            console.log('Valid commands: start, stop, status');
            process.exit(1);
        }
        break;

      case 'emit':
        await emitEvent(args.join(' '));
        break;

      case 'help':
      case '--help':
      case '-h':
        showHelp();
        break;

      default:
        console.error('❌ Unknown command:', command);
        showHelp();
        process.exit(1);
    }
  } catch (e) {
    console.error('❌ Error:', e.message);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.main) {
  main();
}
