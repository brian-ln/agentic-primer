#!/usr/bin/env bun
/**
 * Full system integration test
 */

import { DaemonActor } from './src/daemon.js';

console.log('=== Full Event System Test ===\n');

// Test 1: Start Daemon
console.log('Test 1: Starting Daemon with all actors');
console.log('----------------------------------------');

const daemon = new DaemonActor('./config.json');
const startResult = await daemon.start();

if (!startResult.success) {
  console.error('❌ Failed to start daemon:', startResult.error);
  process.exit(1);
}

console.log('✓ Daemon started successfully');
console.log('✓ State:', startResult.state);

// Test 2: Check Status
console.log('\n\nTest 2: Checking Daemon Status');
console.log('-------------------------------');

const status = daemon.getStatus();
console.log('✓ Daemon status:');
console.log('  - State:', status.state);
console.log('  - PID:', status.pid);
console.log('  - Uptime:', status.uptime + 'ms');
console.log('  - Actors:', status.actors);

// Test 3: Test EventLogActor through daemon
console.log('\n\nTest 3: Testing EventLogActor');
console.log('-----------------------------');

if (daemon.actors.eventLog) {
  const eventLogStatus = daemon.actors.eventLog.getStatus();
  console.log('✓ EventLogActor status:', eventLogStatus);

  // Emit a test event
  const { createMessage, PROTOCOLS, ACTIONS } = await import('./src/protocol.js');
  const testEvent = {
    type: 'system.test',
    data: { message: 'Full system test event' }
  };

  const emitResult = await daemon.actors.eventLog.handleMessage(
    createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, testEvent)
  );

  console.log('✓ Event emitted:', emitResult.eventId);
  console.log('✓ Event count:', emitResult.eventCount);
} else {
  console.log('⚠ EventLogActor not spawned');
}

// Test 4: Graceful Shutdown
console.log('\n\nTest 4: Graceful Shutdown');
console.log('-------------------------');

const stopResult = await daemon.stop();
if (stopResult.success) {
  console.log('✓ Daemon stopped gracefully');
  console.log('✓ Final state:', stopResult.state);
} else {
  console.error('❌ Failed to stop daemon:', stopResult.error);
  process.exit(1);
}

console.log('\n=== Full System Test Passed! ===');

process.exit(0);
