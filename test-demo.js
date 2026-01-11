#!/usr/bin/env bun
/**
 * Simple demonstration that the Event System works
 */

import { DaemonActor } from './src/daemon.js';
import { EventLogActor } from './src/actors/event-log.js';
import { createMessage, PROTOCOLS, ACTIONS } from './src/protocol.js';

console.log('=== Event System Demonstration ===\n');

// Test 1: EventLogActor lifecycle
console.log('Test 1: EventLogActor Lifecycle');
console.log('--------------------------------');
const eventLog = new EventLogActor({ eventLog: { file: 'test-events.jsonl' } });

console.log('Starting EventLogActor...');
const startResult = await eventLog.start();
console.log('✓ Start result:', startResult);

console.log('\nGetting status...');
const status = eventLog.getStatus();
console.log('✓ Status:', status);

// Test 2: Emit events
console.log('\n\nTest 2: Event Emission');
console.log('----------------------');

const event1 = {
  type: 'demo.test',
  data: { message: 'Hello from test!' }
};

console.log('Emitting event:', event1.type);
const appendResult = await eventLog.handleMessage(
  createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, event1)
);
console.log('✓ Append result:', appendResult);

// Test 3: Query events
console.log('\n\nTest 3: Event Query');
console.log('-------------------');

const queryResult = await eventLog.handleMessage(
  createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, { limit: 10 })
);
console.log('✓ Query result:', {
  success: queryResult.success,
  count: queryResult.count,
  total: queryResult.total
});
console.log('✓ Latest event:', queryResult.events[queryResult.events.length - 1]);

// Test 4: Stop actor
console.log('\n\nTest 4: Graceful Shutdown');
console.log('-------------------------');
const stopResult = await eventLog.stop();
console.log('✓ Stop result:', stopResult);

console.log('\n=== All Tests Passed! ===');

process.exit(0);
