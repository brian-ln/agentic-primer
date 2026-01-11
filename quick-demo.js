#!/usr/bin/env bun
/**
 * Quick demonstration of all key capabilities
 */

import { DaemonActor } from './src/daemon.js';
import { createMessage, PROTOCOLS, ACTIONS } from './src/protocol.js';

console.log('🚀 Event System Quick Demo\n');

// Start daemon
const daemon = new DaemonActor();
await daemon.start();
console.log('✓ Daemon started');

// Get actor status
const eventLog = daemon.actors.eventLog;
console.log('✓ EventLogActor status:', eventLog.getStatus());

// Emit 3 events
console.log('\n📝 Emitting events...');
for (let i = 1; i <= 3; i++) {
  const result = await eventLog.handleMessage(
    createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
      type: `demo.event.${i}`,
      data: { number: i, message: `Event ${i}` }
    })
  );
  console.log(`✓ Event ${i} emitted:`, result.eventId);
}

// Query events
console.log('\n🔍 Querying events...');
const query = await eventLog.handleMessage(
  createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, { limit: 5, reverse: true })
);
console.log(`✓ Found ${query.count} recent events:`);
query.events.forEach((e, i) => {
  console.log(`  ${i + 1}. [${e.type}] ${e.data.message || JSON.stringify(e.data)}`);
});

// Create checkpoint
console.log('\n📍 Creating checkpoint...');
const checkpoint = await eventLog.handleMessage(
  createMessage(PROTOCOLS.EVENT, ACTIONS.CHECKPOINT, {})
);
console.log('✓ Checkpoint created at event count:', checkpoint.checkpoint);

// Stop daemon
console.log('\n🛑 Stopping daemon...');
await daemon.stop();
console.log('✓ Daemon stopped gracefully');

console.log('\n✅ All capabilities working!\n');
