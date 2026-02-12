#!/usr/bin/env bun
/**
 * Benchmark: Messaging Patterns and Throughput
 *
 * Measures high-level messaging patterns, throughput, and memory stability.
 * Target: >10k messages/sec, stable memory over 10k messages
 */

import { bench, run } from 'mitata';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import { ActorSystem, address } from '../index.ts';

// Setup: Create actor system
const store = new GraphStore();
const programManager = new ProgramManager(store);
const actorSystem = new ActorSystem(store, programManager);

// Create benchmark programs
await programManager.createProgram(
  'counter',
  `
  // Counter program - tracks invocation count
  const count = (this.state?.count || 0) + 1;
  this.state = { count };
  return count;
  `,
  {
    name: 'Counter',
    description: 'Counts invocations',
  }
);
await programManager.publishProgram('counter');

await programManager.createProgram(
  'fast-echo',
  `
  // Fast echo - minimal processing
  return input.message;
  `,
  {
    name: 'Fast Echo',
    description: 'High-speed echo',
  }
);
await programManager.publishProgram('fast-echo');

await programManager.createProgram(
  'aggregator',
  `
  // Aggregator - collects items
  const items = this.state?.items || [];
  items.push(input.message);
  this.state = { items };
  return { count: items.length };
  `,
  {
    name: 'Aggregator',
    description: 'Aggregates messages',
  }
);
await programManager.publishProgram('aggregator');

// Pattern: Concurrent tell operations
bench('concurrent: 10 tell operations', async () => {
  const promises = [];
  for (let i = 0; i < 10; i++) {
    promises.push(
      actorSystem.tell(address('fast-echo'), 'echo', { index: i })
    );
  }
  await Promise.all(promises);
});

bench('concurrent: 100 tell operations', async () => {
  const promises = [];
  for (let i = 0; i < 100; i++) {
    promises.push(
      actorSystem.tell(address('fast-echo'), 'echo', { index: i })
    );
  }
  await Promise.all(promises);
});

// Pattern: Concurrent ask operations
bench('concurrent: 10 ask operations', async () => {
  const promises = [];
  for (let i = 0; i < 10; i++) {
    promises.push(
      actorSystem.send(address('fast-echo'), 'echo', { index: i })
    );
  }
  await Promise.all(promises);
});

bench('concurrent: 100 ask operations', async () => {
  const promises = [];
  for (let i = 0; i < 100; i++) {
    promises.push(
      actorSystem.send(address('fast-echo'), 'echo', { index: i })
    );
  }
  await Promise.all(promises);
});

// Pattern: Mixed tell/ask
bench('mixed: 50 tell + 50 ask operations', async () => {
  const promises = [];
  for (let i = 0; i < 50; i++) {
    promises.push(
      actorSystem.tell(address('fast-echo'), 'echo', { index: i })
    );
    promises.push(
      actorSystem.send(address('fast-echo'), 'echo', { index: i })
    );
  }
  await Promise.all(promises);
});

// Throughput: 1k messages
bench('throughput: 1k messages (sequential)', async () => {
  for (let i = 0; i < 1000; i++) {
    await actorSystem.send(address('fast-echo'), 'echo', { index: i });
  }
});

bench('throughput: 1k messages (batches of 100)', async () => {
  for (let batch = 0; batch < 10; batch++) {
    const promises = [];
    for (let i = 0; i < 100; i++) {
      promises.push(
        actorSystem.send(address('fast-echo'), 'echo', {
          batch,
          index: i,
        })
      );
    }
    await Promise.all(promises);
  }
});

// Throughput: 10k messages (memory stability test)
bench('throughput: 10k messages (batches of 100)', async () => {
  const batchSize = 100;
  const totalMessages = 10000;
  const batches = totalMessages / batchSize;

  for (let batch = 0; batch < batches; batch++) {
    const promises = [];
    for (let i = 0; i < batchSize; i++) {
      promises.push(
        actorSystem.send(address('fast-echo'), 'echo', {
          batch,
          index: i,
        })
      );
    }
    await Promise.all(promises);
  }
});

// Actor-to-actor communication pattern
bench('actor-to-actor: 10 chained messages', async () => {
  const counterActor = actorSystem.actor('counter');
  const echoActor = actorSystem.actor('fast-echo');

  for (let i = 0; i < 10; i++) {
    // Counter increments
    await counterActor.ask(address('counter'), 'increment', {});
    // Echo confirms
    await echoActor.ask(address('fast-echo'), 'echo', { i });
  }
});

// Fan-out pattern: One message triggers multiple responses
bench('fan-out: 1 to 10 actors', async () => {
  const promises = [];
  for (let i = 0; i < 10; i++) {
    promises.push(
      actorSystem.send(address('fast-echo'), 'echo', { fanout: i })
    );
  }
  await Promise.all(promises);
});

// Aggregation pattern: Multiple messages to one actor
bench('aggregation: 100 messages to aggregator', async () => {
  // Create fresh aggregator for this benchmark
  const aggregatorActor = actorSystem.actor('aggregator');

  const promises = [];
  for (let i = 0; i < 100; i++) {
    promises.push(
      aggregatorActor.ask(address('aggregator'), 'add', { value: i })
    );
  }
  await Promise.all(promises);
});

// Memory test: Create and discard many messages
bench('memory: create 1k messages (no send)', () => {
  const messages = [];
  for (let i = 0; i < 1000; i++) {
    messages.push({ index: i, data: 'test payload', timestamp: Date.now() });
  }
  // Let GC clean up
});

// Run benchmarks if this is the main module
if (import.meta.main) {
  console.log('📊 Messaging Patterns Benchmarks\n');
  console.log('Targets:');
  console.log('  • Throughput: >10k messages/second');
  console.log('  • Memory: Stable over 10k messages');
  console.log('  • Concurrency: Handle 100+ concurrent operations\n');

  // Capture initial memory
  const initialMemory = process.memoryUsage();
  console.log('Initial Memory:');
  console.log(`  RSS: ${(initialMemory.rss / 1024 / 1024).toFixed(2)} MB`);
  console.log(
    `  Heap Used: ${(initialMemory.heapUsed / 1024 / 1024).toFixed(2)} MB\n`
  );

  await run();

  // Capture final memory
  const finalMemory = process.memoryUsage();
  console.log('\nFinal Memory:');
  console.log(`  RSS: ${(finalMemory.rss / 1024 / 1024).toFixed(2)} MB`);
  console.log(
    `  Heap Used: ${(finalMemory.heapUsed / 1024 / 1024).toFixed(2)} MB`
  );

  const heapGrowth = finalMemory.heapUsed - initialMemory.heapUsed;
  console.log(
    `\nHeap Growth: ${(heapGrowth / 1024 / 1024).toFixed(2)} MB`
  );

  if (heapGrowth > 50 * 1024 * 1024) {
    console.log('⚠️  Warning: Heap grew by >50MB, possible memory leak');
  } else {
    console.log('✓ Memory stable');
  }
}
