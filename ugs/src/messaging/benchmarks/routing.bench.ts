#!/usr/bin/env bun
/**
 * Benchmark: Message Routing Latency
 *
 * Measures the performance of Router.tell() and Router.ask() operations.
 * Target: P95 < 100µs for routing latency (excluding program execution)
 */

import { bench, run } from 'mitata';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import { MessageRouter } from '../router.ts';
import { createMessage, address, generateCorrelationId } from '../message.ts';

// Setup: Create store, programs, and router
const store = new GraphStore();
const programManager = new ProgramManager(store);
const router = new MessageRouter(store, programManager);

// Create a simple echo program for benchmarking
await programManager.createProgram(
  'bench-echo',
  `
  // Simple echo - minimal overhead
  return input.message;
  `,
  {
    name: 'Benchmark Echo',
    description: 'Echo program for benchmarking',
  }
);
await programManager.publishProgram('bench-echo');

// Create a noop program (no return value)
await programManager.createProgram(
  'bench-noop',
  `
  // No-op - absolute minimal overhead
  return null;
  `,
  {
    name: 'Benchmark Noop',
    description: 'No-op program for benchmarking',
  }
);
await programManager.publishProgram('bench-noop');

// Create a computation program
await programManager.createProgram(
  'bench-compute',
  `
  // Simple computation
  const { a, b } = input.message;
  return a + b;
  `,
  {
    name: 'Benchmark Compute',
    description: 'Computation program for benchmarking',
  }
);
await programManager.publishProgram('bench-compute');

// Benchmarks
bench('Router.tell() to echo program', async () => {
  const msg = createMessage(
    address('bench-echo'),
    'echo',
    { data: 'test' },
    { pattern: 'tell' }
  );
  await router.tell(msg);
});

bench('Router.ask() to echo program', async () => {
  const msg = createMessage(
    address('bench-echo'),
    'echo',
    { data: 'test' },
    {
      pattern: 'ask',
      correlationId: generateCorrelationId(),
    }
  );
  await router.ask(msg);
});

bench('Router.ask() to noop program (minimal)', async () => {
  const msg = createMessage(
    address('bench-noop'),
    'noop',
    {},
    {
      pattern: 'ask',
      correlationId: generateCorrelationId(),
    }
  );
  await router.ask(msg);
});

bench('Router.ask() to compute program', async () => {
  const msg = createMessage(
    address('bench-compute'),
    'compute',
    { a: 5, b: 3 },
    {
      pattern: 'ask',
      correlationId: generateCorrelationId(),
    }
  );
  await router.ask(msg);
});

// Round-trip latency measurement
bench('ask round-trip (echo)', async () => {
  const msg = createMessage(
    address('bench-echo'),
    'echo',
    { timestamp: Date.now() },
    {
      pattern: 'ask',
      from: address('benchmark'),
      correlationId: generateCorrelationId(),
    }
  );
  const response = await router.ask(msg);
  // Verify successful round-trip
  if (!response.success) {
    throw new Error('Round-trip failed');
  }
});

// Sequential operations
bench('sequential: 10 ask operations', async () => {
  for (let i = 0; i < 10; i++) {
    const msg = createMessage(
      address('bench-echo'),
      'echo',
      { index: i },
      {
        pattern: 'ask',
        correlationId: generateCorrelationId(),
      }
    );
    await router.ask(msg);
  }
});

// Concurrent operations
bench('concurrent: 10 ask operations', async () => {
  const promises = [];
  for (let i = 0; i < 10; i++) {
    const msg = createMessage(
      address('bench-echo'),
      'echo',
      { index: i },
      {
        pattern: 'ask',
        correlationId: generateCorrelationId(),
      }
    );
    promises.push(router.ask(msg));
  }
  await Promise.all(promises);
});

// Run benchmarks if this is the main module
if (import.meta.main) {
  console.log('📊 Message Routing Benchmarks\n');
  console.log('Target: P95 routing latency < 100µs\n');
  await run();
}
