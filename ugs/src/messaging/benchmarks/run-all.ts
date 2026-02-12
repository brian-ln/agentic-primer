#!/usr/bin/env bun
/**
 * Run All Benchmarks
 *
 * Executes all benchmark suites and formats results for documentation.
 */

import { $ } from 'bun';

const benchmarks = [
  {
    name: 'Message Creation',
    file: 'message-creation.bench.ts',
    description: 'Measures overhead of message creation utilities',
  },
  {
    name: 'Message Routing',
    file: 'routing.bench.ts',
    description: 'Measures Router.tell() and Router.ask() latency',
  },
  {
    name: 'Messaging Patterns',
    file: 'patterns.bench.ts',
    description: 'Measures throughput, concurrency, and memory stability',
  },
  {
    name: 'Tool Actors',
    file: 'tool-actors.bench.ts',
    description: 'Measures FileSystemActor and UnsafeCodeExecutionActor performance',
  },
];

console.log('═'.repeat(80));
console.log('📊 SIMPLIFY MESSAGE LAYER - PERFORMANCE BENCHMARKS');
console.log('═'.repeat(80));
console.log();
console.log('Running comprehensive benchmarks on the messaging layer...');
console.log();

const results = [];

for (const benchmark of benchmarks) {
  console.log('─'.repeat(80));
  console.log(`\n🔬 ${benchmark.name}`);
  console.log(`   ${benchmark.description}\n`);
  console.log('─'.repeat(80));
  console.log();

  const startTime = Date.now();

  try {
    // Run benchmark
    const result = await $`bun run ${import.meta.dir}/${benchmark.file}`.text();

    const duration = Date.now() - startTime;

    results.push({
      name: benchmark.name,
      success: true,
      duration,
      output: result,
    });

    console.log(result);
  } catch (error) {
    const duration = Date.now() - startTime;

    results.push({
      name: benchmark.name,
      success: false,
      duration,
      error: error instanceof Error ? error.message : String(error),
    });

    console.error('❌ Benchmark failed:', error);
  }

  console.log();
}

// Summary
console.log('═'.repeat(80));
console.log('📈 BENCHMARK SUMMARY');
console.log('═'.repeat(80));
console.log();

let totalDuration = 0;
let successCount = 0;

for (const result of results) {
  totalDuration += result.duration;

  if (result.success) {
    successCount++;
    console.log(`✓ ${result.name.padEnd(30)} ${result.duration}ms`);
  } else {
    console.log(`✗ ${result.name.padEnd(30)} FAILED`);
  }
}

console.log();
console.log(`Total: ${successCount}/${results.length} benchmarks passed`);
console.log(`Duration: ${totalDuration}ms`);
console.log();

if (successCount === results.length) {
  console.log('✨ All benchmarks completed successfully!\n');
  console.log('Next steps:');
  console.log('  1. Review results above');
  console.log('  2. Update docs/performance/benchmarks.md with [MEASURED] data');
  console.log('  3. Update README.md with performance summary');
  console.log();
} else {
  console.log('⚠️  Some benchmarks failed. Review errors above.\n');
  process.exit(1);
}

console.log('═'.repeat(80));
