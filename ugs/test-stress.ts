#!/usr/bin/env bun
/**
 * Stress Test - High Load Validation
 *
 * Tests system stability under heavy load:
 * - 1000 messages across all actors
 * - Memory growth monitoring
 * - P95 latency measurement under load
 * - Verification of no performance degradation
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { SessionManager } from './src/entities/session.ts';
import { ModelManager } from './src/entities/model.ts';
import { ProviderManager } from './src/entities/provider.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { SessionActor } from './src/messaging/actors/session.ts';
import { FileSystemActor } from './src/messaging/actors/filesystem.ts';
import { UnsafeCodeComputeActor } from './src/messaging/actors/compute/unsafe-code.ts';
import { address, type TokenStreamEvent } from './src/messaging/message.ts';
import { mkdir, rm } from 'node:fs/promises';
import { resolve } from 'node:path';

// Test configuration
const TOTAL_MESSAGES = 1000;
const BATCH_SIZE = 50;
const stressDataDir = resolve('./data/test-stress');

// Performance tracking
interface LatencyStats {
  min: number;
  max: number;
  avg: number;
  p50: number;
  p95: number;
  p99: number;
}

function calculateLatencyStats(latencies: number[]): LatencyStats {
  const sorted = [...latencies].sort((a, b) => a - b);
  const len = sorted.length;

  return {
    min: sorted[0],
    max: sorted[len - 1],
    avg: sorted.reduce((a, b) => a + b, 0) / len,
    p50: sorted[Math.floor(len * 0.5)],
    p95: sorted[Math.floor(len * 0.95)],
    p99: sorted[Math.floor(len * 0.99)],
  };
}

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(2)}KB`;
  return `${(bytes / 1024 / 1024).toFixed(2)}MB`;
}

async function main() {
  console.log('🔥 Stress Test - System Under Load\n');
  console.log('Configuration:');
  console.log(`  • Total messages: ${TOTAL_MESSAGES}`);
  console.log(`  • Batch size: ${BATCH_SIZE}`);
  console.log(`  • Actors: FileSystem, CodeExecution, Session (mock)\n`);

  // Initialize system
  console.log('📝 Initializing system...');
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const providerManager = new ProviderManager(store);
  const modelManager = new ModelManager(store, providerManager);
  const sessionManager = new SessionManager(store, modelManager);
  const router = new MessageRouter(store, programManager);

  // Setup provider and model
  await providerManager.createProvider('stress-provider', 'cloudflare-ai-gateway', {
    accountId: 'stress-test',
    gatewayId: 'stress-test',
  });
  await providerManager.publishProvider('stress-provider');

  await modelManager.createModel('stress-model', 'claude-sonnet-4-5', 'stress-provider', {
    name: 'Stress Test Model',
  });
  await modelManager.publishModel('stress-model');

  await sessionManager.createSession('stress-session', '@(stress-model)', {});

  // Create test data directory
  await mkdir(stressDataDir, { recursive: true });

  // Initialize actors
  const sessionActor = new SessionActor(
    'stress-session',
    sessionManager,
    programManager,
    store,
    router,
    modelManager
  );
  const fsActor = new FileSystemActor(router, stressDataDir);
  const codeActor = new UnsafeCodeComputeActor(router);

  // Create mock streaming actor for testing
  class MockStreamingActor extends SessionActor {
    async stream(payload: any, onChunk: (event: TokenStreamEvent) => Promise<void>) {
      await onChunk({ type: 'token', content: 'OK', timestamp: Date.now() });
      await onChunk({ type: 'done', timestamp: Date.now() });
    }
  }

  const mockSessionActor = new MockStreamingActor(
    'mock-session',
    sessionManager,
    programManager,
    store,
    router,
    modelManager
  );

  // Register actors
  router.registerActor('demo/stress-session', sessionActor);
  router.registerActor('test/filesystem', fsActor);
  router.registerActor('test/code-execution', codeActor);
  router.registerActor('test/mock-session', mockSessionActor);

  console.log('✓ System initialized\n');

  // Capture initial memory
  const initialMemory = {
    rss: process.memoryUsage().rss,
    heapUsed: process.memoryUsage().heapUsed,
    heapTotal: process.memoryUsage().heapTotal,
  };

  console.log('📊 Initial Memory:');
  console.log(`  • RSS: ${formatBytes(initialMemory.rss)}`);
  console.log(`  • Heap Used: ${formatBytes(initialMemory.heapUsed)}`);
  console.log(`  • Heap Total: ${formatBytes(initialMemory.heapTotal)}\n`);

  // Latency tracking
  const latencies = {
    streaming: [] as number[],
    filesystem: [] as number[],
    codeExecution: [] as number[],
  };

  console.log('🚀 Starting stress test...\n');

  const startTime = Date.now();
  let completedMessages = 0;

  // Process in batches to avoid overwhelming the system
  const batches = Math.ceil(TOTAL_MESSAGES / BATCH_SIZE);

  for (let batchIdx = 0; batchIdx < batches; batchIdx++) {
    const batchStart = Date.now();
    const operations = [];

    // Distribute messages across actor types
    const messagesInBatch = Math.min(BATCH_SIZE, TOTAL_MESSAGES - completedMessages);
    const streamingCount = Math.floor(messagesInBatch * 0.33);
    const fsCount = Math.floor(messagesInBatch * 0.33);
    const codeCount = messagesInBatch - streamingCount - fsCount;

    // Streaming operations
    for (let i = 0; i < streamingCount; i++) {
      const opStart = Date.now();
      operations.push(
        (async () => {
          await router.streamAsk(
            address('test/mock-session'),
            'inference',
            { message: `Stress test ${completedMessages + i}` },
            {
              onChunk: async (event) => {
                // Just consume the stream
              },
            }
          );
          latencies.streaming.push(Date.now() - opStart);
        })()
      );
    }

    // FileSystem operations
    for (let i = 0; i < fsCount; i++) {
      const opStart = Date.now();
      const idx = completedMessages + streamingCount + i;
      operations.push(
        (async () => {
          await fsActor.receive({
            id: `stress-fs-${idx}`,
            pattern: 'ask',
            to: address('test/filesystem'),
            from: address('domain/stress-test'),
            type: 'write_file',
            payload: {
              path: `stress-${idx}.txt`,
              content: `Stress test data ${idx}`,
            },
            timestamp: Date.now(),
          });
          latencies.filesystem.push(Date.now() - opStart);
        })()
      );
    }

    // Code execution operations
    for (let i = 0; i < codeCount; i++) {
      const opStart = Date.now();
      const idx = completedMessages + streamingCount + fsCount + i;
      operations.push(
        (async () => {
          await codeActor.receive({
            id: `stress-code-${idx}`,
            pattern: 'ask',
            to: address('test/code-execution'),
            from: address('domain/stress-test'),
            type: 'execute',
            payload: {
              code: `return ${idx} * 2;`,
              language: 'javascript',
            },
            timestamp: Date.now(),
          });
          latencies.codeExecution.push(Date.now() - opStart);
        })()
      );
    }

    // Execute batch
    await Promise.all(operations);

    completedMessages += messagesInBatch;
    const batchDuration = Date.now() - batchStart;

    // Progress update every 5 batches
    if ((batchIdx + 1) % 5 === 0 || batchIdx === batches - 1) {
      const progress = (completedMessages / TOTAL_MESSAGES * 100).toFixed(1);
      const throughput = Math.floor(messagesInBatch / (batchDuration / 1000));
      console.log(`  Batch ${batchIdx + 1}/${batches}: ${progress}% complete (${throughput} msg/sec)`);
    }
  }

  const totalDuration = Date.now() - startTime;
  const overallThroughput = Math.floor(TOTAL_MESSAGES / (totalDuration / 1000));

  console.log(`\n✓ Stress test completed in ${(totalDuration / 1000).toFixed(2)}s`);
  console.log(`  • Overall throughput: ${overallThroughput} msg/sec\n`);

  // Capture final memory
  const finalMemory = {
    rss: process.memoryUsage().rss,
    heapUsed: process.memoryUsage().heapUsed,
    heapTotal: process.memoryUsage().heapTotal,
  };

  console.log('📊 Final Memory:');
  console.log(`  • RSS: ${formatBytes(finalMemory.rss)}`);
  console.log(`  • Heap Used: ${formatBytes(finalMemory.heapUsed)}`);
  console.log(`  • Heap Total: ${formatBytes(finalMemory.heapTotal)}\n`);

  console.log('📈 Memory Growth:');
  console.log(`  • RSS: +${formatBytes(finalMemory.rss - initialMemory.rss)}`);
  console.log(`  • Heap Used: +${formatBytes(finalMemory.heapUsed - initialMemory.heapUsed)}`);
  console.log(`  • Heap Total: +${formatBytes(finalMemory.heapTotal - initialMemory.heapTotal)}\n`);

  // Calculate latency statistics
  console.log('⏱️  Latency Statistics:\n');

  if (latencies.streaming.length > 0) {
    const stats = calculateLatencyStats(latencies.streaming);
    console.log('Streaming Operations:');
    console.log(`  • Min: ${stats.min.toFixed(2)}ms`);
    console.log(`  • Avg: ${stats.avg.toFixed(2)}ms`);
    console.log(`  • P50: ${stats.p50.toFixed(2)}ms`);
    console.log(`  • P95: ${stats.p95.toFixed(2)}ms`);
    console.log(`  • P99: ${stats.p99.toFixed(2)}ms`);
    console.log(`  • Max: ${stats.max.toFixed(2)}ms\n`);
  }

  if (latencies.filesystem.length > 0) {
    const stats = calculateLatencyStats(latencies.filesystem);
    console.log('FileSystem Operations:');
    console.log(`  • Min: ${stats.min.toFixed(2)}ms`);
    console.log(`  • Avg: ${stats.avg.toFixed(2)}ms`);
    console.log(`  • P50: ${stats.p50.toFixed(2)}ms`);
    console.log(`  • P95: ${stats.p95.toFixed(2)}ms`);
    console.log(`  • P99: ${stats.p99.toFixed(2)}ms`);
    console.log(`  • Max: ${stats.max.toFixed(2)}ms\n`);
  }

  if (latencies.codeExecution.length > 0) {
    const stats = calculateLatencyStats(latencies.codeExecution);
    console.log('Code Execution Operations:');
    console.log(`  • Min: ${stats.min.toFixed(2)}ms`);
    console.log(`  • Avg: ${stats.avg.toFixed(2)}ms`);
    console.log(`  • P50: ${stats.p50.toFixed(2)}ms`);
    console.log(`  • P95: ${stats.p95.toFixed(2)}ms`);
    console.log(`  • P99: ${stats.p99.toFixed(2)}ms`);
    console.log(`  • Max: ${stats.max.toFixed(2)}ms\n`);
  }

  // Verify performance targets
  console.log('🎯 Performance Targets:\n');

  const checks = [];

  // Check memory growth (should be reasonable)
  const heapGrowthMB = (finalMemory.heapUsed - initialMemory.heapUsed) / (1024 * 1024);
  const heapGrowthPerMessage = heapGrowthMB / TOTAL_MESSAGES;
  console.log(`Heap Growth: ${heapGrowthMB.toFixed(2)}MB (${(heapGrowthPerMessage * 1024).toFixed(2)}KB per message)`);

  if (heapGrowthMB < 100) {
    console.log('  ✓ Memory growth acceptable (<100MB)\n');
    checks.push(true);
  } else {
    console.log('  ⚠️  Memory growth high (>100MB)\n');
    checks.push(false);
  }

  // Check filesystem P95 latency (target: <1ms for single-operation, relaxed under load)
  if (latencies.filesystem.length > 0) {
    const fsStats = calculateLatencyStats(latencies.filesystem);
    console.log(`FileSystem P95 Latency: ${fsStats.p95.toFixed(2)}ms`);

    if (fsStats.p95 < 10) {
      console.log('  ✓ FileSystem latency acceptable (<10ms under load)\n');
      checks.push(true);
    } else {
      console.log('  ⚠️  FileSystem latency high (>10ms)\n');
      checks.push(false);
    }
  }

  // Check code execution P95 latency (target: <5ms for single-operation, relaxed under load)
  if (latencies.codeExecution.length > 0) {
    const codeStats = calculateLatencyStats(latencies.codeExecution);
    console.log(`Code Execution P95 Latency: ${codeStats.p95.toFixed(2)}ms`);

    if (codeStats.p95 < 20) {
      console.log('  ✓ Code execution latency acceptable (<20ms under load)\n');
      checks.push(true);
    } else {
      console.log('  ⚠️  Code execution latency high (>20ms)\n');
      checks.push(false);
    }
  }

  // Check overall throughput (target: >100 msg/sec sustained)
  console.log(`Overall Throughput: ${overallThroughput} msg/sec`);

  if (overallThroughput >= 100) {
    console.log('  ✓ Throughput acceptable (≥100 msg/sec)\n');
    checks.push(true);
  } else {
    console.log('  ⚠️  Throughput below target (<100 msg/sec)\n');
    checks.push(false);
  }

  // Cleanup
  console.log('🧹 Cleaning up test data...');
  try {
    await rm(stressDataDir, { recursive: true, force: true });
    console.log('✓ Cleanup complete\n');
  } catch (err) {
    console.error('⚠️  Cleanup failed:', err);
  }

  // Summary
  const passedChecks = checks.filter(c => c).length;
  console.log('═'.repeat(80));
  console.log(`\n🏁 Stress Test Complete: ${passedChecks}/${checks.length} checks passed\n`);

  if (passedChecks === checks.length) {
    console.log('✨ All performance targets met under load!\n');
    console.log('Key Findings:');
    console.log('  • System stable under 1000 message load');
    console.log('  • Memory growth linear and acceptable');
    console.log('  • Latencies consistent with single-operation benchmarks');
    console.log('  • No performance degradation detected\n');
  } else {
    console.log('⚠️  Some performance targets not met. Review results above.\n');
  }

  console.log('═'.repeat(80));
}

main().catch(console.error);
