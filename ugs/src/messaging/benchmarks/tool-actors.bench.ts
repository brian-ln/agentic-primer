#!/usr/bin/env bun
/**
 * Benchmark: Tool Actors Performance
 *
 * Measures the performance of FileSystemActor and UnsafeCodeComputeActor.
 * Compares tool actor overhead to routing benchmarks.
 *
 * Targets:
 * - FileSystem operations: P95 < 1ms
 * - Code execution: P95 < 5ms
 * - Tool actor overhead: < 10% vs routing baseline
 */

import { bench, run } from 'mitata';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import { MessageRouter } from '../router.ts';
import { FileSystemActor } from '../actors/filesystem.ts';
import { UnsafeCodeComputeActor } from '../actors/compute/unsafe-code.ts';
import { address } from '../message.ts';
import { mkdir, rm } from 'node:fs/promises';
import { resolve } from 'node:path';

// Setup: Create store, router, and actors
const store = new GraphStore();
const programManager = new ProgramManager(store);
const router = new MessageRouter(store, programManager);

const benchDataDir = resolve('./data/bench-tool-actors');

// Ensure benchmark data directory exists
await mkdir(benchDataDir, { recursive: true });

// Initialize actors
const fsActor = new FileSystemActor(router, benchDataDir);
const codeActor = new UnsafeCodeComputeActor(router);

// Register actors
router.registerActor('filesystem', fsActor);
router.registerActor('code-execution', codeActor);

// Pre-create a test file for read benchmarks
await fsActor.receive({
  id: 'setup-read-test',
  pattern: 'ask',
  to: address('filesystem'),
  from: address('benchmark'),
  type: 'write_file',
  payload: {
    path: 'read-test.txt',
    content: 'This is a test file for read benchmarks.\n'.repeat(10),
  },
  timestamp: Date.now(),
});

console.log('📊 Tool Actors Performance Benchmarks\n');
console.log('Targets:');
console.log('  • FileSystem operations: P95 < 1ms');
console.log('  • Code execution: P95 < 5ms');
console.log('  • Tool actor overhead: < 10% vs routing baseline\n');

// ============================================================================
// FileSystemActor Benchmarks
// ============================================================================

bench('FileSystemActor: write small file (100 bytes)', async () => {
  const msg = {
    id: `write-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path: `bench-write-small-${Date.now()}.txt`,
      content: 'X'.repeat(100),
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(msg);
});

bench('FileSystemActor: write medium file (10KB)', async () => {
  const msg = {
    id: `write-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path: `bench-write-medium-${Date.now()}.txt`,
      content: 'X'.repeat(10_000),
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(msg);
});

bench('FileSystemActor: write large file (100KB)', async () => {
  const msg = {
    id: `write-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path: `bench-write-large-${Date.now()}.txt`,
      content: 'X'.repeat(100_000),
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(msg);
});

bench('FileSystemActor: read file', async () => {
  const msg = {
    id: `read-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'read_file',
    payload: {
      path: 'read-test.txt',
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(msg);
});

bench('FileSystemActor: list directory', async () => {
  const msg = {
    id: `list-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'list_dir',
    payload: {
      path: '.',
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(msg);
});

bench('FileSystemActor: delete file', async () => {
  // Create file first
  const createMsg = {
    id: `create-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path: `bench-delete-${Date.now()}.txt`,
      content: 'To be deleted',
    },
    timestamp: Date.now(),
  };
  const createResult = await fsActor.receive(createMsg);

  // Delete it
  const deleteMsg = {
    id: `delete-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'delete_file',
    payload: {
      path: createResult.payload?.path,
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(deleteMsg);
});

bench('FileSystemActor: write + read round-trip', async () => {
  const path = `bench-roundtrip-${Date.now()}.txt`;

  // Write
  const writeMsg = {
    id: `write-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path,
      content: 'Round-trip test',
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(writeMsg);

  // Read
  const readMsg = {
    id: `read-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'read_file',
    payload: {
      path,
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(readMsg);
});

// ============================================================================
// UnsafeCodeExecutionActor Benchmarks
// ============================================================================

bench('UnsafeCodeExecutionActor: simple arithmetic', async () => {
  const msg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: 'return 2 + 2;',
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  await codeActor.receive(msg);
});

bench('UnsafeCodeExecutionActor: array operation', async () => {
  const msg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: 'return [1, 2, 3, 4, 5].reduce((a, b) => a + b, 0);',
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  await codeActor.receive(msg);
});

bench('UnsafeCodeExecutionActor: string manipulation', async () => {
  const msg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: 'return "hello world".toUpperCase().split(" ").join("-");',
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  await codeActor.receive(msg);
});

bench('UnsafeCodeExecutionActor: JSON processing', async () => {
  const msg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: `
        const data = { a: 1, b: 2, c: 3 };
        const sum = Object.values(data).reduce((a, b) => a + b, 0);
        return { ...data, sum };
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  await codeActor.receive(msg);
});

bench('UnsafeCodeExecutionActor: loop computation', async () => {
  const msg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: `
        let sum = 0;
        for (let i = 0; i < 100; i++) {
          sum += i;
        }
        return sum;
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  await codeActor.receive(msg);
});

bench('UnsafeCodeExecutionActor: with console output', async () => {
  const msg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: `
        console.log("Starting computation");
        const result = 42 * 2;
        console.log("Result:", result);
        return result;
      `,
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  await codeActor.receive(msg);
});

// ============================================================================
// Concurrent Operations
// ============================================================================

bench('FileSystemActor: 10 concurrent writes', async () => {
  const operations = [];
  for (let i = 0; i < 10; i++) {
    const msg = {
      id: `concurrent-write-${i}-${Date.now()}`,
      pattern: 'ask' as const,
      to: address('filesystem'),
      from: address('benchmark'),
      type: 'write_file',
      payload: {
        path: `bench-concurrent-${i}-${Date.now()}.txt`,
        content: `File ${i}`,
      },
      timestamp: Date.now(),
    };
    operations.push(fsActor.receive(msg));
  }
  await Promise.all(operations);
});

bench('UnsafeCodeExecutionActor: 10 concurrent executions', async () => {
  const operations = [];
  for (let i = 0; i < 10; i++) {
    const msg = {
      id: `concurrent-exec-${i}-${Date.now()}`,
      pattern: 'ask' as const,
      to: address('code-execution'),
      from: address('benchmark'),
      type: 'execute',
      payload: {
        code: `return ${i} * 2;`,
        language: 'javascript',
      },
      timestamp: Date.now(),
    };
    operations.push(codeActor.receive(msg));
  }
  await Promise.all(operations);
});

// ============================================================================
// Combined Workflow Benchmark
// ============================================================================

bench('Combined: code execution → file write', async () => {
  // Execute code
  const execMsg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: 'return { result: 42, timestamp: Date.now() };',
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  const execResult = await codeActor.receive(execMsg);

  // Write result to file
  const writeMsg = {
    id: `write-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path: `bench-combined-${Date.now()}.json`,
      content: JSON.stringify(execResult.payload),
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(writeMsg);
});

bench('Combined: file read → code execution → file write', async () => {
  const path = `bench-full-workflow-${Date.now()}.txt`;

  // Write initial data
  const writeMsg1 = {
    id: `write1-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path,
      content: '10',
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(writeMsg1);

  // Read data
  const readMsg = {
    id: `read-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'read_file',
    payload: {
      path,
    },
    timestamp: Date.now(),
  };
  const readResult = await fsActor.receive(readMsg);

  // Process data
  const execMsg = {
    id: `exec-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('code-execution'),
    from: address('benchmark'),
    type: 'execute',
    payload: {
      code: `return ${readResult.payload?.content} * 2;`,
      language: 'javascript',
    },
    timestamp: Date.now(),
  };
  const execResult = await codeActor.receive(execMsg);

  // Write result
  const writeMsg2 = {
    id: `write2-${Date.now()}`,
    pattern: 'ask' as const,
    to: address('filesystem'),
    from: address('benchmark'),
    type: 'write_file',
    payload: {
      path: `${path}.result`,
      content: String(execResult.payload?.result),
    },
    timestamp: Date.now(),
  };
  await fsActor.receive(writeMsg2);
});

// Run benchmarks if this is the main module
if (import.meta.main) {
  await run();

  // Cleanup benchmark files
  console.log('\n🧹 Cleaning up benchmark files...');
  try {
    await rm(benchDataDir, { recursive: true, force: true });
    console.log('✓ Cleanup complete\n');
  } catch (err) {
    console.error('⚠️  Cleanup failed:', err);
  }
}
