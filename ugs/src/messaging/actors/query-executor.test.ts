#!/usr/bin/env bun
/**
 * QueryExecutor Actor Tests
 * Tests for src/messaging/actors/query-executor.ts
 * Target: >90% coverage
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { QueryExecutor } from './query-executor.ts';
import { MessageRouter } from '../router.ts';
import { SilentLoggerActor } from '../../messaging/actors/logger.ts';
import { address, generateMessageId, type Message } from '@agentic-primer/actors';
import { query, send } from '../../query/builder.ts';
import { pattern } from '../../query/pattern.ts';
import type GraphStore from '../../graph.ts';
import type { ProgramManager } from '../../entities/program.ts';

// Mock implementations
function createMockGraphStore(): GraphStore {
  const nodes = new Map();
  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
  } as any as GraphStore;
}

function createMockProgramManager(): ProgramManager {
  return {} as any as ProgramManager;
}

// Helper to create test message
function testMessage(type: string, payload: any): Message {
  return {
    id: generateMessageId(),
    pattern: 'ask',
    type,
    payload,
    from: address('test'),
    to: address('query-executor'),
    timestamp: Date.now(),
  };
}

describe('QueryExecutor - Construction', () => {
  test('creates query executor with id and router', () => {
    const router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    const executor = new QueryExecutor('query-executor', router);
    expect(executor).toBeDefined();
    expect(executor.address).toBe('@(query-executor)');
  });
});

describe('QueryExecutor - Message Handling', () => {
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  test('handles unknown message type', async () => {
    const msg = testMessage('unknown-type', {});
    const response = await executor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Unknown message type');
  });

  test('handles execute message', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const msg = testMessage('execute', { query: q });
    const response = await executor.receive(msg);
    expect(response).toBeDefined();
  });

  test('returns error if execute missing query', async () => {
    const msg = testMessage('execute', {});
    const response = await executor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Missing query definition');
  });

  test('returns error if execute-plan missing plan', async () => {
    const msg = testMessage('execute-plan', {});
    const response = await executor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Missing query plan');
  });
});

describe('QueryExecutor - Cache Operations', () => {
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  test('handles get-cache-stats message', async () => {
    const msg = testMessage('get-cache-stats', {});
    const response = await executor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.cache).toBeDefined();
    expect(response.payload.cache.size).toBeDefined();
  });

  test('handles clear-cache message', async () => {
    const msg = testMessage('clear-cache', {});
    const response = await executor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.cleared).toBe(true);
  });
});

describe('QueryExecutor - Plan Execution', () => {
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  test('executes empty plan successfully', async () => {
    const q = query().build();
    const plan = {
      id: 'empty-plan',
      steps: [],
      variables: [],
      metadata: {
        estimatedCost: {
          makespan: 0,
          totalWork: 0,
          resourceUsage: { memoryBytes: 0, ioOps: 0, messageCount: 0 },
        },
        indexes: [],
        parallelizable: false,
        criticalPathSteps: 0,
        compiledAt: Date.now(),
      },
      original: q,
    };

    const msg = testMessage('execute-plan', { plan });
    const response = await executor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.result.success).toBe(true);
    expect(response.payload.result.stats.stepsExecuted).toBe(0);
  });

  test('execution result includes statistics', async () => {
    const q = query().build();
    const plan = {
      id: 'test-plan',
      steps: [],
      variables: [],
      metadata: {
        estimatedCost: {
          makespan: 0,
          totalWork: 0,
          resourceUsage: { memoryBytes: 0, ioOps: 0, messageCount: 0 },
        },
        indexes: [],
        parallelizable: false,
        criticalPathSteps: 0,
        compiledAt: Date.now(),
      },
      original: q,
    };

    const msg = testMessage('execute-plan', { plan });
    const response = await executor.receive(msg);

    expect(response.payload.result.stats).toBeDefined();
    expect(response.payload.result.stats.durationMs).toBeGreaterThanOrEqual(0);
  });
});

describe('QueryExecutor - Compilation and Caching', () => {
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  test('compiles query on first execution', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const msg = testMessage('execute', { query: q });
    const response = await executor.receive(msg);

    expect(response.payload?.plan?.cacheHit).toBe(false);
  });

  test('uses cached plan on second execution', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const msg = testMessage('execute', { query: q });

    await executor.receive(msg);
    const response2 = await executor.receive(msg);
    expect(response2.payload?.plan?.cacheHit).toBe(true);
  });
});

describe('QueryExecutor - Error Handling', () => {
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  test('handles malformed query gracefully', async () => {
    const msg = testMessage('execute', { query: null });
    const response = await executor.receive(msg);
    expect(response.success).toBe(false);
  });

  test('returns error response on exception', async () => {
    const msg = testMessage('execute', { query: { invalid: 'query' } as any });
    const response = await executor.receive(msg);

    expect(response.success).toBe(false);
    expect(response.error).toBeDefined();
  });
});

describe('Performance and Metrics', () => {
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  test('tracks execution duration', async () => {
    const q = query().build();
    const plan = {
      id: 'test',
      steps: [],
      variables: [],
      metadata: {
        estimatedCost: {
          makespan: 0,
          totalWork: 0,
          resourceUsage: { memoryBytes: 0, ioOps: 0, messageCount: 0 },
        },
        indexes: [],
        parallelizable: false,
        criticalPathSteps: 0,
        compiledAt: Date.now(),
      },
      original: q,
    };

    const msg = testMessage('execute-plan', { plan });
    const response = await executor.receive(msg);

    expect(response.payload.result.stats.durationMs).toBeGreaterThanOrEqual(0);
  });
});

describe('Edge Cases', () => {
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  test('handles empty message payload', async () => {
    const msg = testMessage('execute', {});
    const response = await executor.receive(msg);
    expect(response.success).toBe(false);
  });

  test('handles plan with no steps', async () => {
    const q = query().build();
    const plan = {
      id: 'empty',
      steps: [],
      variables: [],
      metadata: {
        estimatedCost: {
          makespan: 0,
          totalWork: 0,
          resourceUsage: { memoryBytes: 0, ioOps: 0, messageCount: 0 },
        },
        indexes: [],
        parallelizable: false,
        criticalPathSteps: 0,
        compiledAt: Date.now(),
      },
      original: q,
    };

    const msg = testMessage('execute-plan', { plan });
    const response = await executor.receive(msg);

    expect(response.success).toBe(true);
    expect(response.payload.result.success).toBe(true);
  });
});
