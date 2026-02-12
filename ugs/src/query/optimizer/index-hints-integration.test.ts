#!/usr/bin/env bun
/**
 * Index Hints Integration Tests
 * Tests the full flow: QueryBuilder → Compiler → Cache with index hints
 */

import { test, expect, describe } from 'bun:test';
import { query } from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import { QueryCache } from '../cache.ts';
import type { ExecutionContext, ExecutionStats } from '../types.ts';
import { address } from '@agentic-primer/actors';

// Helper to create execution context
function createContext(): ExecutionContext {
  return {
    warmActors: new Set([address('tasks'), address('users')]),
    computationCache: new Map(),
    resources: {
      maxConcurrency: 10,
      availableMemory: 1024 * 1024 * 1024,
    },
    startTime: Date.now(),
  };
}

describe('Index Hints Integration - Manual Hints', () => {
  test('manual hints flow through to compiled plan', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .useIndex('task', 'status', 'Optimize status queries')
      .build();

    const plan = await compiler.compile(q);

    expect(plan.metadata.indexHints).toBeDefined();
    expect(plan.metadata.indexHints).toHaveLength(1);
    expect(plan.metadata.indexHints?.[0]).toEqual({
      variable: 'task',
      index: 'status',
      source: 'manual',
      confidence: 1.0,
      reason: 'Optimize status queries',
    });
  });

  test('manual hints are applied to query steps', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .useIndex('task', 'status')
      .build();

    const plan = await compiler.compile(q);

    const queryStep = plan.steps.find((s) => s.type === 'query');
    expect(queryStep).toBeDefined();
    expect(queryStep?.metadata?.indexHints).toBeDefined();
    expect(queryStep?.message.payload.useIndexes).toContain('status');
  });

  test('multiple manual hints are preserved', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'open', priority: 'high' })
      )
      .useIndex('task', 'status')
      .useIndex('task', 'priority')
      .build();

    const plan = await compiler.compile(q);

    expect(plan.metadata.indexHints?.length).toBe(2);
    expect(plan.metadata.indexHints?.map((h) => h.index)).toContain('status');
    expect(plan.metadata.indexHints?.map((h) => h.index)).toContain('priority');
  });
});

describe('Index Hints Integration - Automatic Selection', () => {
  test('automatic hints are generated when no manual hints', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .build();

    const plan = await compiler.compile(q);

    expect(plan.metadata.indexHints).toBeDefined();
    expect(plan.metadata.indexHints && plan.metadata.indexHints.length > 0).toBe(
      true
    );

    const idHint = plan.metadata.indexHints?.find((h) => h.index === 'id');
    expect(idHint).toBeDefined();
    expect(idHint?.source).toBe('automatic');
    expect(idHint?.confidence).toBeGreaterThan(0.7);
  });

  test('automatic hints cover multiple properties', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(
        pattern('user').label('User').where({
          email: 'alice@example.com',
          active: true,
        })
      )
      .build();

    const plan = await compiler.compile(q);

    const emailHint = plan.metadata.indexHints?.find((h) => h.index === 'email');
    expect(emailHint).toBeDefined();
    expect(emailHint?.source).toBe('automatic');
  });

  test('automatic hints improve cost estimates', async () => {
    const compiler = new QueryCompiler();
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .build();

    const plan = await compiler.compile(q, context);

    // Query steps should have reduced cost due to index hints
    const queryStep = plan.steps.find((s) => s.type === 'query');
    expect(queryStep).toBeDefined();

    // Cost should be reduced from baseline due to index
    expect(queryStep?.cost.latencyMs).toBeLessThan(10); // Less than base cost
  });
});

describe('Index Hints Integration - Manual + Automatic', () => {
  test('manual hints take precedence over automatic for same variable', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open', id: '123' }))
      .useIndex('task', 'status', 'Manual override')
      .build();

    const plan = await compiler.compile(q);

    // Should have manual hint for status
    const manualHint = plan.metadata.indexHints?.find(
      (h) => h.index === 'status' && h.source === 'manual'
    );
    expect(manualHint).toBeDefined();

    // Should NOT have automatic hint for task variable (since manual provided)
    const autoTaskHints = plan.metadata.indexHints?.filter(
      (h) => h.variable === 'task' && h.source === 'automatic'
    );
    expect(autoTaskHints?.length).toBe(0);
  });

  test('automatic hints fill in for uncovered variables', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'open' }),
        pattern('user').label('User').where({ email: 'alice@example.com' })
      )
      .useIndex('task', 'status')
      .build();

    const plan = await compiler.compile(q);

    // Manual hint for task
    const taskHint = plan.metadata.indexHints?.find(
      (h) => h.variable === 'task' && h.source === 'manual'
    );
    expect(taskHint).toBeDefined();

    // Automatic hints for user
    const userHints = plan.metadata.indexHints?.filter(
      (h) => h.variable === 'user' && h.source === 'automatic'
    );
    expect(userHints && userHints.length > 0).toBe(true);
  });
});

describe('Index Hints Integration - Cache Effectiveness Tracking', () => {
  test('cache tracks index effectiveness after execution', async () => {
    const compiler = new QueryCompiler();
    const cache = new QueryCache();

    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .useIndex('task', 'status')
      .build();

    const plan = await compiler.compile(q);

    // Simulate execution
    const stats: ExecutionStats = {
      durationMs: 25,
      stepsExecuted: 1,
      messagesSent: 1,
      cacheHits: 0,
      cacheMisses: 1,
      resultsReturned: 10,
      stepStats: new Map(),
    };

    cache.recordExecution(plan, stats);

    // Get statistics
    const queryStats = cache.getStatistics(plan);
    expect(queryStats).toBeDefined();
    expect(queryStats?.indexEffectiveness).toBeDefined();

    const statusEff = queryStats?.indexEffectiveness?.get('status');
    expect(statusEff).toBeDefined();
    expect(statusEff?.indexName).toBe('status');
    expect(statusEff?.useCount).toBe(1);
  });

  test('index effectiveness improves over multiple executions', async () => {
    const compiler = new QueryCompiler();
    const cache = new QueryCache();

    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .useIndex('task', 'id')
      .build();

    const plan = await compiler.compile(q);

    // Execute multiple times with varying performance
    const executions = [
      { durationMs: 30, resultsReturned: 1 },
      { durationMs: 25, resultsReturned: 1 },
      { durationMs: 20, resultsReturned: 1 },
    ];

    for (const exec of executions) {
      const stats: ExecutionStats = {
        durationMs: exec.durationMs,
        stepsExecuted: 1,
        messagesSent: 1,
        cacheHits: 0,
        cacheMisses: 1,
        resultsReturned: exec.resultsReturned,
        stepStats: new Map(),
      };

      cache.recordExecution(plan, stats);
    }

    // Check aggregated statistics
    const queryStats = cache.getStatistics(plan);
    const idEff = queryStats?.indexEffectiveness?.get('id');

    expect(idEff?.useCount).toBe(3);
    expect(idEff?.avgResultCount).toBe(1);
    expect(idEff?.successRate).toBe(1.0);
    expect(idEff?.avgImprovement).toBeGreaterThanOrEqual(0);
  });

  test('index effectiveness distinguishes between indexes', async () => {
    const compiler = new QueryCompiler();
    const cache = new QueryCache();

    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'open', priority: 'high' })
      )
      .useIndex('task', 'status')
      .useIndex('task', 'priority')
      .build();

    const plan = await compiler.compile(q);

    const stats: ExecutionStats = {
      durationMs: 20,
      stepsExecuted: 1,
      messagesSent: 1,
      cacheHits: 0,
      cacheMisses: 1,
      resultsReturned: 5,
      stepStats: new Map(),
    };

    cache.recordExecution(plan, stats);

    const queryStats = cache.getStatistics(plan);

    // Both indexes should be tracked
    expect(queryStats?.indexEffectiveness?.has('status')).toBe(true);
    expect(queryStats?.indexEffectiveness?.has('priority')).toBe(true);
  });
});

describe('Index Hints Integration - Legacy Compatibility', () => {
  test('plan metadata includes legacy string indexes', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .useIndex('task', 'status')
      .build();

    const plan = await compiler.compile(q);

    // Legacy format: array of strings
    expect(plan.metadata.indexes).toBeDefined();
    expect(Array.isArray(plan.metadata.indexes)).toBe(true);
    expect(plan.metadata.indexes[0]).toBe('task:status');
  });

  test('legacy indexes include all hints', async () => {
    const compiler = new QueryCompiler();

    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'open', priority: 'high' })
      )
      .useIndex('task', 'status')
      .useIndex('task', 'priority')
      .build();

    const plan = await compiler.compile(q);

    expect(plan.metadata.indexes.length).toBe(2);
    expect(plan.metadata.indexes).toContain('task:status');
    expect(plan.metadata.indexes).toContain('task:priority');
  });
});
