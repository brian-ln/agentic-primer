#!/usr/bin/env bun
/**
 * Join Optimizer Tests
 * Tests for src/query/optimizer/join-optimizer.ts
 * Target: >90% coverage
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { JoinOptimizer, type SelectivityStats } from './join-optimizer.ts';
import type { PlanStep, ExecutionContext } from '../types.ts';
import { address } from '@agentic-primer/actors';

// Helper to create mock plan steps
function createMockStep(
  id: string,
  signature: string,
  actor: string = 'tasks'
): PlanStep {
  return {
    id,
    type: 'query',
    actor: address(actor),
    message: {
      pattern: 'ask',
      type: 'query',
      payload: {},
      from: address('query-executor'),
    },
    bindings: [id],
    dependencies: [],
    parallelizable: true,
    signature,
    cost: {
      latencyMs: 10,
      cpuMs: 8,
      resultCount: 100,
      cacheHitProb: 0.1,
    },
  };
}

// Helper to create execution context
function createContext(warmActors: string[] = []): ExecutionContext {
  return {
    warmActors: new Set(warmActors.map(a => address(a))),
    computationCache: new Map(),
    resources: {
      maxConcurrency: 10,
      availableMemory: 1024 * 1024 * 1024,
    },
    startTime: Date.now(),
  };
}

describe('JoinOptimizer - Construction', () => {
  test('creates optimizer with default options', () => {
    const optimizer = new JoinOptimizer();
    expect(optimizer).toBeDefined();
  });

  test('creates optimizer with custom options', () => {
    const optimizer = new JoinOptimizer({
      defaultSelectivity: 0.2,
      defaultCardinality: 50,
      enableDynamicProgramming: false,
    });
    expect(optimizer).toBeDefined();
  });
});

describe('JoinOptimizer - Basic Optimization', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('returns single step unchanged', () => {
    const steps = [createMockStep('step_0', 'sig1')];
    const optimized = optimizer.optimizeJoinOrder(steps);

    expect(optimized).toHaveLength(1);
    expect(optimized[0].id).toBe('step_0');
  });

  test('optimizes two-step join based on selectivity', () => {
    const steps = [
      createMockStep('step_0', 'sig1'),
      createMockStep('step_1', 'sig2'),
    ];

    // Make sig2 more selective
    optimizer.updateStatistics('sig1', 500); // Low selectivity
    optimizer.updateStatistics('sig2', 10);  // High selectivity

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Most selective (sig2) should come first
    expect(optimized[0].signature).toBe('sig2');
    expect(optimized[1].signature).toBe('sig1');
  });

  test('optimizes three-step join', () => {
    const steps = [
      createMockStep('step_0', 'sig1'),
      createMockStep('step_1', 'sig2'),
      createMockStep('step_2', 'sig3'),
    ];

    // Set different selectivities
    optimizer.updateStatistics('sig1', 1000); // Least selective
    optimizer.updateStatistics('sig2', 50);   // Most selective
    optimizer.updateStatistics('sig3', 300);  // Medium

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Order should be: sig2, sig3, sig1
    expect(optimized[0].signature).toBe('sig2');
    expect(optimized[1].signature).toBe('sig3');
    expect(optimized[2].signature).toBe('sig1');
  });

  test('preserves non-query steps', () => {
    const steps = [
      createMockStep('step_0', 'sig1'),
      createMockStep('step_1', 'sig2'),
      {
        ...createMockStep('step_2', 'sig3'),
        type: 'traverse' as const,
      },
    ];

    optimizer.updateStatistics('sig2', 10);
    optimizer.updateStatistics('sig1', 100);

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Query steps should be reordered, traverse preserved
    const querySteps = optimized.filter(s => s.type === 'query');
    expect(querySteps[0].signature).toBe('sig2');
  });
});

describe('JoinOptimizer - Warm Actor Consideration', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('prefers warm actors even with slightly worse selectivity', () => {
    const steps = [
      createMockStep('step_0', 'sig1', 'tasks'),
      createMockStep('step_1', 'sig2', 'users'),
    ];

    // Both have similar selectivity
    optimizer.updateStatistics('sig1', 50);
    optimizer.updateStatistics('sig2', 40);

    const context = createContext(['tasks']); // tasks is warm

    const optimized = optimizer.optimizeJoinOrder(steps, context);

    // Warm actor (tasks) should be preferred
    expect(optimized[0].actor).toBe('@(tasks)');
  });

  test('does not prefer warm actors with much worse selectivity', () => {
    const steps = [
      createMockStep('step_0', 'sig1', 'tasks'),
      createMockStep('step_1', 'sig2', 'users'),
    ];

    // sig2 is much more selective
    optimizer.updateStatistics('sig1', 500);
    optimizer.updateStatistics('sig2', 5);

    const context = createContext(['tasks']);

    const optimized = optimizer.optimizeJoinOrder(steps, context);

    // High selectivity (sig2) should win despite warm actor
    expect(optimized[0].signature).toBe('sig2');
  });
});

describe('JoinOptimizer - Statistics Management', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('updates statistics with single execution', () => {
    optimizer.updateStatistics('sig1', 100, 1000);

    const stats = optimizer.getStatistics();
    expect(stats).toHaveLength(1);
    expect(stats[0].signature).toBe('sig1');
    expect(stats[0].avgResultCount).toBe(100);
    expect(stats[0].executionCount).toBe(1);
  });

  test('updates statistics with multiple executions (moving average)', () => {
    optimizer.updateStatistics('sig1', 100, 1000);
    optimizer.updateStatistics('sig1', 200, 1000);
    optimizer.updateStatistics('sig1', 150, 1000);

    const stats = optimizer.getStatistics();
    expect(stats[0].executionCount).toBe(3);
    // Should be weighted toward recent executions
    expect(stats[0].avgResultCount).toBeGreaterThan(100);
    expect(stats[0].avgResultCount).toBeLessThan(200);
  });

  test('calculates selectivity correctly', () => {
    optimizer.updateStatistics('sig1', 50, 1000);

    const stats = optimizer.getStatistics();
    expect(stats[0].selectivity).toBeCloseTo(0.05, 2);
  });

  test('updates variance on multiple executions', () => {
    optimizer.updateStatistics('sig1', 100, 1000);
    optimizer.updateStatistics('sig1', 100, 1000);

    const stats = optimizer.getStatistics();
    expect(stats[0].resultVariance).toBe(0);

    optimizer.updateStatistics('sig1', 200, 1000);
    expect(stats[0].resultVariance).toBeGreaterThan(0);
  });

  test('clears statistics', () => {
    optimizer.updateStatistics('sig1', 100);
    optimizer.updateStatistics('sig2', 200);

    optimizer.clearStatistics();

    const stats = optimizer.getStatistics();
    expect(stats).toHaveLength(0);
  });
});

describe('JoinOptimizer - Statistics Import/Export', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('exports statistics', () => {
    optimizer.updateStatistics('sig1', 100, 1000);
    optimizer.updateStatistics('sig2', 50, 1000);

    const exported = optimizer.exportStatistics();

    expect(Object.keys(exported)).toHaveLength(2);
    expect(exported['sig1']).toBeDefined();
    expect(exported['sig2']).toBeDefined();
  });

  test('imports statistics', () => {
    const stats: Record<string, SelectivityStats> = {
      'sig1': {
        signature: 'sig1',
        avgResultCount: 100,
        resultVariance: 10,
        executionCount: 5,
        selectivity: 0.1,
        lastUpdated: Date.now(),
      },
    };

    optimizer.importStatistics(stats);

    const imported = optimizer.getStatistics();
    expect(imported).toHaveLength(1);
    expect(imported[0].signature).toBe('sig1');
    expect(imported[0].avgResultCount).toBe(100);
  });

  test('import/export round-trip preserves data', () => {
    optimizer.updateStatistics('sig1', 100, 1000);
    optimizer.updateStatistics('sig2', 50, 1000);

    const exported = optimizer.exportStatistics();

    const newOptimizer = new JoinOptimizer();
    newOptimizer.importStatistics(exported);

    const stats = newOptimizer.getStatistics();
    expect(stats).toHaveLength(2);
  });
});

describe('JoinOptimizer - Dynamic Programming Mode', () => {
  test('uses DP for small join sets', () => {
    const optimizer = new JoinOptimizer({
      enableDynamicProgramming: true,
    });

    const steps = [
      createMockStep('step_0', 'sig1'),
      createMockStep('step_1', 'sig2'),
      createMockStep('step_2', 'sig3'),
    ];

    optimizer.updateStatistics('sig1', 1000);
    optimizer.updateStatistics('sig2', 10);
    optimizer.updateStatistics('sig3', 100);

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Should find optimal order
    expect(optimized[0].signature).toBe('sig2');
  });

  test('uses greedy for large join sets', () => {
    const optimizer = new JoinOptimizer({
      enableDynamicProgramming: true,
    });

    // Create 10 steps (exceeds DP threshold of 8)
    const steps = Array.from({ length: 10 }, (_, i) =>
      createMockStep(`step_${i}`, `sig${i}`)
    );

    // Update stats
    steps.forEach((s, i) => {
      optimizer.updateStatistics(s.signature, (i + 1) * 100);
    });

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Should still optimize (using greedy)
    expect(optimized).toHaveLength(10);
    expect(optimized[0].signature).toBe('sig0'); // Most selective
  });

  test('can disable DP mode', () => {
    const optimizer = new JoinOptimizer({
      enableDynamicProgramming: false,
    });

    const steps = [
      createMockStep('step_0', 'sig1'),
      createMockStep('step_1', 'sig2'),
    ];

    optimizer.updateStatistics('sig1', 100);
    optimizer.updateStatistics('sig2', 10);

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Should use greedy (still works correctly)
    expect(optimized[0].signature).toBe('sig2');
  });
});

describe('JoinOptimizer - Import from Query Statistics', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('imports query statistics', () => {
    const queryStats = [
      {
        signature: 'sig1',
        executionCount: 10,
        avgDurationMs: 50,
        durationVariance: 5,
        avgResultCount: 100,
        successRate: 0.95,
        cacheHitRate: 0.7,
        lastExecutedAt: Date.now(),
        latencyPercentiles: { p50: 45, p90: 60, p99: 80 },
      },
      {
        signature: 'sig2',
        executionCount: 5,
        avgDurationMs: 30,
        durationVariance: 2,
        avgResultCount: 20,
        successRate: 1.0,
        cacheHitRate: 0.8,
        lastExecutedAt: Date.now(),
        latencyPercentiles: { p50: 28, p90: 35, p99: 45 },
      },
    ];

    optimizer.importFromQueryStatistics(queryStats);

    const stats = optimizer.getStatistics();
    expect(stats).toHaveLength(2);
    expect(stats[0].avgResultCount).toBe(100);
    expect(stats[1].avgResultCount).toBe(20);
  });
});

describe('JoinOptimizer - Explain Join Order', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('generates explanation for join order', () => {
    const steps = [
      createMockStep('step_0', 'sig1'),
      createMockStep('step_1', 'sig2'),
    ];

    optimizer.updateStatistics('sig1', 100, 1000);
    optimizer.updateStatistics('sig2', 10, 1000);

    const explanation = optimizer.explainJoinOrder(steps);

    expect(explanation).toContain('Join Order Analysis');
    expect(explanation).toContain('step_0');
    expect(explanation).toContain('step_1');
    expect(explanation).toContain('Selectivity');
    expect(explanation).toContain('Est. Results');
  });

  test('explanation includes signature info', () => {
    const steps = [createMockStep('step_0', 'test-signature-123')];

    const explanation = optimizer.explainJoinOrder(steps);

    expect(explanation).toContain('test-signature-123');
  });
});

describe('JoinOptimizer - Edge Cases', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('handles empty step array', () => {
    const optimized = optimizer.optimizeJoinOrder([]);
    expect(optimized).toHaveLength(0);
  });

  test('handles steps with no statistics (uses defaults)', () => {
    const steps = [
      createMockStep('step_0', 'unknown1'),
      createMockStep('step_1', 'unknown2'),
    ];

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Should still return valid order (using defaults)
    expect(optimized).toHaveLength(2);
  });

  test('handles steps with zero result count', () => {
    optimizer.updateStatistics('sig1', 0, 1000);

    const stats = optimizer.getStatistics();
    expect(stats[0].avgResultCount).toBe(0);
    expect(stats[0].selectivity).toBe(0);
  });

  test('handles very large result counts', () => {
    optimizer.updateStatistics('sig1', 1000000, 1000000);

    const stats = optimizer.getStatistics();
    expect(stats[0].avgResultCount).toBe(1000000);
  });

  test('handles mixed query and non-query steps', () => {
    const steps = [
      createMockStep('step_0', 'sig1'),
      {
        ...createMockStep('step_1', 'sig2'),
        type: 'action' as const,
      },
      createMockStep('step_2', 'sig3'),
    ];

    optimizer.updateStatistics('sig1', 100);
    optimizer.updateStatistics('sig3', 10);

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Should only reorder query steps
    const querySteps = optimized.filter(s => s.type === 'query');
    expect(querySteps).toHaveLength(2);
  });
});

describe('JoinOptimizer - Realistic Scenarios', () => {
  let optimizer: JoinOptimizer;

  beforeEach(() => {
    optimizer = new JoinOptimizer();
  });

  test('scenario: find high-priority tasks for active users', () => {
    // Query: Find tasks (common) assigned to users (rare) with high priority (very rare)
    const steps = [
      createMockStep('tasks', 'task-all', 'tasks'),           // 10000 results
      createMockStep('users', 'user-active', 'users'),        // 50 results
      createMockStep('priority', 'task-high-priority', 'tasks'), // 100 results
    ];

    optimizer.updateStatistics('task-all', 10000, 10000);
    optimizer.updateStatistics('user-active', 50, 1000);
    optimizer.updateStatistics('task-high-priority', 100, 10000);

    const optimized = optimizer.optimizeJoinOrder(steps);

    // Should start with users (most selective)
    expect(optimized[0].id).toBe('users');
  });

  test('scenario: warm vs cold with comparable selectivity', () => {
    const steps = [
      createMockStep('step_0', 'sig-cold', 'service-a'),  // Cold, selective
      createMockStep('step_1', 'sig-warm', 'service-b'),  // Warm, slightly less selective
    ];

    optimizer.updateStatistics('sig-cold', 20, 1000);
    optimizer.updateStatistics('sig-warm', 30, 1000);

    const context = createContext(['service-b']);

    const optimized = optimizer.optimizeJoinOrder(steps, context);

    // Warm actor should win despite slightly lower selectivity
    expect(optimized[0].actor).toBe('@(service-b)');
  });

  test('scenario: gradually learning selectivity', () => {
    const steps = [
      createMockStep('step_0', 'sig1'),
      createMockStep('step_1', 'sig2'),
    ];

    // Initially, assume sig1 is more selective
    optimizer.updateStatistics('sig1', 10);
    optimizer.updateStatistics('sig2', 100);

    let optimized = optimizer.optimizeJoinOrder(steps);
    expect(optimized[0].signature).toBe('sig1');

    // But over time, learn that sig2 is actually more selective
    // Need more iterations for EMA (alpha=0.1) to converge
    for (let i = 0; i < 30; i++) {
      optimizer.updateStatistics('sig1', 50); // Actually medium
      optimizer.updateStatistics('sig2', 5);  // Actually high
    }

    optimized = optimizer.optimizeJoinOrder(steps);
    // After 30 iterations, sig2 should be recognized as more selective
    expect(optimized[0].signature).toBe('sig2'); // Now sig2 comes first
  });
});
