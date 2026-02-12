#!/usr/bin/env bun
/**
 * Predicate Pushdown Integration Tests
 *
 * Tests end-to-end integration with QueryCompiler and validates
 * that optimization preserves query semantics.
 */

import { test, expect, describe } from 'bun:test';
import { QueryCompiler } from '../compiler.ts';
import { query } from '../builder.ts';
import { pattern, filter, logic } from '../pattern.ts';
import type { ExecutionContext, QueryPlan } from '../types.ts';
import { address } from '@agentic-primer/actors';

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

describe('Predicate Pushdown Integration - Compiler Integration', () => {
  test('compiler applies predicate pushdown by default', async () => {
    const compiler = new QueryCompiler({ enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('open'))
      .build();

    const plan = await compiler.compile(q, context);

    // Check that filter was pushed to query step
    const queryStep = plan.steps.find(s => s.type === 'query');
    expect(queryStep).toBeDefined();
    expect(queryStep!.message.payload.filter).toBeDefined();
  });

  test('can disable predicate pushdown', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: false, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('open'))
      .build();

    const plan = await compiler.compile(q, context);

    // Filter should not be in query payload when disabled
    // (it stays in the original query definition only)
    expect(plan.original.filters).toHaveLength(1);
  });

  test('preserves query plan structure', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('open'))
      .build();

    const planWithOpt = await compiler.compile(q, context);

    // Should have same number of steps
    expect(planWithOpt.steps).toHaveLength(1);

    // Should have same variables
    expect(planWithOpt.variables).toContain('task');

    // Should preserve original query
    expect(planWithOpt.original).toEqual(q);
  });
});

describe('Predicate Pushdown Integration - Semantics Preservation', () => {
  test('preserves filter semantics for equality', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('open'))
      .build();

    const plan = await compiler.compile(q, context);

    // Filter should be in payload with correct value
    const queryStep = plan.steps[0];
    expect(queryStep.message.payload.filter).toHaveProperty('status', 'open');
  });

  test('preserves filter semantics for comparisons', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'priority').gt(5))
      .build();

    const plan = await compiler.compile(q, context);

    // Filter should be in payload with operator
    const queryStep = plan.steps[0];
    expect(queryStep.message.payload.filter).toHaveProperty('priority');
    expect(queryStep.message.payload.filter.priority).toHaveProperty('$gt', 5);
  });

  test('preserves AND filter semantics', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(
        logic.and(
          filter('task', 'status').eq('open'),
          filter('task', 'priority').eq('high')
        )
      )
      .build();

    const plan = await compiler.compile(q, context);

    // Both filters should be in payload
    const queryStep = plan.steps[0];
    expect(queryStep.message.payload.filter).toHaveProperty('status', 'open');
    expect(queryStep.message.payload.filter).toHaveProperty('priority', 'high');
  });

  test('handles multiple patterns correctly', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .where(
        filter('task', 'status').eq('open'),
        filter('user', 'active').eq(true)
      )
      .build();

    const plan = await compiler.compile(q, context);

    // Each filter should be pushed to its respective step
    expect(plan.steps).toHaveLength(2);

    const taskStep = plan.steps.find(s => s.bindings.includes('task'));
    const userStep = plan.steps.find(s => s.bindings.includes('user'));

    expect(taskStep?.message.payload.filter).toHaveProperty('status');
    expect(userStep?.message.payload.filter).toHaveProperty('active');
  });
});

describe('Predicate Pushdown Integration - Safety', () => {
  test('does not push filters to traversal steps', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps'
      })
      .where(filter('deps', 'status').eq('open'))
      .build();

    const plan = await compiler.compile(q, context);

    // Find traversal step
    const traverseStep = plan.steps.find(s => s.type === 'traverse');
    expect(traverseStep).toBeDefined();

    // Filter should NOT be in traversal step
    expect(traverseStep?.message.payload.filter).toBeUndefined();
  });

  test('handles queries without filters gracefully', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q, context);

    expect(plan).toBeDefined();
    expect(plan.steps).toHaveLength(1);
  });

  test('handles OR filters conservatively', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(
        logic.or(
          filter('task', 'priority').eq('high'),
          filter('task', 'priority').eq('urgent')
        )
      )
      .build();

    const plan = await compiler.compile(q, context);

    // OR filters may not be pushed down (implementation dependent)
    expect(plan).toBeDefined();
    expect(plan.steps).toHaveLength(1);
  });
});

describe('Predicate Pushdown Integration - Cost Estimation', () => {
  test('updates cost estimates after optimization', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(
        filter('task', 'status').eq('open'),
        filter('task', 'priority').eq('high')
      )
      .build();

    const plan = await compiler.compile(q, context);

    // Cost metadata should be present
    expect(plan.metadata.estimatedCost).toBeDefined();
    expect(plan.metadata.estimatedCost.makespan).toBeGreaterThan(0);

    // Resource usage should be estimated
    expect(plan.metadata.estimatedCost.resourceUsage.memoryBytes).toBeGreaterThan(0);
  });

  test('reduces memory estimate with filters', async () => {
    const compilerNoOpt = new QueryCompiler({ enablePredicatePushdown: false, enableJoinOptimization: false });
    const compilerOpt = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('open'))
      .build();

    const planNoOpt = await compilerNoOpt.compile(q, context);
    const planOpt = await compilerOpt.compile(q, context);

    // Optimized plan should have lower or equal memory estimate
    expect(planOpt.metadata.estimatedCost.resourceUsage.memoryBytes)
      .toBeLessThanOrEqual(planNoOpt.metadata.estimatedCost.resourceUsage.memoryBytes);
  });
});

describe('Predicate Pushdown Integration - Complex Queries', () => {
  test('handles complex multi-step queries', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps'
      })
      .where(filter('root', 'status').eq('open'))
      .build();

    const plan = await compiler.compile(q, context);

    // Should have query + traversal steps
    expect(plan.steps.length).toBeGreaterThanOrEqual(2);

    // Filter should be pushed to root query
    const rootStep = plan.steps.find(s => s.bindings.includes('root'));
    expect(rootStep?.message.payload.filter).toHaveProperty('status', 'open');
  });

  test('handles queries with actions', async () => {
    const compiler = new QueryCompiler({ enablePredicatePushdown: true, enableJoinOptimization: false });
    const context = createContext();

    const { send } = await import('../builder.ts');
    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('ready'))
      .forEach(send('task').tell('start'))
      .build();

    const plan = await compiler.compile(q, context);

    // Should have query + action steps
    expect(plan.steps.length).toBeGreaterThanOrEqual(2);

    // Filter should be pushed to query
    const queryStep = plan.steps.find(s => s.type === 'query');
    expect(queryStep?.message.payload.filter).toHaveProperty('status', 'ready');
  });
});
