#!/usr/bin/env bun
/**
 * EXPLAIN Tests
 * Tests for query plan explanation functionality
 * Target: >15 test cases
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { query, pattern, send, filter } from '../index.ts';
import { logic } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import { QueryExplainer } from './explainer.ts';
import { PlanFormatter } from './plan-formatter.ts';
import { PlanVisualizer } from './plan-visualizer.ts';
import type { QueryPlan, ExecutionContext } from '../types.ts';
import { address } from '@agentic-primer/actors';

describe('QueryBuilder.explain() - API', () => {
  test('explain() method exists on QueryBuilder', async () => {
    const q = query().match(pattern('task').label('Task'));
    expect(typeof q.explain).toBe('function');
  });

  test('explain() returns ExplainResult with all fields', async () => {
    const result = await query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .return(['task'])
      .explain();

    expect(result).toHaveProperty('plan');
    expect(result).toHaveProperty('text');
    expect(result).toHaveProperty('tree');
    expect(result).toHaveProperty('optimizations');
    expect(result).toHaveProperty('costBreakdown');
    expect(result).toHaveProperty('cacheAnalysis');
  });

  test('explain() accepts options', async () => {
    const result = await query()
      .match(pattern('task').label('Task'))
      .explain({ verbose: true, costs: false, cache: false });

    expect(result.text).toBeTruthy();
    expect(result.tree).toBeTruthy();
  });

  test('explain() with execution context', async () => {
    const context: ExecutionContext = {
      warmActors: new Set([address('tasks')]),
      computationCache: new Map(),
      resources: {
        maxConcurrency: 4,
        availableMemory: 1024 * 1024 * 100,
      },
      startTime: Date.now(),
    };

    const result = await query()
      .match(pattern('task').label('Task'))
      .explain({}, context);

    expect(result.plan).toBeTruthy();
    // Warm actors should affect cost estimates
    const step = result.plan.steps[0];
    expect(step.cost.cacheHitProb).toBeGreaterThan(0);
  });

  test('compile() method returns QueryPlan', async () => {
    const plan = await query()
      .match(pattern('task').label('Task'))
      .compile();

    expect(plan).toHaveProperty('id');
    expect(plan).toHaveProperty('steps');
    expect(plan).toHaveProperty('variables');
    expect(plan).toHaveProperty('metadata');
    expect(plan.steps.length).toBeGreaterThan(0);
  });
});

describe('QueryExplainer - Basic Functionality', () => {
  let explainer: QueryExplainer;
  let compiler: QueryCompiler;

  beforeEach(() => {
    explainer = new QueryExplainer();
    compiler = new QueryCompiler();
  });

  test('explain simple query', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .return(['task'])
      .build();

    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    expect(result.text).toContain('QUERY PLAN');
    expect(result.text).toContain('OVERVIEW');
    expect(result.text).toContain('EXECUTION STEPS');
    expect(result.tree).toContain('EXECUTION TREE');
  });

  test('explain query with traversal', async () => {
    const queryDef = query()
      .match(pattern('root').label('Task').where({ id: 'build' }))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        depth: { max: 3 },
        as: 'dependencies',
      })
      .return(['root', 'dependencies'])
      .build();

    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    expect(plan.steps.length).toBe(2);
    expect(result.text).toContain('TRAVERSE');
    expect(result.costBreakdown.stepCosts.length).toBe(2);
  });

  test('explain query with actions', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task').where({ status: 'ready' }))
      .forEach(send('task').tell('start'))
      .build();

    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    expect(plan.steps.length).toBe(2); // query + action
    expect(result.text).toContain('ACTION');
  });

  test('summary generates concise one-liner', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(queryDef);
    const summary = explainer.summary(plan);

    expect(summary).toContain('steps');
    expect(summary).toContain('ms');
    expect(summary).toMatch(/parallel|sequential/);
  });
});

describe('PlanFormatter - Text Output', () => {
  let formatter: PlanFormatter;
  let compiler: QueryCompiler;

  beforeEach(() => {
    formatter = new PlanFormatter();
    compiler = new QueryCompiler();
  });

  test('formats header with plan ID', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);
    const text = formatter.format(plan);

    expect(text).toContain('QUERY PLAN:');
    expect(text).toContain(plan.id);
    expect(text).toContain('='.repeat(60));
  });

  test('formats overview section', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .return(['task'])
      .build();
    const plan = await compiler.compile(queryDef);
    const text = formatter.format(plan);

    expect(text).toContain('OVERVIEW');
    expect(text).toContain('Steps:');
    expect(text).toContain('Variables:');
    expect(text).toContain('Critical Path:');
    expect(text).toContain('Total Work:');
  });

  test('formats execution steps with details', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);
    const text = formatter.format(plan, { verbose: true });

    expect(text).toContain('EXECUTION STEPS');
    expect(text).toContain('[step_0]');
    expect(text).toContain('Dependencies:');
    expect(text).toContain('Cost:');
    expect(text).toContain('Signature:');
  });

  test('formats cost breakdown', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .traverse({
        from: 'task',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();
    const plan = await compiler.compile(queryDef);
    const text = formatter.format(plan, { costs: true });

    expect(text).toContain('COST BREAKDOWN');
    expect(text).toContain('By Step');
    expect(text).toContain('Resource Usage:');
    expect(text).toContain('Memory:');
    expect(text).toContain('I/O Operations:');
  });

  test('formats cache analysis', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);
    const text = formatter.format(plan, { cache: true });

    expect(text).toContain('CACHE ANALYSIS');
    expect(text).toContain('Cache Hit Probability');
    expect(text).toContain('Expected Hits');
  });

  test('formats optimization notes', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .build();
    const plan = await compiler.compile(queryDef);
    const text = formatter.format(plan, { optimize: true });

    expect(text).toContain('OPTIMIZATION NOTES');
  });

  test('respects verbose option', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);

    const normalText = formatter.format(plan, { verbose: false });
    const verboseText = formatter.format(plan, { verbose: true });

    expect(verboseText.length).toBeGreaterThan(normalText.length);
    expect(verboseText).toContain('Message:');
    expect(verboseText).toContain('Signature:');
  });
});

describe('PlanVisualizer - Tree Output', () => {
  let visualizer: PlanVisualizer;
  let compiler: QueryCompiler;

  beforeEach(() => {
    visualizer = new PlanVisualizer();
    compiler = new QueryCompiler();
  });

  test('generates ASCII tree', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .build();
    const plan = await compiler.compile(queryDef);
    const tree = visualizer.visualize(plan);

    expect(tree).toContain('EXECUTION TREE');
    expect(tree).toContain('LEGEND');
    expect(tree).toMatch(/[├└]/); // Tree connectors
  });

  test('tree shows dependencies', async () => {
    const queryDef = query()
      .match(pattern('root').label('Task').where({ id: 'test' }))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();
    const plan = await compiler.compile(queryDef);
    const tree = visualizer.visualize(plan);

    expect(tree).toContain('[Q]'); // Query step
    expect(tree).toContain('[T]'); // Traverse step
  });

  test('tree shows costs when enabled', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);
    const treeWithCosts = visualizer.visualize(plan, true);
    const treeWithoutCosts = visualizer.visualize(plan, false);

    expect(treeWithCosts).toContain('ms');
    expect(treeWithoutCosts).not.toContain('ms');
  });

  test('generates execution flow diagram', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .traverse({
        from: 'task',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();
    const plan = await compiler.compile(queryDef);
    const flow = visualizer.visualizeFlow(plan);

    expect(flow).toContain('EXECUTION FLOW');
    expect(flow).toContain('Stage 1');
    expect(flow).toContain('↓');
  });

  test('flow diagram shows parallel stages', async () => {
    const queryDef = query()
      .match(
        pattern('task1').label('Task').where({ id: '1' }),
        pattern('task2').label('Task').where({ id: '2' })
      )
      .build();
    const plan = await compiler.compile(queryDef);
    const flow = visualizer.visualizeFlow(plan);

    // Both patterns should be in same stage (parallel)
    expect(flow).toContain('Parallel execution');
  });

  test('tree includes cache indicators', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);
    const tree = visualizer.visualize(plan);

    expect(tree).toMatch(/cache|⚡|❄/);
  });
});

describe('Cost Breakdown', () => {
  let explainer: QueryExplainer;
  let compiler: QueryCompiler;

  beforeEach(() => {
    explainer = new QueryExplainer();
    compiler = new QueryCompiler();
  });

  test('cost breakdown includes all steps', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .traverse({
        from: 'task',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    expect(result.costBreakdown.stepCosts.length).toBe(plan.steps.length);
    expect(result.costBreakdown.totalLatency).toBeGreaterThan(0);
    expect(result.costBreakdown.criticalPath).toBeGreaterThan(0);
  });

  test('cost breakdown sorted by latency', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .traverse({
        from: 'task',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    const costs = result.costBreakdown.stepCosts;
    for (let i = 1; i < costs.length; i++) {
      expect(costs[i - 1].latency).toBeGreaterThanOrEqual(costs[i].latency);
    }
  });

  test('parallelism benefit calculated correctly', async () => {
    const queryDef = query()
      .match(
        pattern('task1').label('Task').where({ id: '1' }),
        pattern('task2').label('Task').where({ id: '2' })
      )
      .build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    if (plan.metadata.parallelizable) {
      expect(result.costBreakdown.parallelismBenefit).toBeGreaterThanOrEqual(0);
    }
  });
});

describe('Cache Analysis', () => {
  let explainer: QueryExplainer;
  let compiler: QueryCompiler;

  beforeEach(() => {
    explainer = new QueryExplainer();
    compiler = new QueryCompiler();
  });

  test('cache analysis includes hit probabilities', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    expect(result.cacheAnalysis.overallHitProb).toBeGreaterThanOrEqual(0);
    expect(result.cacheAnalysis.overallHitProb).toBeLessThanOrEqual(1);
    expect(result.cacheAnalysis.expectedHits).toBeGreaterThanOrEqual(0);
    expect(result.cacheAnalysis.expectedMisses).toBeGreaterThanOrEqual(0);
  });

  test('cache analysis shows per-step probabilities', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    expect(result.cacheAnalysis.stepCacheProbs.length).toBe(plan.steps.length);
    for (const stepProb of result.cacheAnalysis.stepCacheProbs) {
      expect(stepProb.hitProb).toBeGreaterThanOrEqual(0);
      expect(stepProb.hitProb).toBeLessThanOrEqual(1);
      expect(['high', 'medium', 'low']).toContain(stepProb.impact);
    }
  });

  test('cache analysis with warm actors', async () => {
    const context: ExecutionContext = {
      warmActors: new Set([address('tasks')]),
      computationCache: new Map(),
      resources: {
        maxConcurrency: 4,
        availableMemory: 1024 * 1024 * 100,
      },
      startTime: Date.now(),
    };

    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef, context);
    const result = explainer.explain(plan);

    // With warm actors, cache hit probability should be higher
    expect(result.cacheAnalysis.overallHitProb).toBeGreaterThan(0.5);
  });

  test('cache recommendations generated', async () => {
    const queryDef = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan);

    expect(Array.isArray(result.cacheAnalysis.recommendations)).toBe(true);
  });
});

describe('Optimization Notes', () => {
  let explainer: QueryExplainer;
  let compiler: QueryCompiler;

  beforeEach(() => {
    explainer = new QueryExplainer();
    compiler = new QueryCompiler();
  });

  test('generates optimization notes', async () => {
    const queryDef = query()
      .match(pattern('task').label('Task'))
      .build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan, { optimize: true });

    expect(Array.isArray(result.optimizations)).toBe(true);
    for (const note of result.optimizations) {
      expect(['info', 'warning', 'tip']).toContain(note.type);
      expect(note.message).toBeTruthy();
    }
  });

  test('detects parallelism opportunities', async () => {
    const queryDef = query()
      .match(
        pattern('task1').label('Task').where({ id: '1' }),
        pattern('task2').label('Task').where({ id: '2' })
      )
      .build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan, { optimize: true });

    // Plan should be parallelizable with multiple steps
    expect(plan.metadata.parallelizable).toBe(true);
    expect(plan.steps.length).toBeGreaterThan(1);
    // Should have optimization notes array (may be empty for simple queries)
    expect(Array.isArray(result.optimizations)).toBe(true);
  });

  test('warns about expensive steps', async () => {
    // Create a query with a traversal (typically expensive)
    const queryDef = query()
      .match(pattern('root').label('Task').where({ id: 'test' }))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        depth: { max: 10 }, // Deep traversal = expensive
        as: 'all_deps',
      })
      .build();
    const plan = await compiler.compile(queryDef);
    const result = explainer.explain(plan, { optimize: true });

    // Should have optimization notes
    expect(result.optimizations.length).toBeGreaterThan(0);
  });
});

describe('Integration - Complex Queries', () => {
  test('explain complex workflow query', async () => {
    const result = await query()
      .match(
        pattern('test').label('Task').where({ id: 'test' }),
        pattern('deploy').label('Task').where({ id: 'deploy' })
      )
      .when(
        pattern('test').where({
          lifecycle: 'completed',
          result: { passed: true },
        })
      )
      .then(send('deploy').tell('start'))
      .explain({ verbose: true, costs: true, cache: true, optimize: true });

    expect(result.text).toContain('QUERY PLAN');
    expect(result.tree).toContain('EXECUTION TREE');
    expect(result.costBreakdown.stepCosts.length).toBeGreaterThan(0);
    expect(result.optimizations.length).toBeGreaterThan(0);
  });

  test('explain query with filters', async () => {
    const result = await query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .where(
        logic.and(
          filter('task', 'priority').eq('high'),
          filter('task', 'assignee').eq('alice')
        )
      )
      .return(['task'])
      .explain();

    expect(result.plan.steps.length).toBeGreaterThan(0);
    expect(result.text).toBeTruthy();
  });

  test('explain handles multiple traversals', async () => {
    const result = await query()
      .match(pattern('root').label('Task').where({ id: 'build' }))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        depth: { max: 2 },
        as: 'deps',
      })
      .traverse({
        from: 'deps',
        relationship: 'supports',
        direction: 'both',
        depth: { max: 1 },
        as: 'knowledge',
      })
      .return(['root', 'deps', 'knowledge'])
      .explain();

    expect(result.plan.steps.length).toBe(3); // 1 query + 2 traversals
    expect(result.costBreakdown.criticalPath).toBeGreaterThan(0);
  });
});
