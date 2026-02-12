#!/usr/bin/env bun
/**
 * Query Compiler Tests
 * Tests for src/query/compiler.ts
 * Target: >90% coverage
 */

import { test, expect, describe } from 'bun:test';
import { QueryCompiler } from './compiler.ts';
import { query, send } from './builder.ts';
import { pattern } from './pattern.ts';
import type { ExecutionContext, QueryPlan } from './types.ts';
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

describe('QueryCompiler - Construction', () => {
  test('creates compiler instance', () => {
    const compiler = new QueryCompiler();
    expect(compiler).toBeDefined();
  });
});

describe('QueryCompiler - Basic Compilation', () => {
  test('compiles simple pattern query', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    const plan = await compiler.compile(q);

    expect(plan.id).toBeDefined();
    expect(plan.steps).toHaveLength(1);
    expect(plan.steps[0].type).toBe('query');
    expect(plan.variables).toContain('task');
  });

  test('compiles query with multiple patterns', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps).toHaveLength(2);
  });

  test('assigns unique step IDs', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .build();

    const plan = await compiler.compile(q);
    const stepIds = plan.steps.map(s => s.id);
    expect(new Set(stepIds).size).toBe(stepIds.length);
  });

  test('generates consistent plan ID for same query', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan1 = await compiler.compile(q);
    const plan2 = await compiler.compile(q);

    expect(plan1.id).toBe(plan2.id);
  });

  test('generates different plan IDs for different queries', async () => {
    const compiler = new QueryCompiler();
    const q1 = query()
      .match(pattern('task').label('Task'))
      .build();
    const q2 = query()
      .match(pattern('user').label('User'))
      .build();

    const plan1 = await compiler.compile(q1);
    const plan2 = await compiler.compile(q2);

    expect(plan1.id).not.toBe(plan2.id);
  });
});

describe('QueryCompiler - Pattern Steps', () => {
  test('creates query step with correct actor address', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[0].actor).toBe('@(tasks)');
  });

  test('includes filter in query payload', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open', priority: 'high' }))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[0].message.payload.filter).toEqual({
      status: 'open',
      priority: 'high',
    });
  });

  test('marks pattern steps as parallelizable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[0].parallelizable).toBe(true);
    expect(plan.steps[1].parallelizable).toBe(true);
  });

  test('generates operation signatures', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[0].signature).toBeDefined();
    expect(plan.steps[0].signature).toHaveLength(16);
  });

  test('same patterns generate same signatures', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task1').label('Task').where({ status: 'open' }),
        pattern('task2').label('Task').where({ status: 'open' })
      )
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[0].signature).toBe(plan.steps[1].signature);
  });
});

describe('QueryCompiler - Traversal Steps', () => {
  test('compiles traversal step', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'dependencies',
      })
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps).toHaveLength(2);
    expect(plan.steps[1].type).toBe('traverse');
  });

  test('traversal depends on source pattern', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[1].dependencies).toContain(plan.steps[0].id);
  });

  test('traversal is not parallelizable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[1].parallelizable).toBe(false);
  });

  test('throws if traversal references unknown variable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .traverse({
        from: 'unknownVar',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();

    await expect(compiler.compile(q)).rejects.toThrow('unknown variable');
  });

  test('includes traversal parameters in message', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        depth: { min: 1, max: 5 },
        as: 'deps',
      })
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[1].message.payload).toMatchObject({
      relationship: 'requires',
      direction: 'outbound',
      depth: { min: 1, max: 5 },
    });
  });
});

describe('QueryCompiler - Action Steps', () => {
  test('compiles action step', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(send('task').tell('start'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps).toHaveLength(2);
    expect(plan.steps[1].type).toBe('action');
  });

  test('action depends on target variable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(send('task').tell('start'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[1].dependencies).toContain(plan.steps[0].id);
  });

  test('action is parallelizable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(send('task').tell('start'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[1].parallelizable).toBe(true);
  });

  test('throws if action references unknown variable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(send('unknownVar').tell('start'))
      .build();

    await expect(compiler.compile(q)).rejects.toThrow('unknown variable');
  });

  test('actions do not produce bindings', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(send('task').tell('start'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[1].bindings).toEqual([]);
  });
});

describe('QueryCompiler - Variable Extraction', () => {
  test('extracts variables from patterns', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .build();

    const plan = await compiler.compile(q);
    expect(plan.variables).toContain('task');
    expect(plan.variables).toContain('user');
  });

  test('extracts variables from traversals', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();

    const plan = await compiler.compile(q);
    expect(plan.variables).toContain('root');
    expect(plan.variables).toContain('deps');
  });

  test('deduplicates variables', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .traverse({
        from: 'task',
        relationship: 'rel',
        direction: 'outbound',
        as: 'related',
      })
      .build();

    const plan = await compiler.compile(q);
    const taskCount = plan.variables.filter(v => v === 'task').length;
    expect(taskCount).toBe(1);
  });
});

describe('QueryCompiler - Cost Estimation', () => {
  test('includes cost metadata in plan', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.metadata.estimatedCost).toBeDefined();
    expect(plan.metadata.estimatedCost.makespan).toBeGreaterThan(0);
  });

  test('each step has cost estimate', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    for (const step of plan.steps) {
      expect(step.cost.latencyMs).toBeGreaterThan(0);
      expect(step.cost.cpuMs).toBeGreaterThan(0);
    }
  });

  test('warm actors reduce cost', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const warmContext = createContext();
    warmContext.warmActors.add(address('tasks'));

    const coldContext = createContext();
    coldContext.warmActors.clear();

    const warmPlan = await compiler.compile(q, warmContext);
    const coldPlan = await compiler.compile(q, coldContext);

    expect(warmPlan.steps[0].cost.latencyMs).toBeLessThan(
      coldPlan.steps[0].cost.latencyMs
    );
  });

  test('calculates makespan correctly', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();

    const plan = await compiler.compile(q);
    const totalStepCost = plan.steps.reduce((sum, s) => sum + s.cost.latencyMs, 0);

    // Makespan should be >= longest single path
    expect(plan.metadata.estimatedCost.makespan).toBeGreaterThanOrEqual(
      plan.steps[0].cost.latencyMs
    );
  });

  test('calculates total work', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .build();

    const plan = await compiler.compile(q);
    const stepSum = plan.steps.reduce((sum, s) => sum + s.cost.latencyMs, 0);
    expect(plan.metadata.estimatedCost.totalWork).toBe(stepSum);
  });

  test('estimates resource usage', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.metadata.estimatedCost.resourceUsage.memoryBytes).toBeGreaterThan(0);
    expect(plan.metadata.estimatedCost.resourceUsage.messageCount).toBe(plan.steps.length);
  });
});

describe('QueryCompiler - Plan Metadata', () => {
  test('includes compilation timestamp', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const before = Date.now();
    const plan = await compiler.compile(q);
    const after = Date.now();

    expect(plan.metadata.compiledAt).toBeGreaterThanOrEqual(before);
    expect(plan.metadata.compiledAt).toBeLessThanOrEqual(after);
  });

  test('marks plan as parallelizable if any step is', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .build();

    const plan = await compiler.compile(q);
    expect(plan.metadata.parallelizable).toBe(true);
  });

  test('calculates critical path length', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .build();

    const plan = await compiler.compile(q);
    expect(plan.metadata.criticalPathSteps).toBe(2); // root + traversal
  });

  test('preserves original query', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.original).toEqual(q);
  });
});

describe('QueryCompiler - Complex Queries', () => {
  test('compiles multi-step query correctly', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'deps',
      })
      .forEach(send('deps').tell('check'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps).toHaveLength(3);

    // Check dependency chain
    expect(plan.steps[0].dependencies).toHaveLength(0);
    expect(plan.steps[1].dependencies).toContain(plan.steps[0].id);
    expect(plan.steps[2].dependencies).toContain(plan.steps[1].id);
  });

  test('handles queries with multiple independent patterns', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User'),
        pattern('doc').label('Document')
      )
      .build();

    const plan = await compiler.compile(q);

    // All independent patterns have no dependencies
    for (const step of plan.steps) {
      expect(step.dependencies).toHaveLength(0);
    }
  });
});

describe('Edge Cases', () => {
  test('compiles empty query', async () => {
    const compiler = new QueryCompiler();
    const q = query().build();

    const plan = await compiler.compile(q);
    expect(plan.steps).toHaveLength(0);
    expect(plan.variables).toHaveLength(0);
  });

  test('handles query with no filters', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[0].message.payload.filter).toEqual({});
  });

  test('handles undefined context', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q);
    expect(plan).toBeDefined();
  });
});
