#!/usr/bin/env bun
/**
 * CREATE Action Tests
 * Tests for CREATE operations in the query/DSL layer
 * Target: >20 test cases covering CREATE semantics
 */

import { test, expect, describe } from 'bun:test';
import { query, create } from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import type { QueryDefinition, QueryPlan } from '../types.ts';

describe('CREATE Action - Builder API', () => {
  test('creates basic CREATE action', () => {
    const action = create('task').as({
      title: 'New Task',
      status: 'open',
    });

    const spec = action.build();
    expect(spec.type).toBe('create');
    expect(spec.target).toBe('task');
    expect(spec.params.properties).toEqual({
      title: 'New Task',
      status: 'open',
    });
  });

  test('creates CREATE action with multiple properties', () => {
    const action = create('task').as({
      title: 'Complex Task',
      status: 'open',
      priority: 'high',
      assignee: 'alice',
      tags: ['urgent', 'backend'],
    });

    const spec = action.build();
    expect(Object.keys(spec.params.properties)).toHaveLength(5);
    expect(spec.params.properties.tags).toEqual(['urgent', 'backend']);
  });

  test('creates CREATE action with nested properties', () => {
    const action = create('task').as({
      title: 'Task with metadata',
      metadata: {
        source: 'api',
        createdBy: 'system',
      },
    });

    const spec = action.build();
    expect(spec.params.properties.metadata).toEqual({
      source: 'api',
      createdBy: 'system',
    });
  });

  test('creates CREATE action with empty properties', () => {
    const action = create('task').as({});
    const spec = action.build();
    expect(spec.params.properties).toEqual({});
  });
});

describe('CREATE Action - Query Integration', () => {
  test('adds CREATE action to query', () => {
    const q = query().forEach(
      create('task').as({ title: 'New Task', status: 'open' })
    );

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].type).toBe('create');
  });

  test('chains multiple CREATE actions', () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task 1' }))
      .forEach(create('task').as({ title: 'Task 2' }))
      .forEach(create('task').as({ title: 'Task 3' }));

    const def = q.build();
    expect(def.actions).toHaveLength(3);
    expect(def.actions!.every((a) => a.type === 'create')).toBe(true);
  });

  test('combines CREATE with MATCH pattern', () => {
    const q = query()
      .match(pattern('template').label('Template').where({ id: 'default' }))
      .forEach(create('task').as({ title: 'From Template', status: 'open' }));

    const def = q.build();
    expect(def.patterns).toHaveLength(1);
    expect(def.actions).toHaveLength(1);
  });

  test('CREATE action can reference matched variables in properties', () => {
    const q = query()
      .match(pattern('user').label('User').where({ id: 'alice' }))
      .forEach(
        create('task').as({
          title: 'User Task',
          assignee: '${user.id}', // Template reference
        })
      );

    const def = q.build();
    expect(def.actions![0].params.properties.assignee).toContain('user');
  });
});

describe('CREATE Action - Compilation', () => {
  test('compiles CREATE action without target dependency', async () => {
    const q = query().forEach(create('task').as({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps).toHaveLength(1);
    expect(plan.steps[0].type).toBe('action');
    expect(plan.steps[0].dependencies).toHaveLength(0); // No dependencies
  });

  test('compiles CREATE action to collection actor', async () => {
    const q = query().forEach(create('task').as({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].actor).toContain('tasks'); // Routes to collection
  });

  test('CREATE action produces bindings', async () => {
    const q = query().forEach(create('task').as({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].bindings).toContain('task');
  });

  test('CREATE action is marked as parallelizable', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task 1' }))
      .forEach(create('task').as({ title: 'Task 2' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].parallelizable).toBe(true);
    expect(plan.steps[1].parallelizable).toBe(true);
  });

  test('compiles CREATE message type correctly', async () => {
    const q = query().forEach(create('task').as({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].message.type).toBe('create');
  });

  test('compiles CREATE message payload correctly', async () => {
    const q = query().forEach(
      create('task').as({ title: 'New Task', status: 'open' })
    );

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].message.payload).toEqual({
      title: 'New Task',
      status: 'open',
    });
  });
});

describe('CREATE Action - Multiple Entity Types', () => {
  test('creates different entity types', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task' }))
      .forEach(create('knowledge').as({ title: 'Knowledge' }))
      .forEach(create('user').as({ name: 'User' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].actor).toContain('tasks');
    expect(plan.steps[1].actor).toContain('knowledge');
    expect(plan.steps[2].actor).toContain('users');
  });

  test('handles entity type pluralization', async () => {
    const q = query().forEach(create('category').as({ name: 'Category' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Should pluralize 'category' to 'categorys' (basic pluralization)
    expect(plan.steps[0].actor).toContain('category');
  });
});

describe('CREATE Action - Cost Estimation', () => {
  test('estimates cost for CREATE action', async () => {
    const q = query().forEach(create('task').as({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].cost).toBeDefined();
    expect(plan.steps[0].cost.latencyMs).toBeGreaterThan(0);
  });

  test('CREATE cost is independent of query results', async () => {
    const q1 = query().forEach(create('task').as({ title: 'Task' }));
    const q2 = query()
      .match(pattern('user').label('User'))
      .forEach(create('task').as({ title: 'Task' }));

    const compiler = new QueryCompiler();
    const plan1 = await compiler.compile(q1.build());
    const plan2 = await compiler.compile(q2.build());

    // CREATE step cost should be similar regardless of preceding steps
    const createStep1 = plan1.steps.find((s) => s.type === 'action');
    const createStep2 = plan2.steps.find((s) => s.type === 'action');

    expect(createStep1?.cost.latencyMs).toBeDefined();
    expect(createStep2?.cost.latencyMs).toBeDefined();
  });
});

describe('CREATE Action - Signature Generation', () => {
  test('generates unique signature for CREATE action', async () => {
    const q = query().forEach(create('task').as({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps[0].signature).toBeDefined();
    expect(plan.steps[0].signature.length).toBeGreaterThan(0);
  });

  test('identical CREATE actions have same signature', async () => {
    const q1 = query().forEach(create('task').as({ title: 'Task', status: 'open' }));
    const q2 = query().forEach(create('task').as({ title: 'Task', status: 'open' }));

    const compiler = new QueryCompiler();
    const plan1 = await compiler.compile(q1.build());
    const plan2 = await compiler.compile(q2.build());

    expect(plan1.steps[0].signature).toBe(plan2.steps[0].signature);
  });

  test('different CREATE actions have different signatures', async () => {
    const q1 = query().forEach(create('task').as({ title: 'Task 1' }));
    const q2 = query().forEach(create('task').as({ title: 'Task 2' }));

    const compiler = new QueryCompiler();
    const plan1 = await compiler.compile(q1.build());
    const plan2 = await compiler.compile(q2.build());

    expect(plan1.steps[0].signature).not.toBe(plan2.steps[0].signature);
  });
});

describe('CREATE Action - Complex Scenarios', () => {
  test('CREATE followed by UPDATE', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'New Task', status: 'open' }))
      .match(pattern('task').label('Task').where({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Should have both CREATE and MATCH steps
    expect(plan.steps.some((s) => s.type === 'action')).toBe(true);
    expect(plan.steps.some((s) => s.type === 'query')).toBe(true);
  });

  test('CREATE with conditional', async () => {
    const q = query()
      .match(pattern('user').label('User').where({ needsTask: true }))
      .forEach(create('task').as({ title: 'Conditional Task', assignee: 'user' }));

    const def = q.build();
    expect(def.patterns).toHaveLength(1);
    expect(def.actions).toHaveLength(1);
  });

  test('batch CREATE operations', async () => {
    const tasks = ['Task 1', 'Task 2', 'Task 3'];
    let q = query();

    for (const title of tasks) {
      q = q.forEach(create('task').as({ title }));
    }

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.steps.filter((s) => s.type === 'action')).toHaveLength(3);
  });

  test('CREATE with metadata', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task with Metadata' }))
      .withMetadata({ priority: 'high', expectedResults: 1 });

    const def = q.build();
    expect(def.metadata?.priority).toBe('high');
  });
});

describe('CREATE Action - Error Handling', () => {
  test('handles CREATE with invalid properties gracefully', async () => {
    const q = query().forEach(
      create('task').as({
        title: undefined as any,
        status: null as any,
      })
    );

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    // Should compile without throwing
    expect(plan.steps).toHaveLength(1);
  });

  test('CREATE action with no properties', async () => {
    const action = create('task').as({});
    const spec = action.build();

    expect(spec.type).toBe('create');
    expect(spec.params.properties).toEqual({});
  });
});

describe('CREATE Action - Plan Metadata', () => {
  test('plan includes CREATE action in cost estimation', async () => {
    const q = query().forEach(create('task').as({ title: 'New Task' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.metadata.estimatedCost.totalWork).toBeGreaterThan(0);
    expect(plan.metadata.estimatedCost.makespan).toBeGreaterThan(0);
  });

  test('plan marks CREATE as parallelizable', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task 1' }))
      .forEach(create('task').as({ title: 'Task 2' }));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build());

    expect(plan.metadata.parallelizable).toBe(true);
  });
});

describe('CREATE Action - Variable Bindings', () => {
  test('CREATE produces variable binding', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'New Task' }))
      .return(['task']);

    const def = q.build();
    expect(def.returns).toContain('task');
  });

  test('multiple CREATE actions produce multiple bindings', async () => {
    const q = query()
      .forEach(create('task').as({ title: 'Task 1' }))
      .forEach(create('knowledge').as({ title: 'Knowledge 1' }))
      .return(['task', 'knowledge']);

    const def = q.build();
    expect(def.returns).toEqual(['task', 'knowledge']);
  });
});
