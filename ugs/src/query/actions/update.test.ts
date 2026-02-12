#!/usr/bin/env bun
/**
 * UPDATE Action Tests
 * Comprehensive tests for UPDATE operations in the query/DSL layer
 * Target: >20 test cases covering all UPDATE scenarios
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { query, update, pattern } from '../index.ts';
import { QueryCompiler } from '../compiler.ts';
import type { QueryPlan, ExecutionContext } from '../types.ts';
import { address } from '@agentic-primer/actors';

// Helper to create execution context
function createContext(): ExecutionContext {
  return {
    warmActors: new Set([address('tasks')]),
    computationCache: new Map(),
    resources: {
      maxConcurrency: 10,
      availableMemory: 1024 * 1024 * 1024,
    },
    startTime: Date.now(),
  };
}

describe('UPDATE Action Builder', () => {
  test('creates UPDATE action with single property', () => {
    const action = update('task').set({ status: 'completed' });
    const spec = action.build();

    expect(spec.type).toBe('update');
    expect(spec.target).toBe('task');
    expect(spec.params.properties).toEqual({ status: 'completed' });
  });

  test('creates UPDATE action with multiple properties', () => {
    const action = update('task').set({
      status: 'in_progress',
      priority: 'high',
      assignee: 'alice',
    });
    const spec = action.build();

    expect(spec.params.properties).toEqual({
      status: 'in_progress',
      priority: 'high',
      assignee: 'alice',
    });
  });

  test('creates UPDATE action with nested properties', () => {
    const action = update('task').set({
      metadata: {
        lastModified: Date.now(),
        modifiedBy: 'alice',
      },
    });
    const spec = action.build();

    expect(spec.params.properties.metadata).toBeDefined();
    expect(spec.params.properties.metadata.modifiedBy).toBe('alice');
  });

  test('creates UPDATE action with null values (property removal)', () => {
    const action = update('task').set({
      assignee: null,
      dueDate: null,
    });
    const spec = action.build();

    expect(spec.params.properties.assignee).toBeNull();
    expect(spec.params.properties.dueDate).toBeNull();
  });

  test('creates UPDATE action with timestamp', () => {
    const now = Date.now();
    const action = update('task').set({
      startedAt: now,
    });
    const spec = action.build();

    expect(spec.params.properties.startedAt).toBe(now);
  });
});

describe('UPDATE Query Building', () => {
  test('builds query with single UPDATE action', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    expect(q.patterns).toHaveLength(1);
    expect(q.actions).toHaveLength(1);
    expect(q.actions![0].type).toBe('update');
  });

  test('builds query with multiple UPDATE actions', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'ready' }))
      .forEach(update('task').set({ status: 'in_progress' }))
      .forEach(update('task').set({ startedAt: Date.now() }))
      .build();

    expect(q.actions).toHaveLength(2);
    expect(q.actions![0].type).toBe('update');
    expect(q.actions![1].type).toBe('update');
  });

  test('builds query with conditional UPDATE', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'testing' }))
      .when(pattern('task').where({ testsPassed: true }))
      .then(update('task').set({ status: 'completed' }))
      .build();

    expect(q.patterns.length).toBeGreaterThan(0);
    expect(q.actions).toHaveLength(1);
  });

  test('builds bulk UPDATE query', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open', priority: 'low' }))
      .forEach(update('task').set({ priority: 'medium' }))
      .build();

    expect(q.patterns).toHaveLength(1);
    expect(q.actions).toHaveLength(1);
  });
});

describe('UPDATE Compilation', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('compiles simple UPDATE query', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);

    expect(plan.steps).toHaveLength(2); // Query + Update
    const updateStep = plan.steps.find(s => s.type === 'action');
    expect(updateStep).toBeDefined();
    expect(updateStep!.message.type).toBe('update');
  });

  test('UPDATE step depends on query step', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);

    const queryStep = plan.steps.find(s => s.type === 'query');
    const updateStep = plan.steps.find(s => s.type === 'action');

    expect(updateStep!.dependencies).toContain(queryStep!.id);
  });

  test('UPDATE step has correct message payload', async () => {
    const properties = { status: 'completed', completedAt: Date.now() };
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set(properties))
      .build();

    const plan = await compiler.compile(q);
    const updateStep = plan.steps.find(s => s.type === 'action');

    expect(updateStep!.message.payload).toEqual(properties);
  });

  test('UPDATE step uses tell pattern (fire-and-forget)', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);
    const updateStep = plan.steps.find(s => s.type === 'action');

    expect(updateStep!.message.pattern).toBe('tell');
  });

  test('UPDATE step includes metadata', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);
    const updateStep = plan.steps.find(s => s.type === 'action');

    expect(updateStep!.metadata).toBeDefined();
    expect(updateStep!.metadata!.actionType).toBe('update');
    expect(updateStep!.metadata!.targetVariable).toBe('task');
  });

  test('multiple UPDATE steps have dependencies', async () => {
    const q = query()
      .match(pattern('task1').label('Task').where({ id: 'task-123' }))
      .match(pattern('task2').label('Task').where({ id: 'task-456' }))
      .forEach(update('task1').set({ status: 'completed' }))
      .forEach(update('task2').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);
    const updateSteps = plan.steps.filter(s => s.type === 'action');

    expect(updateSteps).toHaveLength(2);
    // UPDATE actions have dependencies on query results
    expect(updateSteps[0].dependencies.length).toBeGreaterThan(0);
    expect(updateSteps[1].dependencies.length).toBeGreaterThan(0);
  });
});

describe('UPDATE with Filters', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('UPDATE only matching entities', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open', priority: 'high' }))
      .forEach(update('task').set({ status: 'in_progress' }))
      .build();

    const plan = await compiler.compile(q);

    expect(plan.steps).toHaveLength(2);
    const queryStep = plan.steps[0];
    expect(queryStep.message.payload.filter).toEqual({ status: 'open', priority: 'high' });
  });

  test('UPDATE with complex filter conditions', async () => {
    const q = query()
      .match(
        pattern('task')
          .label('Task')
          .where({ assignee: 'alice', status: 'open' })
      )
      .forEach(update('task').set({ priority: 'urgent' }))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps[0].message.payload.filter).toMatchObject({
      assignee: 'alice',
      status: 'open',
    });
  });
});

describe('Partial UPDATE Operations', () => {
  test('UPDATE only changed fields', () => {
    const action = update('task').set({
      status: 'in_progress',
      // Only updating status, not touching other fields
    });
    const spec = action.build();

    expect(Object.keys(spec.params.properties)).toHaveLength(1);
    expect(spec.params.properties.status).toBe('in_progress');
  });

  test('UPDATE preserves unspecified fields', () => {
    // This is semantic - the UPDATE action should only touch specified fields
    const action = update('task').set({
      priority: 'high',
    });
    const spec = action.build();

    // Should not have status, assignee, etc.
    expect(spec.params.properties.status).toBeUndefined();
    expect(spec.params.properties.assignee).toBeUndefined();
    expect(spec.params.properties).toEqual({ priority: 'high' });
  });
});

describe('UPDATE Error Cases', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('UPDATE without target variable throws error', async () => {
    const q = query()
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    await expect(compiler.compile(q)).rejects.toThrow('unknown variable');
  });

  test('UPDATE with wrong variable name throws error', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(update('wrongName').set({ status: 'completed' }))
      .build();

    await expect(compiler.compile(q)).rejects.toThrow('unknown variable');
  });
});

describe('UPDATE with Traversals', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('UPDATE traversal results', async () => {
    const q = query()
      .match(pattern('root').label('Task').where({ id: 'root-task' }))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'dependencies',
      })
      .forEach(update('dependencies').set({ status: 'ready' }))
      .build();

    const plan = await compiler.compile(q);

    expect(plan.steps).toHaveLength(3); // Query + Traverse + Update
    const updateStep = plan.steps.find(s => s.type === 'action');
    expect(updateStep!.metadata!.targetVariable).toBe('dependencies');
  });
});

describe('Bulk UPDATE Operations', () => {
  test('bulk UPDATE maintains property consistency', () => {
    const properties = {
      status: 'archived',
      archivedAt: Date.now(),
      archivedBy: 'system',
    };

    const action = update('task').set(properties);
    const spec = action.build();

    expect(spec.params.properties).toEqual(properties);
  });

  test('bulk UPDATE with timestamp consistency', () => {
    const now = Date.now();
    const action = update('task').set({
      processedAt: now,
      processingStatus: 'complete',
    });
    const spec = action.build();

    expect(spec.params.properties.processedAt).toBe(now);
  });
});

describe('UPDATE Plan Optimization', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('UPDATE step has estimated cost', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);
    const updateStep = plan.steps.find(s => s.type === 'action');

    expect(updateStep!.cost).toBeDefined();
    expect(updateStep!.cost.latencyMs).toBeGreaterThan(0);
  });

  test('UPDATE step has signature for deduplication', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);
    const updateStep = plan.steps.find(s => s.type === 'action');

    expect(updateStep!.signature).toBeDefined();
    expect(typeof updateStep!.signature).toBe('string');
  });

  test('identical UPDATE operations have same signature', async () => {
    const properties = { status: 'completed' };
    const q1 = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set(properties))
      .build();

    const q2 = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(update('task').set(properties))
      .build();

    const plan1 = await compiler.compile(q1);
    const plan2 = await compiler.compile(q2);

    const update1 = plan1.steps.find(s => s.type === 'action');
    const update2 = plan2.steps.find(s => s.type === 'action');

    expect(update1!.signature).toBe(update2!.signature);
  });
});

describe('UPDATE Return Values', () => {
  test('UPDATE query with return clause', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .forEach(update('task').set({ status: 'in_progress' }))
      .return(['task'])
      .build();

    expect(q.returns).toContain('task');
  });

  test('UPDATE does not produce new bindings', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(update('task').set({ status: 'completed' }))
      .build();

    const plan = await compiler.compile(q);
    const updateStep = plan.steps.find(s => s.type === 'action');

    // UPDATE actions don't produce bindings
    expect(updateStep!.bindings).toEqual([]);
  });
});

describe('UPDATE Integration Examples', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('workflow progression UPDATE', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'ready' }))
      .forEach(update('task').set({
        status: 'in_progress',
        startedAt: Date.now(),
      }))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps.length).toBeGreaterThan(0);
  });

  test('priority escalation UPDATE', async () => {
    const q = query()
      .match(
        pattern('task')
          .label('Task')
          .where({ status: 'open', priority: 'normal' })
      )
      .forEach(update('task').set({
        priority: 'high',
        escalatedAt: Date.now(),
      }))
      .build();

    const plan = await compiler.compile(q);
    const updateStep = plan.steps.find(s => s.type === 'action');
    expect(updateStep!.message.payload.priority).toBe('high');
  });

  test('batch status UPDATE', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ category: 'maintenance' }))
      .forEach(update('task').set({
        status: 'deferred',
        deferredUntil: Date.now() + 7 * 24 * 60 * 60 * 1000, // 7 days
      }))
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps.find(s => s.type === 'action')).toBeDefined();
  });
});
