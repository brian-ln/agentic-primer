#!/usr/bin/env bun
/**
 * DELETE_RELATIONSHIP Action Tests
 * Tests for DELETE_RELATIONSHIP operations with safety checks
 * Target: >20 test cases covering all relationship deletion scenarios
 */

import { test, expect, describe } from 'bun:test';
import {
  query,
  deleteRelationship,
  QueryBuilder,
  ActionBuilder,
} from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import type { QueryDefinition, ActionSpec } from '../types.ts';

describe('DeleteRelationshipActionBuilder - Construction', () => {
  test('creates delete relationship action builder', () => {
    const action = deleteRelationship('task', 'dep', { type: 'requires' });
    expect(action).toBeDefined();
  });

  test('requires explicit confirmation', () => {
    const action = deleteRelationship('task', 'dep', { type: 'requires' }).confirm();
    const spec = action.build();
    expect(spec.type).toBe('delete_relationship');
    expect(spec.params.confirmed).toBe(true);
    expect(spec.params.from).toBe('task');
    expect(spec.params.to).toBe('dep');
    expect(spec.params.type).toBe('requires');
  });

  test('throws error when used without confirmation', () => {
    const action = deleteRelationship('task', 'dep', { type: 'requires' });
    const spec = action.build();

    // Should have confirmed: false
    expect(spec.params.confirmed).toBe(false);
  });
});

describe('DeleteRelationshipActionBuilder - Confirmation Methods', () => {
  test('confirm() sets confirmed flag', () => {
    const action = deleteRelationship('task', 'dep', { type: 'requires' }).confirm();
    const spec = action.build();
    expect(spec.params.confirmed).toBe(true);
    expect(spec.params.deleteAll).toBeUndefined();
  });

  test('confirmAll() enables bulk deletion', () => {
    const action = deleteRelationship('task', undefined, { type: 'assignedTo' }).confirmAll();
    const spec = action.build();
    expect(spec.params.confirmed).toBe(true);
    expect(spec.params.deleteAll).toBe(true);
  });

  test('cascadeOrphans() enables orphan cleanup', () => {
    const action = deleteRelationship('task', 'dep', { type: 'requires' })
      .cascadeOrphans()
      .confirm();
    const spec = action.build();
    expect(spec.params.cascadeOrphans).toBe(true);
    expect(spec.params.confirmed).toBe(true);
  });

  test('sets direction correctly', () => {
    const outbound = deleteRelationship('task', 'dep', {
      type: 'requires',
      direction: 'outbound'
    }).confirm();
    expect(outbound.build().params.direction).toBe('outbound');

    const inbound = deleteRelationship('task', 'dep', {
      type: 'requires',
      direction: 'inbound'
    }).confirm();
    expect(inbound.build().params.direction).toBe('inbound');

    const both = deleteRelationship('task', 'dep', {
      type: 'requires',
      direction: 'both'
    }).confirm();
    expect(both.build().params.direction).toBe('both');
  });
});

describe('DeleteRelationshipActionBuilder - Static Methods', () => {
  test('ActionBuilder.deleteRelationship creates DeleteRelationshipActionBuilder', () => {
    const action = ActionBuilder.deleteRelationship('task', 'dep', { type: 'requires' });
    expect(action).toBeDefined();
  });

  test('deleteRelationship convenience export works', () => {
    const action = deleteRelationship('task', 'dep', { type: 'requires' }).confirm();
    expect(action.build().type).toBe('delete_relationship');
  });
});

describe('QueryBuilder - DELETE_RELATIONSHIP Integration', () => {
  test('adds delete relationship action to query', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', { type: 'requires' }).confirm());

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].type).toBe('delete_relationship');
  });

  test('chains delete relationship with pattern matching', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'cancelled' }),
        pattern('old-dep').label('Task').relatedTo('task', {
          type: 'requires',
          direction: 'inbound'
        })
      )
      .forEach(deleteRelationship('task', 'old-dep', { type: 'requires' }).confirm());

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].params.from).toBe('task');
    expect(def.actions![0].params.to).toBe('old-dep');
  });

  test('builds complete delete relationship query', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-123' }),
        pattern('old-assignee').label('User').relatedTo('task', {
          type: 'assignedTo',
          direction: 'inbound'
        })
      )
      .forEach(deleteRelationship('task', 'old-assignee', { type: 'assignedTo' }).confirm())
      .return(['task']);

    const def = q.build();
    expect(def.patterns).toHaveLength(2);
    expect(def.actions).toHaveLength(1);
    expect(def.returns).toEqual(['task']);
  });
});

describe('QueryCompiler - DELETE_RELATIONSHIP Compilation', () => {
  test('compiles simple delete relationship query', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', { type: 'requires' }).confirm())
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps.length).toBeGreaterThanOrEqual(3); // 2 queries + 1 delete_relationship

    const deleteRelStep = plan.steps.find(s => s.message.type === 'delete_relationship');
    expect(deleteRelStep).toBeDefined();
    expect(deleteRelStep!.type).toBe('action');
    expect(deleteRelStep!.actor).toBe('@(domain/relationships)');
  });

  test('throws error for unconfirmed delete relationship', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', { type: 'requires' }))
      .build();

    await expect(compiler.compile(q)).rejects.toThrow(
      /DELETE_RELATIONSHIP action requires explicit confirmation/
    );
  });

  test('compiles delete relationship with type filter', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', { type: 'requires' }).confirm())
      .build();

    const plan = await compiler.compile(q);
    const deleteRelStep = plan.steps.find(s => s.message.type === 'delete_relationship');
    expect(deleteRelStep!.message.payload.type).toBe('requires');
  });

  test('compiles delete relationship with direction', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', {
        type: 'requires',
        direction: 'inbound'
      }).confirm())
      .build();

    const plan = await compiler.compile(q);
    const deleteRelStep = plan.steps.find(s => s.message.type === 'delete_relationship');
    expect(deleteRelStep!.message.payload.direction).toBe('inbound');
  });

  test('marks delete relationship as non-parallelizable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', { type: 'requires' }).confirm())
      .build();

    const plan = await compiler.compile(q);
    const deleteRelStep = plan.steps.find(s => s.message.type === 'delete_relationship');
    expect(deleteRelStep!.parallelizable).toBe(false);
  });

  test('validates from variable exists', async () => {
    const compiler = new QueryCompiler();
    const q: QueryDefinition = {
      patterns: [],
      filters: [],
      traversals: [],
      aggregations: [],
      actions: [{
        type: 'delete_relationship',
        target: 'nonexistent',
        params: {
          from: 'nonexistent',
          to: 'also-nonexistent',
          type: 'requires',
          confirmed: true
        }
      }],
      returns: [],
      metadata: {},
    };

    await expect(compiler.compile(q)).rejects.toThrow(/unknown from variable/i);
  });

  test('validates to variable exists when specified', async () => {
    const compiler = new QueryCompiler();
    const q: QueryDefinition = {
      patterns: [
        { variable: 'task', labels: ['Task'], where: { id: 'task-1' } }
      ],
      filters: [],
      traversals: [],
      aggregations: [],
      actions: [{
        type: 'delete_relationship',
        target: 'task',
        params: {
          from: 'task',
          to: 'nonexistent',
          type: 'requires',
          confirmed: true
        }
      }],
      returns: [],
      metadata: {},
    };

    await expect(compiler.compile(q)).rejects.toThrow(/unknown to variable/i);
  });
});

describe('DELETE_RELATIONSHIP Safety Checks', () => {
  test('specific relationship deletion requires confirmation', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', { type: 'requires' }).confirm());

    const def = q.build();
    expect(def.actions![0].params.confirmed).toBe(true);
    expect(def.actions![0].params.deleteAll).toBeUndefined();
  });

  test('bulk relationship deletion requires confirmAll', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'cancelled' }))
      .forEach(deleteRelationship('task', undefined, { type: 'assignedTo' }).confirmAll());

    const def = q.build();
    expect(def.actions![0].params.confirmed).toBe(true);
    expect(def.actions![0].params.deleteAll).toBe(true);
  });

  test('cascade orphans includes flag', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(
        deleteRelationship('task', 'dep', { type: 'requires' })
          .cascadeOrphans()
          .confirm()
      );

    const def = q.build();
    expect(def.actions![0].params.cascadeOrphans).toBe(true);
  });
});

describe('DELETE_RELATIONSHIP Query Examples', () => {
  test('delete specific relationship by endpoints', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('old-dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'old-dep', { type: 'requires' }).confirm())
      .build();

    expect(q.patterns).toHaveLength(2);
    expect(q.actions).toHaveLength(1);
    expect(q.actions![0].type).toBe('delete_relationship');
  });

  test('delete all relationships of a type from a node', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-1' }))
      .forEach(
        deleteRelationship('task', undefined, { type: 'assignedTo' }).confirmAll()
      )
      .build();

    expect(q.actions![0].params.to).toBeUndefined();
    expect(q.actions![0].params.deleteAll).toBe(true);
  });

  test('delete all outbound relationships', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'cancelled' }))
      .forEach(
        deleteRelationship('task', undefined, {
          direction: 'outbound'
        }).confirmAll()
      )
      .build();

    expect(q.actions![0].params.direction).toBe('outbound');
    expect(q.actions![0].params.type).toBeUndefined(); // All types
  });

  test('delete inbound relationships', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-1' }))
      .forEach(
        deleteRelationship('task', undefined, {
          type: 'requires',
          direction: 'inbound'
        }).confirmAll()
      )
      .build();

    expect(q.actions![0].params.direction).toBe('inbound');
  });

  test('detach node by deleting all relationships', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-1' }))
      .forEach(
        deleteRelationship('task', undefined, {
          direction: 'both'
        }).confirmAll()
      )
      .build();

    expect(q.actions![0].params.direction).toBe('both');
  });
});

describe('DELETE_RELATIONSHIP Error Handling', () => {
  test('throws on unconfirmed delete during compilation', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .forEach(deleteRelationship('task', 'dep', { type: 'requires' }))
      .build();

    await expect(compiler.compile(q)).rejects.toThrow();
  });

  test('validates from variable exists', async () => {
    const compiler = new QueryCompiler();
    const q: QueryDefinition = {
      patterns: [],
      filters: [],
      traversals: [],
      aggregations: [],
      actions: [{
        type: 'delete_relationship',
        target: 'nonexistent',
        params: {
          from: 'nonexistent',
          to: 'dep',
          type: 'requires',
          confirmed: true
        }
      }],
      returns: [],
      metadata: {},
    };

    await expect(compiler.compile(q)).rejects.toThrow(/unknown from variable/i);
  });
});

describe('DELETE_RELATIONSHIP Complex Scenarios', () => {
  test('delete relationship after pattern matching', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'completed' }),
        pattern('blocker').label('Task')
          .relatedTo('task', { type: 'requires', direction: 'inbound' })
      )
      .forEach(deleteRelationship('task', 'blocker', { type: 'requires' }).confirm())
      .build();

    expect(q.patterns).toHaveLength(2);
    expect(q.actions).toHaveLength(1);
  });

  test('conditional delete relationship with when clause', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('dep').label('Task').where({ id: 'task-2' })
      )
      .when(
        pattern('dep').where({ status: 'completed' })
      )
      .then(deleteRelationship('task', 'dep', { type: 'requires' }).confirm())
      .build();

    expect(q.actions).toHaveLength(1);
    expect(q.actions![0].params.type).toBe('requires');
  });

  test('delete multiple relationship types', () => {
    // For deleting multiple types, you need multiple delete actions
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-1' }))
      .forEach(deleteRelationship('task', undefined, { type: 'assignedTo' }).confirmAll())
      .forEach(deleteRelationship('task', undefined, { type: 'blockedBy' }).confirmAll())
      .build();

    expect(q.actions).toHaveLength(2);
  });

  test('delete relationship with cascade orphans', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'task-1' }),
        pattern('subtask').label('Task').relatedTo('task', {
          type: 'subtask',
          direction: 'outbound'
        })
      )
      .forEach(
        deleteRelationship('task', 'subtask', { type: 'subtask' })
          .cascadeOrphans()
          .confirm()
      )
      .build();

    expect(q.actions![0].params.cascadeOrphans).toBe(true);
  });
});

describe('DELETE_RELATIONSHIP Documentation Examples', () => {
  test('example 1: remove old dependency', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ id: 'build' }),
        pattern('old-dep').label('Task').where({ id: 'deprecated' })
      )
      .forEach(deleteRelationship('task', 'old-dep', { type: 'requires' }).confirm())
      .build();

    expect(q.actions![0].params.from).toBe('task');
    expect(q.actions![0].params.to).toBe('old-dep');
  });

  test('example 2: unassign user from all tasks', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ assignee: 'user-123' }))
      .forEach(
        deleteRelationship('task', undefined, { type: 'assignedTo' }).confirmAll()
      )
      .build();

    expect(q.actions![0].params.deleteAll).toBe(true);
  });

  test('example 3: clean up orphaned dependencies', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'deleted' }))
      .forEach(
        deleteRelationship('task', undefined, { direction: 'both' })
          .cascadeOrphans()
          .confirmAll()
      )
      .build();

    expect(q.actions![0].params.cascadeOrphans).toBe(true);
    expect(q.actions![0].params.direction).toBe('both');
  });
});
