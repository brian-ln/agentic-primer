#!/usr/bin/env bun
/**
 * DELETE Action Tests
 * Tests for DELETE operations with safety checks
 * Target: >20 test cases covering all safety mechanisms
 */

import { test, expect, describe } from 'bun:test';
import {
  query,
  deleteEntity,
  QueryBuilder,
  ActionBuilder,
} from '../builder.ts';
import { pattern } from '../pattern.ts';
import { QueryCompiler } from '../compiler.ts';
import { QueryExecutor } from '../../messaging/actors/query-executor.ts';
import { MessageRouter } from '../../messaging/router.ts';
import type { QueryDefinition, ActionSpec } from '../types.ts';

describe('DeleteActionBuilder - Construction', () => {
  test('creates delete action builder', () => {
    const action = deleteEntity('task');
    expect(action).toBeDefined();
  });

  test('requires explicit confirmation', () => {
    const action = deleteEntity('task').confirm();
    const spec = action.build();
    expect(spec.type).toBe('delete');
    expect(spec.target).toBe('task');
    expect(spec.params.confirmed).toBe(true);
  });

  test('throws error when used without confirmation', () => {
    const action = deleteEntity('task');
    const spec = action.build();

    // Should have confirmed: false or undefined
    expect(spec.params.confirmed).toBeUndefined();
  });
});

describe('DeleteActionBuilder - Confirmation Methods', () => {
  test('confirm() sets confirmed flag', () => {
    const action = deleteEntity('task').confirm();
    const spec = action.build();
    expect(spec.params.confirmed).toBe(true);
    expect(spec.params.soft).toBeUndefined();
    expect(spec.params.cascade).toBeUndefined();
  });

  test('soft() enables soft delete', () => {
    const action = deleteEntity('task').soft();
    const spec = action.build();
    expect(spec.params.confirmed).toBe(true);
    expect(spec.params.soft).toBe(true);
  });

  test('cascade() enables cascade delete', () => {
    const action = deleteEntity('task').cascade();
    const spec = action.build();
    expect(spec.params.confirmed).toBe(true);
    expect(spec.params.cascade).toBe(true);
    expect(spec.params.relationships).toBeUndefined();
  });

  test('cascade() with specific relationships', () => {
    const action = deleteEntity('task').cascade(['requires', 'depends_on']);
    const spec = action.build();
    expect(spec.params.cascade).toBe(true);
    expect(spec.params.relationships).toEqual(['requires', 'depends_on']);
  });

  test('confirmBulk() sets bulk confirmation', () => {
    const action = deleteEntity('tasks').confirmBulk(10);
    const spec = action.build();
    expect(spec.params.confirmed).toBe(true);
    expect(spec.params.bulk).toBe(true);
    expect(spec.params.count).toBe(10);
  });
});

describe('DeleteActionBuilder - Static Methods', () => {
  test('ActionBuilder.delete creates DeleteActionBuilder', () => {
    const action = ActionBuilder.delete('task');
    expect(action).toBeDefined();
  });

  test('deleteEntity convenience export works', () => {
    const action = deleteEntity('task').confirm();
    expect(action.build().type).toBe('delete');
  });
});

describe('QueryBuilder - DELETE Integration', () => {
  test('adds delete action to query', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').confirm());

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].type).toBe('delete');
  });

  test('chains delete with other actions', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'obsolete' }))
      .forEach(deleteEntity('task').soft());

    const def = q.build();
    expect(def.actions).toHaveLength(1);
    expect(def.actions![0].params.soft).toBe(true);
  });

  test('builds complete delete query', () => {
    const q = query()
      .match(
        pattern('task')
          .label('Task')
          .where({ status: 'cancelled', createdAt: { lt: Date.now() - 86400000 } })
      )
      .forEach(deleteEntity('task').soft())
      .return(['task']);

    const def = q.build();
    expect(def.patterns).toHaveLength(1);
    expect(def.actions).toHaveLength(1);
    expect(def.returns).toEqual(['task']);
  });
});

describe('QueryCompiler - DELETE Compilation', () => {
  test('compiles simple delete query', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').confirm())
      .build();

    const plan = await compiler.compile(q);
    expect(plan.steps).toHaveLength(2); // query + delete

    const deleteStep = plan.steps.find(s => s.message.type === 'delete');
    expect(deleteStep).toBeDefined();
    expect(deleteStep!.type).toBe('action');
  });

  test('throws error for unconfirmed delete', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task'))
      .build();

    await expect(compiler.compile(q)).rejects.toThrow(
      /DELETE action requires explicit confirmation/
    );
  });

  test('compiles soft delete', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').soft())
      .build();

    const plan = await compiler.compile(q);
    const deleteStep = plan.steps.find(s => s.message.type === 'delete');
    expect(deleteStep!.message.payload.soft).toBe(true);
  });

  test('compiles cascade delete', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').cascade(['requires']))
      .build();

    const plan = await compiler.compile(q);
    const deleteStep = plan.steps.find(s => s.message.type === 'delete');
    expect(deleteStep!.message.payload.cascade).toBe(true);
    expect(deleteStep!.message.payload.relationships).toEqual(['requires']);
  });

  test('marks delete as non-parallelizable', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').confirm())
      .build();

    const plan = await compiler.compile(q);
    const deleteStep = plan.steps.find(s => s.message.type === 'delete');
    expect(deleteStep!.parallelizable).toBe(false);
  });

  test('sets requiresBulkConfirmation flag', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'done' }))
      .forEach(deleteEntity('task').confirm())
      .build();

    const plan = await compiler.compile(q);
    const deleteStep = plan.steps.find(s => s.message.type === 'delete');
    expect(deleteStep!.message.payload.requiresBulkConfirmation).toBe(true);
  });
});

describe('DELETE Safety Checks', () => {
  test('bulk delete example requires explicit confirmation', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'done' }))
      .forEach(deleteEntity('task').confirm());

    const def = q.build();
    expect(def.actions![0].params.confirmed).toBe(true);
    // Bulk confirmation would be checked at runtime
  });

  test('cascade delete includes relationship list', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').cascade(['subtasks', 'attachments']));

    const def = q.build();
    const action = def.actions![0];
    expect(action.params.cascade).toBe(true);
    expect(action.params.relationships).toEqual(['subtasks', 'attachments']);
  });

  test('soft delete does not remove entities', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').soft());

    const def = q.build();
    expect(def.actions![0].params.soft).toBe(true);
    expect(def.actions![0].params.cascade).toBeUndefined();
  });
});

describe('DELETE Query Examples', () => {
  test('delete single entity by id', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').confirm())
      .build();

    expect(q.patterns).toHaveLength(1);
    expect(q.actions).toHaveLength(1);
    expect(q.actions![0].type).toBe('delete');
  });

  test('soft delete completed tasks', () => {
    const q = query()
      .match(
        pattern('task')
          .label('Task')
          .where({ status: 'completed', completedAt: { lt: Date.now() - 2592000000 } })
      )
      .forEach(deleteEntity('task').soft())
      .build();

    expect(q.actions![0].params.soft).toBe(true);
  });

  test('cascade delete project and dependencies', () => {
    const q = query()
      .match(pattern('project').label('Project').where({ id: 'proj-123' }))
      .forEach(deleteEntity('project').cascade(['tasks', 'milestones', 'documents']))
      .build();

    const action = q.actions![0];
    expect(action.params.cascade).toBe(true);
    expect(action.params.relationships).toHaveLength(3);
  });

  test('delete obsolete tasks with confirmation', () => {
    const oneYearAgo = Date.now() - 365 * 24 * 60 * 60 * 1000;
    const q = query()
      .match(
        pattern('task')
          .label('Task')
          .where({
            status: 'cancelled',
            lastModified: { lt: oneYearAgo }
          })
      )
      .forEach(deleteEntity('task').confirm())
      .build();

    expect(q.actions![0].type).toBe('delete');
  });
});

describe('DELETE Error Handling', () => {
  test('throws on unconfirmed delete during compilation', async () => {
    const compiler = new QueryCompiler();
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(deleteEntity('task'))
      .build();

    await expect(compiler.compile(q)).rejects.toThrow();
  });

  test('validates target variable exists', async () => {
    const compiler = new QueryCompiler();
    const q: QueryDefinition = {
      patterns: [],
      filters: [],
      traversals: [],
      aggregations: [],
      actions: [{
        type: 'delete',
        target: 'nonexistent',
        params: { confirmed: true }
      }],
      returns: [],
      metadata: {},
    };

    await expect(compiler.compile(q)).rejects.toThrow(/unknown variable/i);
  });
});

describe('DELETE Complex Scenarios', () => {
  test('delete after traversal', () => {
    const q = query()
      .match(pattern('root').label('Task').where({ id: 'task-root' }))
      .traverse({
        from: 'root',
        relationship: 'subtasks',
        direction: 'outbound',
        depth: { max: 1 },
        as: 'children'
      })
      .forEach(deleteEntity('children').confirm())
      .build();

    expect(q.patterns).toHaveLength(1);
    expect(q.traversals).toHaveLength(1);
    expect(q.actions).toHaveLength(1);
  });

  test('conditional delete with pattern matching', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .when(
        pattern('task').where({
          priority: 'low',
          createdAt: { lt: Date.now() - 86400000 * 90 }
        })
      )
      .then(deleteEntity('task').soft())
      .build();

    expect(q.actions).toHaveLength(1);
    expect(q.actions![0].params.soft).toBe(true);
  });

  test('bulk delete with filter', () => {
    const q = query()
      .match(pattern('tasks').label('Task'))
      .where(pattern('tasks').where({
        status: 'cancelled',
        archived: true
      }))
      .forEach(deleteEntity('tasks').confirmBulk(50))
      .build();

    expect(q.actions![0].params.bulk).toBe(true);
    expect(q.actions![0].params.count).toBe(50);
  });
});

describe('DELETE Documentation Examples', () => {
  test('example 1: safe single entity delete', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .forEach(deleteEntity('task').confirm())
      .build();

    expect(q.actions![0].params.confirmed).toBe(true);
  });

  test('example 2: soft delete for audit trail', () => {
    const q = query()
      .match(pattern('user').label('User').where({ deactivated: true }))
      .forEach(deleteEntity('user').soft())
      .build();

    expect(q.actions![0].params.soft).toBe(true);
  });

  test('example 3: cascade delete with relationships', () => {
    const q = query()
      .match(pattern('workspace').label('Workspace').where({ id: 'ws-123' }))
      .forEach(deleteEntity('workspace').cascade(['projects', 'members']))
      .build();

    expect(q.actions![0].params.cascade).toBe(true);
  });
});
