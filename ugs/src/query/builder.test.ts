#!/usr/bin/env bun
/**
 * Query Builder Tests
 * Tests for src/query/builder.ts
 * Target: >90% coverage
 */

import { test, expect, describe } from 'bun:test';
import {
  query,
  send,
  update,
  create,
  QueryBuilder,
  ActionBuilder,
} from './builder.ts';
import { pattern, filter } from './pattern.ts';
import type { QueryDefinition } from './types.ts';

describe('QueryBuilder - Construction', () => {
  test('creates empty query builder', () => {
    const q = query();
    expect(q).toBeInstanceOf(QueryBuilder);
  });

  test('initializes with empty definition', () => {
    const q = query();
    const def = q.build();
    expect(def.patterns).toEqual([]);
    expect(def.filters).toEqual([]);
    expect(def.traversals).toEqual([]);
    expect(def.aggregations).toEqual([]);
    expect(def.actions).toEqual([]);
    expect(def.returns).toEqual([]);
  });
});

describe('QueryBuilder - Match Patterns', () => {
  test('adds single pattern', () => {
    const q = query().match(pattern('task').label('Task'));
    const def = q.build();
    expect(def.patterns).toHaveLength(1);
    expect(def.patterns[0].variable).toBe('task');
  });

  test('adds multiple patterns at once', () => {
    const q = query().match(
      pattern('task').label('Task'),
      pattern('user').label('User')
    );
    const def = q.build();
    expect(def.patterns).toHaveLength(2);
  });

  test('chains match calls', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .match(pattern('user').label('User'));
    const def = q.build();
    expect(def.patterns).toHaveLength(2);
  });

  test('preserves pattern details', () => {
    const q = query().match(
      pattern('task')
        .label('Task')
        .where({ status: 'open', priority: 'high' })
    );
    const def = q.build();
    expect(def.patterns[0].labels).toEqual(['Task']);
    expect(def.patterns[0].where).toEqual({ status: 'open', priority: 'high' });
  });
});

describe('QueryBuilder - Where Filters', () => {
  test('adds single filter', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('open'));
    const def = q.build();
    expect(def.filters).toHaveLength(1);
  });

  test('adds multiple filters at once', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .where(
        filter('task', 'status').eq('open'),
        filter('task', 'priority').eq('high')
      );
    const def = q.build();
    expect(def.filters).toHaveLength(2);
  });

  test('chains where calls', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .where(filter('task', 'status').eq('open'))
      .where(filter('task', 'priority').eq('high'));
    const def = q.build();
    expect(def.filters).toHaveLength(2);
  });
});

describe('QueryBuilder - Traversals', () => {
  test('adds traversal', () => {
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'dependencies',
      });
    const def = q.build();
    expect(def.traversals).toHaveLength(1);
    expect(def.traversals![0].from).toBe('root');
    expect(def.traversals![0].as).toBe('dependencies');
  });

  test('adds traversal with depth constraints', () => {
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        depth: { min: 1, max: 5 },
        as: 'dependencies',
      });
    const def = q.build();
    expect(def.traversals![0].depth).toEqual({ min: 1, max: 5 });
  });

  test('chains multiple traversals', () => {
    const q = query()
      .match(pattern('root').label('Task'))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        as: 'forward',
      })
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'inbound',
        as: 'backward',
      });
    const def = q.build();
    expect(def.traversals).toHaveLength(2);
  });
});

describe('QueryBuilder - Aggregations', () => {
  test('adds count aggregation', () => {
    const q = query()
      .match(pattern('tasks').label('Task'))
      .aggregate({
        operation: 'count',
        variable: 'tasks',
        as: 'taskCount',
      });
    const def = q.build();
    expect(def.aggregations).toHaveLength(1);
    expect(def.aggregations![0].operation).toBe('count');
  });

  test('adds collect aggregation', () => {
    const q = query()
      .match(pattern('tasks').label('Task'))
      .aggregate({
        operation: 'collect',
        variable: 'tasks',
        as: 'allTasks',
      });
    const def = q.build();
    expect(def.aggregations![0].operation).toBe('collect');
  });

  test('adds group aggregation', () => {
    const q = query()
      .match(pattern('tasks').label('Task'))
      .aggregate({
        operation: 'group',
        variable: 'tasks',
        by: 'status',
        as: 'groupedByStatus',
      });
    const def = q.build();
    expect(def.aggregations![0].by).toBe('status');
  });

  test('chains multiple aggregations', () => {
    const q = query()
      .match(pattern('tasks').label('Task'))
      .aggregate({
        operation: 'count',
        variable: 'tasks',
        as: 'total',
      })
      .aggregate({
        operation: 'collect',
        variable: 'tasks',
        as: 'all',
      });
    const def = q.build();
    expect(def.aggregations).toHaveLength(2);
  });
});

describe('QueryBuilder - Return', () => {
  test('specifies single return variable', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .return(['task']);
    const def = q.build();
    expect(def.returns).toEqual(['task']);
  });

  test('specifies multiple return variables', () => {
    const q = query()
      .match(
        pattern('task').label('Task'),
        pattern('user').label('User')
      )
      .return(['task', 'user']);
    const def = q.build();
    expect(def.returns).toEqual(['task', 'user']);
  });

  test('chains return calls', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .return(['task'])
      .return(['user']);
    const def = q.build();
    expect(def.returns).toEqual(['task', 'user']);
  });
});

describe('QueryBuilder - Actions', () => {
  test('adds forEach action', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(send('task').tell('start'));
    const def = q.build();
    expect(def.actions).toHaveLength(1);
  });

  test('chains multiple actions', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .forEach(send('task').tell('start'))
      .forEach(update('task').set({ status: 'in_progress' }));
    const def = q.build();
    expect(def.actions).toHaveLength(2);
  });
});

describe('QueryBuilder - Conditional Actions (WHEN/THEN)', () => {
  test('adds conditional action', () => {
    const q = query()
      .match(pattern('test').label('Task'))
      .when(
        pattern('test').where({ lifecycle: 'completed', result: { passed: true } })
      )
      .then(send('deploy').tell('start'));
    const def = q.build();
    expect(def.patterns).toHaveLength(2); // Original + when pattern
    expect(def.actions).toHaveLength(1);
  });

  test('conditional returns QueryBuilder for chaining', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .when(pattern('task').where({ status: 'ready' }))
      .then(send('task').tell('start'))
      .return(['task']);
    expect(q).toBeInstanceOf(QueryBuilder);
  });
});

describe('QueryBuilder - Metadata', () => {
  test('adds metadata', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .withMetadata({ priority: 'high', expectedResults: 10 });
    const def = q.build();
    expect(def.metadata?.priority).toBe('high');
    expect(def.metadata?.expectedResults).toBe(10);
  });

  test('merges metadata calls', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .withMetadata({ priority: 'high' })
      .withMetadata({ expectedResults: 10 });
    const def = q.build();
    expect(def.metadata?.priority).toBe('high');
    expect(def.metadata?.expectedResults).toBe(10);
  });
});

describe('ActionBuilder - Send Actions', () => {
  test('creates tell action', () => {
    const action = send('task').tell('start');
    const spec = action.build();
    expect(spec.type).toBe('send');
    expect(spec.target).toBe('task');
    expect(spec.params.pattern).toBe('tell');
    expect(spec.params.type).toBe('start');
  });

  test('creates tell action with payload', () => {
    const action = send('task').tell('update', { priority: 'high' });
    const spec = action.build();
    expect(spec.params.payload).toEqual({ priority: 'high' });
  });

  test('creates ask action', () => {
    const action = send('task').ask('get', { id: '123' });
    const spec = action.build();
    expect(spec.params.pattern).toBe('ask');
  });

  test('creates custom message action', () => {
    const action = send('task').message({
      type: 'custom',
      payload: { data: 'test' },
      pattern: 'ask',
    });
    const spec = action.build();
    expect(spec.params.type).toBe('custom');
    expect(spec.params.pattern).toBe('ask');
  });

  test('defaults message pattern to tell', () => {
    const action = send('task').message({ type: 'custom' });
    const spec = action.build();
    expect(spec.params.pattern).toBe('tell');
  });
});

describe('ActionBuilder - Update Actions', () => {
  test('creates update action', () => {
    const action = update('task').set({ status: 'in_progress' });
    const spec = action.build();
    expect(spec.type).toBe('update');
    expect(spec.target).toBe('task');
    expect(spec.params.properties).toEqual({ status: 'in_progress' });
  });

  test('updates multiple properties', () => {
    const action = update('task').set({
      status: 'in_progress',
      priority: 'high',
      startedAt: Date.now(),
    });
    const spec = action.build();
    expect(Object.keys(spec.params.properties)).toHaveLength(3);
  });
});

describe('ActionBuilder - Create Actions', () => {
  test('creates create action', () => {
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
});

describe('ActionBuilder - Static Methods', () => {
  test('ActionBuilder.send creates SendActionBuilder', () => {
    const action = ActionBuilder.send('task');
    expect(action).toBeDefined();
  });

  test('ActionBuilder.update creates UpdateActionBuilder', () => {
    const action = ActionBuilder.update('task');
    expect(action).toBeDefined();
  });

  test('ActionBuilder.create creates CreateActionBuilder', () => {
    const action = ActionBuilder.create('task');
    expect(action).toBeDefined();
  });
});

describe('Convenience Exports', () => {
  test('query function creates QueryBuilder', () => {
    const q = query();
    expect(q).toBeInstanceOf(QueryBuilder);
  });

  test('send function creates action', () => {
    const action = send('task').tell('start');
    expect(action.build().type).toBe('send');
  });

  test('update function creates action', () => {
    const action = update('task').set({ status: 'done' });
    expect(action.build().type).toBe('update');
  });

  test('create function creates action', () => {
    const action = create('task').as({ title: 'Test' });
    expect(action.build().type).toBe('create');
  });
});

describe('Complex Query Examples', () => {
  test('builds complete query with all features', () => {
    const q = query()
      .match(
        pattern('task')
          .label('Task')
          .where({ status: 'open' })
      )
      .where(filter('task', 'priority').gte('medium'))
      .traverse({
        from: 'task',
        relationship: 'requires',
        direction: 'outbound',
        as: 'dependencies',
      })
      .aggregate({
        operation: 'count',
        variable: 'dependencies',
        as: 'depCount',
      })
      .forEach(send('task').tell('start'))
      .return(['task', 'depCount'])
      .withMetadata({ priority: 'high' });

    const def = q.build();
    expect(def.patterns).toHaveLength(1);
    expect(def.filters).toHaveLength(1);
    expect(def.traversals).toHaveLength(1);
    expect(def.aggregations).toHaveLength(1);
    expect(def.actions).toHaveLength(1);
    expect(def.returns).toHaveLength(2);
    expect(def.metadata?.priority).toBe('high');
  });

  test('builds ready task detection query', () => {
    const blocker = pattern('blocker')
      .label('Task')
      .where({ status: 'open' })
      .relatedTo('task', { type: 'requires', direction: 'inbound' });

    const q = query()
      .match(
        pattern('task')
          .label('Task')
          .where({ status: 'open' })
          .notExists(blocker)
      )
      .forEach(send('task').tell('start'));

    const def = q.build();
    expect(def.patterns).toHaveLength(1);
    expect(def.patterns[0].notExists).toHaveLength(1);
    expect(def.actions).toHaveLength(1);
  });
});

describe('Edge Cases', () => {
  test('builds query with no patterns', () => {
    const q = query().return(['result']);
    const def = q.build();
    expect(def.patterns).toHaveLength(0);
  });

  test('builds query with empty arrays', () => {
    const q = query();
    const def = q.build();
    expect(Array.isArray(def.patterns)).toBe(true);
    expect(Array.isArray(def.filters)).toBe(true);
  });

  test('handles null metadata gracefully', () => {
    const q = query().withMetadata({});
    const def = q.build();
    expect(def.metadata).toBeDefined();
  });
});

describe('QueryBuilder - Index Hints', () => {
  test('adds manual index hint', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .useIndex('task', 'status')
      .build();

    expect(q.metadata).toBeDefined();
    expect(q.metadata?.indexHints).toBeDefined();
    expect(q.metadata?.indexHints).toHaveLength(1);
    expect(q.metadata?.indexHints?.[0]).toEqual({
      variable: 'task',
      index: 'status',
      source: 'manual',
      confidence: 1.0,
      reason: 'Manual index hint',
    });
  });

  test('adds multiple index hints', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open', priority: 'high' }))
      .useIndex('task', 'status')
      .useIndex('task', 'priority', 'Custom reason')
      .build();

    expect(q.metadata?.indexHints).toHaveLength(2);
    expect(q.metadata?.indexHints?.[0].index).toBe('status');
    expect(q.metadata?.indexHints?.[1].index).toBe('priority');
    expect(q.metadata?.indexHints?.[1].reason).toBe('Custom reason');
  });

  test('index hints are chainable', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .useIndex('task', 'status')
      .useIndex('task', 'priority')
      .return(['task']);

    expect(q).toBeInstanceOf(QueryBuilder);
    const def = q.build();
    expect(def.metadata?.indexHints).toHaveLength(2);
    expect(def.returns).toContain('task');
  });

  test('manual hints have confidence 1.0', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .useIndex('task', 'id')
      .build();

    const hint = q.metadata?.indexHints?.[0];
    expect(hint?.confidence).toBe(1.0);
    expect(hint?.source).toBe('manual');
  });

  test('custom reason is preserved', () => {
    const customReason = 'Optimize for user query performance';
    const q = query()
      .match(pattern('user').label('User'))
      .useIndex('user', 'email', customReason)
      .build();

    expect(q.metadata?.indexHints?.[0].reason).toBe(customReason);
  });

  test('index hints work with complex queries', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({ status: 'open' }),
        pattern('user').label('User').where({ active: true })
      )
      .useIndex('task', 'status')
      .useIndex('user', 'active')
      .return(['task', 'user'])
      .build();

    expect(q.metadata?.indexHints).toHaveLength(2);
    expect(q.metadata?.indexHints?.some(h => h.variable === 'task')).toBe(true);
    expect(q.metadata?.indexHints?.some(h => h.variable === 'user')).toBe(true);
  });

  test('index hints initialize metadata if needed', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .useIndex('task', 'status')
      .build();

    expect(q.metadata).toBeDefined();
    expect(q.metadata?.indexHints).toBeDefined();
  });
});
