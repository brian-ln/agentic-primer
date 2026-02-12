#!/usr/bin/env bun
/**
 * Pattern Matching API Tests
 * Tests for src/query/pattern.ts
 * Target: >90% coverage
 */

import { test, expect, describe } from 'bun:test';
import { pattern, filter, logic, PatternBuilder, FilterBuilder } from './pattern.ts';
import type { PatternSpec, FilterExpression } from './types.ts';

describe('PatternBuilder - Construction', () => {
  test('creates pattern with variable name', () => {
    const p = pattern('task');
    expect(p).toBeInstanceOf(PatternBuilder);
    expect(p.getVariable()).toBe('task');
  });

  test('creates typed pattern', () => {
    interface Task {
      id: string;
      status: string;
    }
    const p = pattern<Task>('task');
    expect(p.getVariable()).toBe('task');
  });
});

describe('PatternBuilder - Labels', () => {
  test('adds single label', () => {
    const p = pattern('task').label('Task');
    const spec = p.build();
    expect(spec.labels).toEqual(['Task']);
  });

  test('adds multiple labels via chaining', () => {
    const p = pattern('node').label('Task').label('Urgent');
    const spec = p.build();
    expect(spec.labels).toEqual(['Task', 'Urgent']);
  });

  test('initializes labels array if needed', () => {
    const p = pattern('task');
    const spec1 = p.build();
    expect(spec1.labels).toEqual([]);

    p.label('Task');
    const spec2 = p.build();
    expect(spec2.labels).toEqual(['Task']);
  });
});

describe('PatternBuilder - Where Constraints', () => {
  test('adds property constraints', () => {
    const p = pattern('task').where({ status: 'open', priority: 'high' });
    const spec = p.build();
    expect(spec.where).toEqual({ status: 'open', priority: 'high' });
  });

  test('merges multiple where clauses', () => {
    const p = pattern('task')
      .where({ status: 'open' })
      .where({ priority: 'high' });
    const spec = p.build();
    expect(spec.where).toEqual({ status: 'open', priority: 'high' });
  });

  test('later where clauses override earlier ones', () => {
    const p = pattern('task')
      .where({ status: 'open' })
      .where({ status: 'closed' });
    const spec = p.build();
    expect(spec.where?.status).toBe('closed');
  });

  test('handles empty where clause', () => {
    const p = pattern('task').where({});
    const spec = p.build();
    expect(spec.where).toEqual({});
  });
});

describe('PatternBuilder - Relationships', () => {
  test('adds outbound relationship', () => {
    const p = pattern('task').relatedTo('user', {
      type: 'assignedTo',
      direction: 'outbound',
    });
    const spec = p.build();
    expect(spec.relationships).toHaveLength(1);
    expect(spec.relationships![0]).toMatchObject({
      target: 'user',
      type: 'assignedTo',
      direction: 'outbound',
    });
  });

  test('adds inbound relationship', () => {
    const p = pattern('blocker').relatedTo('task', {
      type: 'requires',
      direction: 'inbound',
    });
    const spec = p.build();
    expect(spec.relationships![0].direction).toBe('inbound');
  });

  test('adds bidirectional relationship', () => {
    const p = pattern('task').relatedTo('related', {
      type: 'relatedTo',
      direction: 'both',
    });
    const spec = p.build();
    expect(spec.relationships![0].direction).toBe('both');
  });

  test('adds relationship without type', () => {
    const p = pattern('task').relatedTo('any', {
      direction: 'outbound',
    });
    const spec = p.build();
    expect(spec.relationships![0].type).toBeUndefined();
  });

  test('adds relationship with properties', () => {
    const p = pattern('task').relatedTo('user', {
      type: 'assignedTo',
      direction: 'outbound',
      properties: { since: '2024-01-01', active: true },
    });
    const spec = p.build();
    expect(spec.relationships![0].properties).toEqual({
      since: '2024-01-01',
      active: true,
    });
  });

  test('chains multiple relationships', () => {
    const p = pattern('task')
      .relatedTo('user', { type: 'assignedTo', direction: 'outbound' })
      .relatedTo('blocker', { type: 'requires', direction: 'inbound' });
    const spec = p.build();
    expect(spec.relationships).toHaveLength(2);
  });
});

describe('PatternBuilder - NOT EXISTS', () => {
  test('adds single NOT EXISTS pattern', () => {
    const blocker = pattern('blocker')
      .label('Task')
      .where({ status: 'open' });

    const p = pattern('task').notExists(blocker);
    const spec = p.build();

    expect(spec.notExists).toHaveLength(1);
    expect(spec.notExists![0].variable).toBe('blocker');
  });

  test('adds multiple NOT EXISTS patterns', () => {
    const blocker1 = pattern('b1').label('Task');
    const blocker2 = pattern('b2').label('Issue');

    const p = pattern('task').notExists(blocker1, blocker2);
    const spec = p.build();

    expect(spec.notExists).toHaveLength(2);
  });

  test('chains NOT EXISTS calls', () => {
    const blocker1 = pattern('b1').label('Task');
    const blocker2 = pattern('b2').label('Issue');

    const p = pattern('task')
      .notExists(blocker1)
      .notExists(blocker2);
    const spec = p.build();

    expect(spec.notExists).toHaveLength(2);
  });

  test('NOT EXISTS with complex pattern', () => {
    const blocker = pattern('blocker')
      .label('Task')
      .where({ status: 'open', priority: 'high' })
      .relatedTo('task', { type: 'requires', direction: 'inbound' });

    const p = pattern('task').notExists(blocker);
    const spec = p.build();

    expect(spec.notExists![0]).toMatchObject({
      variable: 'blocker',
      labels: ['Task'],
      where: { status: 'open', priority: 'high' },
    });
  });
});

describe('PatternBuilder - Complex Patterns', () => {
  test('builds complete pattern with all features', () => {
    const blocker = pattern('blocker')
      .label('Task')
      .where({ status: 'open' })
      .relatedTo('task', { type: 'requires', direction: 'inbound' });

    const p = pattern('task')
      .label('Task')
      .label('Actionable')
      .where({ status: 'open' })
      .where({ priority: 'high' })
      .relatedTo('user', { type: 'assignedTo', direction: 'outbound' })
      .notExists(blocker);

    const spec = p.build();

    expect(spec.variable).toBe('task');
    expect(spec.labels).toEqual(['Task', 'Actionable']);
    expect(spec.where).toEqual({ status: 'open', priority: 'high' });
    expect(spec.relationships).toHaveLength(1);
    expect(spec.notExists).toHaveLength(1);
  });
});

describe('FilterBuilder - Comparison Operators', () => {
  test('creates equality filter', () => {
    const f = filter('task', 'status').eq('open');
    const expr = f.build();
    expect(expr).toMatchObject({
      type: 'comparison',
      operator: '=',
      variable: 'task',
      property: 'status',
      value: 'open',
    });
  });

  test('creates inequality filter', () => {
    const f = filter('task', 'status').neq('closed');
    const expr = f.build();
    expect(expr.operator).toBe('!=');
  });

  test('creates greater than filter', () => {
    const f = filter('task', 'priority').gt(5);
    const expr = f.build();
    expect(expr).toMatchObject({
      operator: '>',
      value: 5,
    });
  });

  test('creates less than filter', () => {
    const f = filter('task', 'priority').lt(10);
    const expr = f.build();
    expect(expr.operator).toBe('<');
  });

  test('creates greater than or equal filter', () => {
    const f = filter('task', 'score').gte(50);
    const expr = f.build();
    expect(expr.operator).toBe('>=');
  });

  test('creates less than or equal filter', () => {
    const f = filter('task', 'score').lte(100);
    const expr = f.build();
    expect(expr.operator).toBe('<=');
  });

  test('creates contains filter', () => {
    const f = filter('task', 'tags').contains('urgent');
    const expr = f.build();
    expect(expr.operator).toBe('CONTAINS');
  });

  test('creates startsWith filter', () => {
    const f = filter('task', 'title').startsWith('Build');
    const expr = f.build();
    expect(expr.operator).toBe('STARTS_WITH');
  });
});

describe('FilterBuilder - Logical Operators', () => {
  test('creates AND filter', () => {
    const f1 = filter('task', 'status').eq('open');
    const f2 = filter('task', 'priority').eq('high');
    const combined = logic.and(f1, f2);

    const expr = combined.build();
    expect(expr.type).toBe('logical');
    expect(expr.operator).toBe('AND');
    expect(expr.expressions).toHaveLength(2);
  });

  test('creates OR filter', () => {
    const f1 = filter('task', 'status').eq('open');
    const f2 = filter('task', 'status').eq('ready');
    const combined = logic.or(f1, f2);

    const expr = combined.build();
    expect(expr.operator).toBe('OR');
  });

  test('creates NOT filter', () => {
    const f = filter('task', 'status').eq('closed');
    const negated = logic.not(f);

    const expr = negated.build();
    expect(expr.operator).toBe('NOT');
    expect(expr.expressions).toHaveLength(1);
  });

  test('creates nested logical filters', () => {
    const f1 = filter('task', 'status').eq('open');
    const f2 = filter('task', 'priority').eq('high');
    const f3 = filter('task', 'tags').contains('urgent');

    const combined = logic.and(
      logic.or(f1, f2),
      f3
    );

    const expr = combined.build();
    expect(expr.type).toBe('logical');
    expect(expr.operator).toBe('AND');
    expect(expr.expressions).toHaveLength(2);
    expect(expr.expressions![0].operator).toBe('OR');
  });

  test('creates complex filter expression', () => {
    const isOpen = filter('task', 'status').eq('open');
    const isHigh = filter('task', 'priority').eq('high');
    const isMedium = filter('task', 'priority').eq('medium');
    const hasUrgent = filter('task', 'tags').contains('urgent');

    const combined = logic.and(
      isOpen,
      logic.or(isHigh, logic.and(isMedium, hasUrgent))
    );

    const expr = combined.build();
    expect(expr.expressions).toHaveLength(2);
  });
});

describe('FilterBuilder - Static Methods', () => {
  test('FilterBuilder.comparison creates ComparisonBuilder', () => {
    const cb = FilterBuilder.comparison('task', 'status');
    const f = cb.eq('open');
    expect(f).toBeInstanceOf(FilterBuilder);
  });

  test('FilterBuilder.and combines filters', () => {
    const f1 = filter('x', 'a').eq(1);
    const f2 = filter('x', 'b').eq(2);
    const combined = FilterBuilder.and(f1, f2);
    expect(combined.build().operator).toBe('AND');
  });

  test('FilterBuilder.or combines filters', () => {
    const f1 = filter('x', 'a').eq(1);
    const f2 = filter('x', 'b').eq(2);
    const combined = FilterBuilder.or(f1, f2);
    expect(combined.build().operator).toBe('OR');
  });

  test('FilterBuilder.not negates filter', () => {
    const f = filter('x', 'a').eq(1);
    const negated = FilterBuilder.not(f);
    expect(negated.build().operator).toBe('NOT');
  });
});

describe('Convenience Exports', () => {
  test('pattern function creates PatternBuilder', () => {
    const p = pattern('test');
    expect(p).toBeInstanceOf(PatternBuilder);
  });

  test('filter function creates ComparisonBuilder', () => {
    const f = filter('task', 'status');
    const result = f.eq('open');
    expect(result).toBeInstanceOf(FilterBuilder);
  });

  test('logic object provides logical operators', () => {
    expect(typeof logic.and).toBe('function');
    expect(typeof logic.or).toBe('function');
    expect(typeof logic.not).toBe('function');
  });
});

describe('Edge Cases', () => {
  test('builds pattern with no constraints', () => {
    const p = pattern('task');
    const spec = p.build();
    expect(spec.variable).toBe('task');
    expect(spec.labels).toEqual([]);
    expect(spec.where).toEqual({});
    expect(spec.relationships).toEqual([]);
    expect(spec.notExists).toEqual([]);
  });

  test('handles special characters in variable names', () => {
    const p = pattern('task_123');
    expect(p.getVariable()).toBe('task_123');
  });

  test('handles null values in where clause', () => {
    const p = pattern('task').where({ deletedAt: null });
    const spec = p.build();
    expect(spec.where?.deletedAt).toBeNull();
  });

  test('handles complex nested objects in where clause', () => {
    const p = pattern('task').where({
      metadata: { author: 'alice', version: 2 },
    });
    const spec = p.build();
    expect(spec.where?.metadata).toEqual({ author: 'alice', version: 2 });
  });
});
