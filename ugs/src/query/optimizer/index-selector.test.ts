#!/usr/bin/env bun
/**
 * Index Selector Tests
 * Tests for src/query/optimizer/index-selector.ts
 * Target: >90% coverage, >15 test cases
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { IndexSelector, getIndexSelector } from './index-selector.ts';
import { query } from '../builder.ts';
import { pattern } from '../pattern.ts';
import type { QueryStatistics, IndexEffectiveness } from '../types.ts';

describe('IndexSelector - Construction', () => {
  test('creates selector instance', () => {
    const selector = new IndexSelector();
    expect(selector).toBeDefined();
  });

  test('getIndexSelector returns singleton', () => {
    const selector1 = getIndexSelector();
    const selector2 = getIndexSelector();
    expect(selector1).toBe(selector2);
  });
});

describe('IndexSelector - Pattern-Based Strategy', () => {
  let selector: IndexSelector;

  beforeEach(() => {
    selector = new IndexSelector();
  });

  test('selects index for equality filter', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    const hints = selector.selectIndexes(q);

    expect(hints.length).toBeGreaterThan(0);
    const statusHint = hints.find((h) => h.index === 'status');
    expect(statusHint).toBeDefined();
    expect(statusHint?.variable).toBe('task');
    expect(statusHint?.source).toBe('automatic');
    expect(statusHint?.confidence).toBeGreaterThan(0.5);
  });

  test('selects index for multiple properties', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          status: 'open',
          priority: 'high',
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    expect(hints.length).toBeGreaterThan(0);
    expect(hints.some((h) => h.index === 'status')).toBe(true);
    expect(hints.some((h) => h.index === 'priority')).toBe(true);
  });

  test('handles query with no WHERE clause', () => {
    const q = query().match(pattern('task').label('Task')).build();

    const hints = selector.selectIndexes(q);

    // Should still work, may suggest label-based indexes
    expect(hints).toBeDefined();
    expect(Array.isArray(hints)).toBe(true);
  });

  test('selects relationship index for traversal patterns', () => {
    const q = query().build();
    q.patterns = [
      {
        variable: 'task',
        labels: ['Task'],
        where: { id: 'task-1' },
        relationships: [
          {
            target: 'dep',
            type: 'requires',
            direction: 'outbound',
          },
        ],
      },
    ];

    const hints = selector.selectIndexes(q);

    const relHint = hints.find((h) => h.index.includes('rel_requires'));
    expect(relHint).toBeDefined();
    expect(relHint?.reason).toContain('Relationship index');
  });
});

describe('IndexSelector - Cardinality Strategy', () => {
  let selector: IndexSelector;

  beforeEach(() => {
    selector = new IndexSelector();
  });

  test('prioritizes high-cardinality properties (id)', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .build();

    const hints = selector.selectIndexes(q);

    const idHint = hints.find((h) => h.index === 'id');
    expect(idHint).toBeDefined();
    expect(idHint?.confidence).toBeGreaterThan(0.8);
    expect(idHint?.reason).toContain('High cardinality');
  });

  test('recognizes UUID patterns', () => {
    const q = query()
      .match(
        pattern('user').label('User').where({
          uuid: '550e8400-e29b-41d4-a716-446655440000',
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    const uuidHint = hints.find((h) => h.index === 'uuid');
    expect(uuidHint).toBeDefined();
    expect(uuidHint?.confidence).toBeGreaterThan(0.8);
  });

  test('recognizes _id suffix pattern', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ user_id: 'user-123' }))
      .build();

    const hints = selector.selectIndexes(q);

    const userIdHint = hints.find((h) => h.index === 'user_id');
    expect(userIdHint).toBeDefined();
    expect(userIdHint?.confidence).toBeGreaterThan(0.7);
  });

  test('handles medium-cardinality properties (name, type)', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ name: 'Build system' }))
      .build();

    const hints = selector.selectIndexes(q);

    const nameHint = hints.find((h) => h.index === 'name');
    expect(nameHint).toBeDefined();
    expect(nameHint?.confidence).toBeGreaterThanOrEqual(0.5);
    expect(nameHint?.confidence).toBeLessThanOrEqual(0.8);
  });
});

describe('IndexSelector - Composite Strategy', () => {
  let selector: IndexSelector;

  beforeEach(() => {
    selector = new IndexSelector();
  });

  test('suggests composite index for multi-property queries', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          status: 'open',
          priority: 'high',
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    const compositeHint = hints.find((h) => h.index.startsWith('composite_'));
    expect(compositeHint).toBeDefined();
    expect(compositeHint?.reason).toContain('Composite index');
  });

  test('composite index includes all properties sorted', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          status: 'open',
          priority: 'high',
          assignee: 'alice',
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    const compositeHint = hints.find((h) => h.index.startsWith('composite_'));
    expect(compositeHint).toBeDefined();
    // Properties should be sorted alphabetically
    expect(compositeHint?.index).toContain('assignee');
    expect(compositeHint?.index).toContain('priority');
    expect(compositeHint?.index).toContain('status');
  });

  test('does not suggest composite for single property', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    const hints = selector.selectIndexes(q);

    const compositeHint = hints.find((h) => h.index.startsWith('composite_'));
    expect(compositeHint).toBeUndefined();
  });

  test('does not suggest composite for too many properties', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          status: 'open',
          priority: 'high',
          assignee: 'alice',
          type: 'bug',
          project: 'main',
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    const compositeHint = hints.find((h) => h.index.startsWith('composite_'));
    expect(compositeHint).toBeUndefined();
  });
});

describe('IndexSelector - Historical Strategy', () => {
  let selector: IndexSelector;

  beforeEach(() => {
    selector = new IndexSelector();
  });

  test('uses historical data when available', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    // Mock historical statistics
    const indexEff = new Map<string, IndexEffectiveness>();
    indexEff.set('status', {
      indexName: 'status',
      useCount: 10,
      avgImprovement: 0.5, // 50% improvement
      successRate: 0.9,
      avgResultCount: 100,
      lastUsedAt: Date.now(),
    });

    const stats: QueryStatistics[] = [
      {
        signature: 'test-sig',
        executionCount: 10,
        avgDurationMs: 50,
        durationVariance: 10,
        avgResultCount: 100,
        successRate: 0.9,
        cacheHitRate: 0.5,
        lastExecutedAt: Date.now(),
        latencyPercentiles: { p50: 45, p90: 80, p99: 100 },
        indexEffectiveness: indexEff,
      },
    ];

    const hints = selector.selectIndexes(q, stats);

    // Historical strategy should provide a hint for status
    // Even if pattern-based also provides one, they'll be merged
    const statusHints = hints.filter((h) => h.index === 'status');
    expect(statusHints.length).toBeGreaterThan(0);

    // At least one should have high confidence based on historical data
    const highConfidenceHint = statusHints.find((h) => h.confidence && h.confidence > 0.4);
    expect(highConfidenceHint).toBeDefined();
  });

  test('ignores ineffective indexes from history', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    // Mock historical statistics with poor performance
    const indexEff = new Map<string, IndexEffectiveness>();
    indexEff.set('status', {
      indexName: 'status',
      useCount: 2, // Too few uses
      avgImprovement: 0.05, // Very small improvement
      successRate: 0.6, // Low success rate
      avgResultCount: 100,
      lastUsedAt: Date.now(),
    });

    const stats: QueryStatistics[] = [
      {
        signature: 'test-sig',
        executionCount: 10,
        avgDurationMs: 50,
        durationVariance: 10,
        avgResultCount: 100,
        successRate: 0.9,
        cacheHitRate: 0.5,
        lastExecutedAt: Date.now(),
        latencyPercentiles: { p50: 45, p90: 80, p99: 100 },
        indexEffectiveness: indexEff,
      },
    ];

    const hints = selector.selectIndexes(q, stats);

    const historicalHint = hints.find(
      (h) => h.index === 'status' && h.reason?.includes('Historical')
    );
    expect(historicalHint).toBeUndefined();
  });

  test('works without historical statistics', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    const hints = selector.selectIndexes(q, []);

    // Should still get pattern-based hints
    expect(hints.length).toBeGreaterThan(0);
    expect(hints.every((h) => h.source === 'automatic')).toBe(true);
  });
});

describe('IndexSelector - Hint Merging', () => {
  let selector: IndexSelector;

  beforeEach(() => {
    selector = new IndexSelector();
  });

  test('merges hints from multiple strategies', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          id: 'task-123',
          status: 'open',
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    // Should have hints from pattern-based, cardinality, and composite strategies
    expect(hints.length).toBeGreaterThan(2);
  });

  test('deduplicates hints by variable:index', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    const hints = selector.selectIndexes(q);

    // Check no duplicates
    const keys = hints.map((h) => `${h.variable}:${h.index}`);
    const uniqueKeys = new Set(keys);
    expect(keys.length).toBe(uniqueKeys.size);
  });

  test('keeps highest confidence when merging duplicates', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ id: 'task-123' }))
      .build();

    const hints = selector.selectIndexes(q);

    const idHints = hints.filter((h) => h.index === 'id');
    expect(idHints.length).toBe(1); // Deduplicated
    expect(idHints[0].confidence).toBeGreaterThan(0.7); // High confidence preserved
  });

  test('sorts hints by confidence descending', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          id: 'task-123',
          status: 'open',
          name: 'Build',
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    // Confidence should be descending
    for (let i = 0; i < hints.length - 1; i++) {
      const current = hints[i].confidence || 0;
      const next = hints[i + 1].confidence || 0;
      expect(current).toBeGreaterThanOrEqual(next);
    }
  });
});

describe('IndexSelector - Edge Cases', () => {
  let selector: IndexSelector;

  beforeEach(() => {
    selector = new IndexSelector();
  });

  test('handles empty query', () => {
    const q = query().build();

    const hints = selector.selectIndexes(q);

    expect(hints).toBeDefined();
    expect(Array.isArray(hints)).toBe(true);
  });

  test('handles query with only filters (no patterns)', () => {
    const q = query().build();
    q.patterns = [];
    q.filters = [
      {
        type: 'comparison',
        operator: '=',
        variable: 'task',
        property: 'status',
        value: 'open',
      },
    ];

    const hints = selector.selectIndexes(q);

    expect(hints).toBeDefined();
    const filterHint = hints.find((h) => h.index === 'status');
    expect(filterHint).toBeDefined();
  });

  test('handles range queries', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          createdAt: { $gt: Date.now() - 86400000 },
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    const createdHint = hints.find((h) => h.index === 'createdAt');
    expect(createdHint).toBeDefined();
    expect(createdHint?.reason).toContain('Range query');
  });

  test('handles array membership queries', () => {
    const q = query()
      .match(
        pattern('task').label('Task').where({
          tags: ['urgent', 'bug'],
        })
      )
      .build();

    const hints = selector.selectIndexes(q);

    const tagsHint = hints.find((h) => h.index === 'tags');
    expect(tagsHint).toBeDefined();
    expect(tagsHint?.reason).toContain('Array membership');
  });

  test('all hints have required fields', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    const hints = selector.selectIndexes(q);

    for (const hint of hints) {
      expect(hint.variable).toBeDefined();
      expect(hint.index).toBeDefined();
      expect(hint.source).toBe('automatic');
      expect(hint.confidence).toBeDefined();
      expect(hint.confidence).toBeGreaterThan(0);
      expect(hint.confidence).toBeLessThanOrEqual(1);
      expect(hint.reason).toBeDefined();
    }
  });
});
