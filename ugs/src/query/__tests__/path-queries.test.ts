#!/usr/bin/env bun
/**
 * Path-Based Query Tests
 *
 * Comprehensive test suite for path-based actor filtering.
 * Tests query DSL, pattern compilation, and SQL generation.
 *
 * Target: 30+ test scenarios covering all path filter types
 * Part of Phase 6: Query Layer Integration for Path-Based Addressing
 */

import { test, expect, describe } from 'bun:test';
import { query } from '../builder.ts';
import { pattern } from '../pattern.ts';
import {
  pathFilter,
  pathPrefix,
  pathPattern,
  pathExact,
} from '../path-filter.ts';
import {
  compilePathFilter,
  runtimePathMatch,
  generatePathQuery,
} from '../path-pattern.ts';

describe('Path Filter Builders', () => {
  describe('PathFilterBuilder API', () => {
    test('creates prefix filter', () => {
      const filter = pathFilter().prefix('workflows/build/');
      const options = filter.build();
      expect(options.pathPrefix).toBe('workflows/build/');
    });

    test('creates pattern filter', () => {
      const filter = pathFilter().pattern('channels/*');
      const options = filter.build();
      expect(options.pathPattern).toBe('channels/*');
    });

    test('creates exact filter', () => {
      const filter = pathFilter().exact('domain/inference');
      const options = filter.build();
      expect(options.pathExact).toBe('domain/inference');
    });

    test('supports method chaining', () => {
      const filter = pathFilter();
      expect(filter.prefix('test/')).toBe(filter);
    });
  });

  describe('Convenience Functions', () => {
    test('pathPrefix() shorthand', () => {
      const filter = pathPrefix('workflows/build/');
      expect(filter.build().pathPrefix).toBe('workflows/build/');
    });

    test('pathPattern() shorthand', () => {
      const filter = pathPattern('services/**');
      expect(filter.build().pathPattern).toBe('services/**');
    });

    test('pathExact() shorthand', () => {
      const filter = pathExact('domain/inference');
      expect(filter.build().pathExact).toBe('domain/inference');
    });
  });

  describe('Filter Expression Conversion', () => {
    test('exact path → equality filter', () => {
      const filter = pathExact('domain/inference');
      const expr = filter.toFilterExpression('actor');

      expect(expr.type).toBe('comparison');
      expect(expr.operator).toBe('=');
      expect(expr.property).toBe('path');
      expect(expr.value).toBe('domain/inference');
    });

    test('path prefix → STARTS_WITH filter', () => {
      const filter = pathPrefix('workflows/build/');
      const expr = filter.toFilterExpression('actor');

      expect(expr.type).toBe('comparison');
      expect(expr.operator).toBe('STARTS_WITH');
      expect(expr.value).toBe('workflows/build/');
    });

    test('path pattern → PATH_MATCH predicate', () => {
      const filter = pathPattern('channels/*');
      const expr = filter.toFilterExpression('actor');

      expect(expr.type).toBe('predicate');
      expect(expr.operator).toBe('PATH_MATCH');
      expect(expr.value).toBe('channels/*');
    });

    test('throws if no filter criteria', () => {
      const filter = pathFilter();
      expect(() => filter.toFilterExpression('actor')).toThrow();
    });
  });
});

describe('Path Pattern Compilation', () => {
  describe('Exact Path Match', () => {
    test('compiles exact match to equality', () => {
      const result = compilePathFilter('exact', 'domain/inference', 'p1');

      expect(result.sql).toBe('path = :p1_path');
      expect(result.params).toEqual({ p1_path: 'domain/inference' });
      expect(result.indexOptimized).toBe(true);
      expect(result.strategy).toBe('exact');
    });

    test('handles simple paths', () => {
      const result = compilePathFilter('exact', 'root', 'p');
      expect(result.params.p_path).toBe('root');
    });

    test('handles deep hierarchies', () => {
      const result = compilePathFilter('exact', 'a/b/c/d/e/f', 'p');
      expect(result.params.p_path).toBe('a/b/c/d/e/f');
    });
  });

  describe('Prefix Match', () => {
    test('compiles prefix to LIKE with %', () => {
      const result = compilePathFilter('prefix', 'workflows/build/', 'p1');

      expect(result.sql).toBe('path LIKE :p1_path');
      expect(result.params).toEqual({ p1_path: 'workflows/build/%' });
      expect(result.indexOptimized).toBe(true);
      expect(result.strategy).toBe('prefix');
    });

    test('adds trailing slash if missing', () => {
      const result = compilePathFilter('prefix', 'workflows/build', 'p');
      expect(result.params.p_path).toBe('workflows/build/%');
    });

    test('preserves existing trailing slash', () => {
      const result = compilePathFilter('prefix', 'workflows/build/', 'p');
      expect(result.params.p_path).toBe('workflows/build/%');
    });

    test('handles single segment prefix', () => {
      const result = compilePathFilter('prefix', 'workflows', 'p');
      expect(result.params.p_path).toBe('workflows/%');
    });
  });

  describe('Pattern Match - No Wildcards', () => {
    test('treats pattern without wildcards as exact match', () => {
      const result = compilePathFilter('pattern', 'domain/inference', 'p');

      expect(result.sql).toBe('path = :p_path');
      expect(result.params).toEqual({ p_path: 'domain/inference' });
      expect(result.indexOptimized).toBe(true);
      expect(result.strategy).toBe('exact');
    });
  });

  describe('Pattern Match - Single Wildcard (*)', () => {
    test('compiles single wildcard pattern', () => {
      const result = compilePathFilter('pattern', 'channels/*', 'p');

      expect(result.sql).toBe('path LIKE :p_path');
      expect(result.params.p_path).toBe('channels/%');
      expect(result.indexOptimized).toBe(true); // Has literal prefix
    });

    test('handles wildcard in middle', () => {
      const result = compilePathFilter('pattern', 'workflows/*/tasks', 'p');

      expect(result.params.p_path).toBe('workflows/%/tasks');
      expect(result.indexOptimized).toBe(true);
    });

    test('handles multiple single wildcards', () => {
      const result = compilePathFilter('pattern', 'workflows/*/tasks/*', 'p');

      expect(result.params.p_path).toBe('workflows/%/tasks/%');
      expect(result.indexOptimized).toBe(true);
    });

    test('handles leading wildcard (no prefix optimization)', () => {
      const result = compilePathFilter('pattern', '*/tasks', 'p');

      expect(result.params.p_path).toBe('%/tasks');
      expect(result.indexOptimized).toBe(false); // No literal prefix
    });
  });

  describe('Pattern Match - Recursive Wildcard (**)', () => {
    test('compiles trailing ** as prefix match', () => {
      const result = compilePathFilter('pattern', 'services/**', 'p');

      expect(result.sql).toBe('path LIKE :p_path');
      expect(result.params.p_path).toBe('services/%');
      expect(result.indexOptimized).toBe(true);
      expect(result.strategy).toBe('prefix');
    });

    test('handles ** in middle', () => {
      const result = compilePathFilter('pattern', 'services/**/tasks', 'p');

      expect(result.params.p_path).toBe('services/%/tasks');
      expect(result.indexOptimized).toBe(true);
    });

    test('handles leading **', () => {
      const result = compilePathFilter('pattern', '**/tasks', 'p');

      expect(result.params.p_path).toBe('%/tasks');
      expect(result.indexOptimized).toBe(false);
    });

    test('handles only **', () => {
      const result = compilePathFilter('pattern', '**', 'p');

      expect(result.params.p_path).toBe('%');
      expect(result.indexOptimized).toBe(false);
    });
  });

  describe('SQL Parameter Naming', () => {
    test('uses custom parameter prefix', () => {
      const result = compilePathFilter('exact', 'domain', 'custom');
      expect(result.sql).toContain(':custom_path');
      expect(result.params).toHaveProperty('custom_path');
    });

    test('supports multiple filters with different prefixes', () => {
      const r1 = compilePathFilter('exact', 'path1', 'p1');
      const r2 = compilePathFilter('exact', 'path2', 'p2');

      expect(r1.params).toHaveProperty('p1_path');
      expect(r2.params).toHaveProperty('p2_path');
    });
  });
});

describe('Runtime Pattern Matching', () => {
  describe('Exact Matches', () => {
    test('matches exact path', () => {
      expect(runtimePathMatch('domain/inference', 'domain/inference')).toBe(true);
    });

    test('rejects different path', () => {
      expect(runtimePathMatch('domain/other', 'domain/inference')).toBe(false);
    });

    test('rejects partial match', () => {
      expect(runtimePathMatch('domain/inference/task', 'domain/inference')).toBe(false);
    });
  });

  describe('Single Wildcard (*)', () => {
    test('matches one segment', () => {
      expect(runtimePathMatch('channels/logs', 'channels/*')).toBe(true);
      expect(runtimePathMatch('channels/metrics', 'channels/*')).toBe(true);
    });

    test('rejects zero segments', () => {
      expect(runtimePathMatch('channels', 'channels/*')).toBe(false);
    });

    test('rejects multiple segments', () => {
      expect(runtimePathMatch('channels/logs/errors', 'channels/*')).toBe(false);
    });

    test('matches wildcard in middle', () => {
      expect(runtimePathMatch('workflows/build/tasks', 'workflows/*/tasks')).toBe(true);
      expect(runtimePathMatch('workflows/deploy/tasks', 'workflows/*/tasks')).toBe(true);
    });

    test('rejects wrong segment count', () => {
      expect(runtimePathMatch('workflows/tasks', 'workflows/*/tasks')).toBe(false);
      expect(runtimePathMatch('workflows/a/b/tasks', 'workflows/*/tasks')).toBe(false);
    });
  });

  describe('Recursive Wildcard (**)', () => {
    test('matches zero segments', () => {
      // Note: PathResolver's ** can match zero segments if followed by nothing
      // This test documents actual behavior
      expect(runtimePathMatch('services', 'services/**')).toBe(true);
    });

    test('matches one segment', () => {
      expect(runtimePathMatch('services/llm', 'services/**')).toBe(true);
    });

    test('matches multiple segments', () => {
      expect(runtimePathMatch('services/stable/inference', 'services/**')).toBe(true);
      expect(runtimePathMatch('services/a/b/c/d/e', 'services/**')).toBe(true);
    });

    test('matches ** in middle', () => {
      expect(runtimePathMatch('services/llm/tasks', 'services/**/tasks')).toBe(true);
      expect(runtimePathMatch('services/a/b/c/tasks', 'services/**/tasks')).toBe(true);
    });

    test('rejects wrong ending', () => {
      expect(runtimePathMatch('services/llm/other', 'services/**/tasks')).toBe(false);
    });
  });

  describe('Complex Patterns', () => {
    test('combines * and **', () => {
      expect(runtimePathMatch('workflows/build/tasks/compile', 'workflows/*/tasks/**')).toBe(true);
      expect(runtimePathMatch('workflows/deploy/tasks/a/b/c', 'workflows/*/tasks/**')).toBe(true);
    });

    test('multiple wildcards', () => {
      expect(runtimePathMatch('a/b/c/d', '*/*/c/*')).toBe(true);
      expect(runtimePathMatch('a/b/c/d/e', '*/*/c/*')).toBe(false);
    });
  });
});

describe('SQL Query Generation', () => {
  test('generates exact match query', () => {
    const result = generatePathQuery('exact', 'domain/inference', 'actors', 'q1');

    expect(result.sql).toBe('actors.path = :q1_path');
    expect(result.params).toEqual({ q1_path: 'domain/inference' });
    expect(result.requiresRuntimeFilter).toBe(false);
  });

  test('generates prefix match query', () => {
    const result = generatePathQuery('prefix', 'workflows/build/', 'actors', 'q1');

    expect(result.sql).toBe('actors.path LIKE :q1_path');
    expect(result.params).toEqual({ q1_path: 'workflows/build/%' });
    expect(result.requiresRuntimeFilter).toBe(false);
  });

  test('generates simple pattern query', () => {
    const result = generatePathQuery('pattern', 'channels/*', 'actors', 'q1');

    expect(result.sql).toBe('actors.path LIKE :q1_path');
    expect(result.requiresRuntimeFilter).toBe(false);
  });

  test('flags runtime filter for complex patterns', () => {
    const result = generatePathQuery('pattern', '**/tasks', 'actors', 'q1');

    expect(result.requiresRuntimeFilter).toBe(true);
    expect(result.runtimePattern).toBe('**/tasks');
  });

  test('supports custom table name', () => {
    const result = generatePathQuery('exact', 'path', 'custom_table', 'q');
    expect(result.sql).toContain('custom_table.path');
  });
});

describe('Query DSL Integration', () => {
  describe('Path Filters in where() Clause', () => {
    test('path_prefix property', () => {
      const q = query()
        .match(
          pattern('task').label('Task').where({
            path_prefix: 'workflows/build-pipeline/tasks/',
            status: 'active',
          })
        )
        .return(['task']);

      const def = q.build();
      expect(def.patterns[0].where).toHaveProperty('path_prefix');
      expect(def.patterns[0].where.path_prefix).toBe('workflows/build-pipeline/tasks/');
    });

    test('path_pattern with single wildcard', () => {
      const q = query()
        .match(pattern('channel').label('Channel').where({ path_pattern: 'channels/*' }))
        .return(['channel']);

      const def = q.build();
      expect(def.patterns[0].where.path_pattern).toBe('channels/*');
    });

    test('path_pattern with recursive wildcard', () => {
      const q = query()
        .match(pattern('service').label('Service').where({ path_pattern: 'services/**' }))
        .return(['service']);

      const def = q.build();
      expect(def.patterns[0].where.path_pattern).toBe('services/**');
    });

    test('path_exact property', () => {
      const q = query()
        .match(pattern('actor').where({ path_exact: 'domain/inference' }))
        .return(['actor']);

      const def = q.build();
      expect(def.patterns[0].where.path_exact).toBe('domain/inference');
    });

    test('combines path filter with other properties', () => {
      const q = query()
        .match(
          pattern('task').where({
            path_pattern: 'workflows/**',
            status: 'active',
            priority: 'high',
          })
        )
        .return(['task']);

      const def = q.build();
      const where = def.patterns[0].where;
      expect(where.path_pattern).toBe('workflows/**');
      expect(where.status).toBe('active');
      expect(where.priority).toBe('high');
    });
  });

  describe('Multiple Patterns with Path Filters', () => {
    test('different path filters per pattern', () => {
      const q = query()
        .match(
          pattern('task').where({ path_prefix: 'workflows/build/' }),
          pattern('channel').where({ path_pattern: 'channels/*' })
        )
        .return(['task', 'channel']);

      const def = q.build();
      expect(def.patterns[0].where.path_prefix).toBe('workflows/build/');
      expect(def.patterns[1].where.path_pattern).toBe('channels/*');
    });
  });

  describe('Query Compilation with Path Filters', () => {
    test('compiles path_prefix filter', async () => {
      const q = query()
        .match(pattern('task').where({ path_prefix: 'workflows/build/' }))
        .return(['task']);

      const plan = await q.compile();
      expect(plan.steps).toHaveLength(1);

      const step = plan.steps[0];
      expect(step.message.payload.pathFilter).toEqual({
        type: 'prefix',
        value: 'workflows/build/',
      });
    });

    test('compiles path_pattern filter', async () => {
      const q = query()
        .match(pattern('channel').where({ path_pattern: 'channels/*' }))
        .return(['channel']);

      const plan = await q.compile();
      const step = plan.steps[0];
      expect(step.message.payload.pathFilter).toEqual({
        type: 'pattern',
        value: 'channels/*',
      });
    });

    test('compiles path_exact filter', async () => {
      const q = query()
        .match(pattern('actor').where({ path_exact: 'domain/inference' }))
        .return(['actor']);

      const plan = await q.compile();
      const step = plan.steps[0];
      expect(step.message.payload.pathFilter).toEqual({
        type: 'exact',
        value: 'domain/inference',
      });
    });

    test('stores path filter in step metadata', async () => {
      const q = query()
        .match(pattern('task').where({ path_pattern: 'workflows/**' }))
        .return(['task']);

      const plan = await q.compile();
      expect(plan.steps[0].metadata?.pathFilter).toBeDefined();
    });
  });
});

describe('Edge Cases', () => {
  test('empty pattern', () => {
    expect(() => compilePathFilter('pattern', '', 'p')).not.toThrow();
  });

  test('pattern with only slashes', () => {
    const result = compilePathFilter('pattern', '///', 'p');
    // parsePath() filters empty segments, so /// -> empty array -> no pattern
    // compilePattern will see empty segments and return exact match with original value
    expect(result.params.p_path).toBe('///'); // Preserves original
  });

  test('pattern with special SQL characters', () => {
    const result = compilePathFilter('pattern', 'test_%/path', 'p');
    // No wildcards, so treated as exact match (no escaping needed for =)
    expect(result.strategy).toBe('exact');
    expect(result.params.p_path).toBe('test_%/path');
  });

  test('very deep path hierarchy', () => {
    const deepPath = new Array(20).fill('segment').join('/');
    const result = compilePathFilter('exact', deepPath, 'p');
    expect(result.params.p_path).toBe(deepPath);
  });

  test('unicode in paths', () => {
    const result = compilePathFilter('exact', 'emoji/😀/test', 'p');
    expect(result.params.p_path).toBe('emoji/😀/test');
  });
});
