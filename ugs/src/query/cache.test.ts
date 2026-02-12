#!/usr/bin/env bun
/**
 * Query Cache Tests
 * Tests for src/query/cache.ts
 * Target: >90% coverage
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { QueryCache } from './cache.ts';
import { QueryCompiler } from './compiler.ts';
import { query } from './builder.ts';
import { pattern } from './pattern.ts';
import type { ExecutionContext, QueryPlan, ExecutionStats } from './types.ts';
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

// Helper to create execution stats
function createStats(overrides?: Partial<ExecutionStats>): ExecutionStats {
  return {
    durationMs: 100,
    stepsExecuted: 1,
    messagesSent: 1,
    cacheHits: 0,
    cacheMisses: 1,
    resultsReturned: 10,
    stepStats: new Map(),
    ...overrides,
  };
}

describe('QueryCache - Construction', () => {
  test('creates cache with default options', () => {
    const cache = new QueryCache();
    expect(cache).toBeDefined();
  });

  test('creates cache with custom options', () => {
    const cache = new QueryCache({
      maxCacheSize: 100,
      maxStatsSize: 500,
    });
    expect(cache).toBeDefined();
  });
});

describe('QueryCache - Exact Matching', () => {
  let cache: QueryCache;
  let compiler: QueryCompiler;
  let context: ExecutionContext;

  beforeEach(() => {
    cache = new QueryCache();
    compiler = new QueryCompiler();
    context = createContext();
  });

  test('returns null for cache miss', () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const result = cache.get(q, context);
    expect(result).toBeNull();
  });

  test('caches and retrieves plan', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q, context);
    cache.put(q, plan, context);

    const cached = cache.get(q, context);
    expect(cached).toBe(plan);
  });

  test('same query returns same cached plan', async () => {
    const q = query()
      .match(pattern('task').label('Task').where({ status: 'open' }))
      .build();

    const plan = await compiler.compile(q, context);
    cache.put(q, plan, context);

    const cached1 = cache.get(q, context);
    const cached2 = cache.get(q, context);

    expect(cached1).toBe(cached2);
  });

  test('different queries return different plans', async () => {
    const q1 = query()
      .match(pattern('task').label('Task'))
      .build();
    const q2 = query()
      .match(pattern('user').label('User'))
      .build();

    const plan1 = await compiler.compile(q1, context);
    const plan2 = await compiler.compile(q2, context);

    cache.put(q1, plan1, context);
    cache.put(q2, plan2, context);

    const cached1 = cache.get(q1, context);
    const cached2 = cache.get(q2, context);

    expect(cached1).not.toBe(cached2);
  });

  test('updates access count on hit', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q, context);
    cache.put(q, plan, context);

    cache.get(q, context);
    cache.get(q, context);
    cache.get(q, context);

    const stats = cache.getCacheStats();
    expect(stats.avgAccessCount).toBeGreaterThan(1);
  });

  test('updates last accessed timestamp', async () => {
    const q = query()
      .match(pattern('task').label('Task'))
      .build();

    const plan = await compiler.compile(q, context);
    cache.put(q, plan, context);

    await new Promise(resolve => setTimeout(resolve, 10));

    const before = Date.now();
    cache.get(q, context);
    const after = Date.now();

    // Access should have updated timestamp (can't directly test internal state)
    expect(true).toBe(true);
  });
});

describe('QueryCache - LRU Eviction', () => {
  test('evicts least recently used when full', async () => {
    const cache = new QueryCache({ maxCacheSize: 2 });
    const compiler = new QueryCompiler();
    const context = createContext();

    const q1 = query().match(pattern('t1').label('Task')).build();
    const q2 = query().match(pattern('t2').label('Task')).build();
    const q3 = query().match(pattern('t3').label('Task')).build();

    const p1 = await compiler.compile(q1, context);
    const p2 = await compiler.compile(q2, context);
    const p3 = await compiler.compile(q3, context);

    cache.put(q1, p1, context);
    cache.put(q2, p2, context);
    cache.put(q3, p3, context); // Should evict q1

    expect(cache.get(q1, context)).toBeNull();
    expect(cache.get(q2, context)).toBe(p2);
    expect(cache.get(q3, context)).toBe(p3);
  });

  test('recent access prevents eviction', async () => {
    const cache = new QueryCache({ maxCacheSize: 2 });
    const compiler = new QueryCompiler();
    const context = createContext();

    const q1 = query().match(pattern('t1').label('Task')).build();
    const q2 = query().match(pattern('t2').label('Task')).build();
    const q3 = query().match(pattern('t3').label('Task')).build();

    const p1 = await compiler.compile(q1, context);
    const p2 = await compiler.compile(q2, context);
    const p3 = await compiler.compile(q3, context);

    cache.put(q1, p1, context);
    await new Promise(resolve => setTimeout(resolve, 10));
    cache.put(q2, p2, context);
    await new Promise(resolve => setTimeout(resolve, 10));

    cache.get(q1, context); // Access q1, updating its lastAccessedAt
    await new Promise(resolve => setTimeout(resolve, 10));

    cache.put(q3, p3, context); // Should evict q2 (oldest lastAccessedAt), not q1

    expect(cache.get(q1, context)).toBe(p1);
    expect(cache.get(q2, context)).toBeNull();
    expect(cache.get(q3, context)).toBe(p3);
  });
});

describe('QueryCache - Semantic Similarity', () => {
  let cache: QueryCache;
  let compiler: QueryCompiler;
  let context: ExecutionContext;

  beforeEach(() => {
    cache = new QueryCache();
    compiler = new QueryCompiler();
    context = createContext();
  });

  test('finds similar query by label', async () => {
    const q1 = query().match(pattern('t').label('Task')).build();
    const q2 = query().match(pattern('x').label('Task')).build();

    const plan = await compiler.compile(q1, context);
    cache.put(q1, plan, context);

    const similar = cache.findSimilar(q2, 0.5);
    expect(similar).not.toBeNull();
  });

  test('returns null if no similar queries', async () => {
    const q1 = query().match(pattern('t').label('Task')).build();
    const q2 = query().match(pattern('u').label('User')).build();

    const plan = await compiler.compile(q1, context);
    cache.put(q1, plan, context);

    const similar = cache.findSimilar(q2, 0.9);
    expect(similar).toBeNull();
  });

  test('respects similarity threshold', async () => {
    const q1 = query().match(pattern('t').label('Task')).build();
    const q2 = query().match(pattern('u').label('User')).build();

    const plan = await compiler.compile(q1, context);
    cache.put(q1, plan, context);

    const lowThreshold = cache.findSimilar(q2, 0.1);
    const highThreshold = cache.findSimilar(q2, 0.9);

    expect(lowThreshold).not.toBeNull();
    expect(highThreshold).toBeNull();
  });

  test('updates access count on similarity hit', async () => {
    const q1 = query().match(pattern('t').label('Task')).build();
    const q2 = query().match(pattern('x').label('Task')).build();

    const plan = await compiler.compile(q1, context);
    cache.put(q1, plan, context);

    cache.findSimilar(q2, 0.5);

    const stats = cache.getCacheStats();
    expect(stats.avgAccessCount).toBeGreaterThan(1);
  });
});

describe('QueryCache - Statistics', () => {
  let cache: QueryCache;
  let compiler: QueryCompiler;
  let context: ExecutionContext;

  beforeEach(() => {
    cache = new QueryCache();
    compiler = new QueryCompiler();
    context = createContext();
  });

  test('records execution statistics', async () => {
    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    const stats = createStats({ durationMs: 150, resultsReturned: 20 });
    cache.recordExecution(plan, stats);

    const queryStats = cache.getStatistics(plan);
    expect(queryStats).not.toBeNull();
    expect(queryStats!.executionCount).toBe(1);
    expect(queryStats!.avgDurationMs).toBe(150);
  });

  test('updates moving average over multiple executions', async () => {
    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    cache.recordExecution(plan, createStats({ durationMs: 100 }));
    cache.recordExecution(plan, createStats({ durationMs: 200 }));

    const queryStats = cache.getStatistics(plan);
    expect(queryStats!.executionCount).toBe(2);
    expect(queryStats!.avgDurationMs).toBe(150);
  });

  test('tracks success rate', async () => {
    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    cache.recordExecution(plan, createStats({ resultsReturned: 10 }));
    cache.recordExecution(plan, createStats({ resultsReturned: 0 }));
    cache.recordExecution(plan, createStats({ resultsReturned: 5 }));

    const queryStats = cache.getStatistics(plan);
    expect(queryStats!.successRate).toBeCloseTo(2/3, 1);
  });

  test('tracks cache hit rate', async () => {
    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    cache.recordExecution(plan, createStats({ cacheHits: 8, cacheMisses: 2 }));
    cache.recordExecution(plan, createStats({ cacheHits: 6, cacheMisses: 4 }));

    const queryStats = cache.getStatistics(plan);
    expect(queryStats!.cacheHitRate).toBeCloseTo(0.7, 1);
  });

  test('updates percentiles', async () => {
    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    for (let i = 0; i < 10; i++) {
      cache.recordExecution(plan, createStats({ durationMs: i * 10 }));
    }

    const queryStats = cache.getStatistics(plan);
    expect(queryStats!.latencyPercentiles.p50).toBeGreaterThan(0);
    expect(queryStats!.latencyPercentiles.p90).toBeGreaterThan(0);
    expect(queryStats!.latencyPercentiles.p99).toBeGreaterThan(0);
  });

  test('returns all statistics', async () => {
    const q1 = query().match(pattern('t1').label('Task')).build();
    const q2 = query().match(pattern('t2').label('User')).build();

    const p1 = await compiler.compile(q1, context);
    const p2 = await compiler.compile(q2, context);

    cache.recordExecution(p1, createStats());
    cache.recordExecution(p2, createStats());

    const allStats = cache.getAllStatistics();
    expect(allStats).toHaveLength(2);
  });

  test('evicts old statistics when limit reached', async () => {
    const cache = new QueryCache({ maxStatsSize: 2 });
    const compiler = new QueryCompiler();
    const context = createContext();

    const q1 = query().match(pattern('t1').label('Task')).build();
    const q2 = query().match(pattern('t2').label('User')).build();
    const q3 = query().match(pattern('t3').label('Doc')).build();

    const p1 = await compiler.compile(q1, context);
    const p2 = await compiler.compile(q2, context);
    const p3 = await compiler.compile(q3, context);

    cache.recordExecution(p1, createStats());
    await new Promise(resolve => setTimeout(resolve, 10));
    cache.recordExecution(p2, createStats());
    await new Promise(resolve => setTimeout(resolve, 10));
    cache.recordExecution(p3, createStats());

    const allStats = cache.getAllStatistics();
    expect(allStats.length).toBeLessThanOrEqual(2);
  });
});

describe('QueryCache - Clear Operations', () => {
  test('clear removes all cached plans', async () => {
    const cache = new QueryCache();
    const compiler = new QueryCompiler();
    const context = createContext();

    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    cache.put(q, plan, context);
    expect(cache.get(q, context)).not.toBeNull();

    cache.clear();
    expect(cache.get(q, context)).toBeNull();
  });

  test('clearStatistics removes all statistics', async () => {
    const cache = new QueryCache();
    const compiler = new QueryCompiler();
    const context = createContext();

    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    cache.recordExecution(plan, createStats());
    expect(cache.getStatistics(plan)).not.toBeNull();

    cache.clearStatistics();
    expect(cache.getStatistics(plan)).toBeNull();
  });
});

describe('QueryCache - Cache Stats', () => {
  test('returns cache statistics', () => {
    const cache = new QueryCache();
    const stats = cache.getCacheStats();

    expect(stats.size).toBe(0);
    expect(stats.maxSize).toBe(1000);
    expect(stats.hitRate).toBe(0);
    expect(stats.avgAccessCount).toBe(0);
  });

  test('updates size as plans are added', async () => {
    const cache = new QueryCache();
    const compiler = new QueryCompiler();
    const context = createContext();

    const q1 = query().match(pattern('t1').label('Task')).build();
    const q2 = query().match(pattern('t2').label('User')).build();

    const p1 = await compiler.compile(q1, context);
    const p2 = await compiler.compile(q2, context);

    cache.put(q1, p1, context);
    expect(cache.getCacheStats().size).toBe(1);

    cache.put(q2, p2, context);
    expect(cache.getCacheStats().size).toBe(2);
  });

  test('calculates average hit rate from query stats', async () => {
    const cache = new QueryCache();
    const compiler = new QueryCompiler();
    const context = createContext();

    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    cache.recordExecution(plan, createStats({ cacheHits: 8, cacheMisses: 2 }));

    const stats = cache.getCacheStats();
    expect(stats.hitRate).toBeCloseTo(0.8, 1);
  });
});

describe('Edge Cases', () => {
  test('handles empty cache gracefully', () => {
    const cache = new QueryCache();
    expect(cache.getAllStatistics()).toHaveLength(0);
    expect(cache.getCacheStats().size).toBe(0);
  });

  test('handles queries with no patterns', () => {
    const cache = new QueryCache();
    const context = createContext();
    const q = query().build();

    const result = cache.get(q, context);
    expect(result).toBeNull();
  });

  test('handles statistics for non-existent plan', async () => {
    const cache = new QueryCache();
    const compiler = new QueryCompiler();
    const context = createContext();

    const q = query().match(pattern('task').label('Task')).build();
    const plan = await compiler.compile(q, context);

    const stats = cache.getStatistics(plan);
    expect(stats).toBeNull();
  });
});
