#!/usr/bin/env bun
/**
 * Query DSL Helpers Test Suite
 *
 * Tests for matchPath() and resolveAlias() helper methods in QueryBuilder.
 *
 * Coverage:
 * - matchPath with wildcards: *, **, {a,b}
 * - resolveAlias with valid/invalid aliases
 * - Error cases and edge cases
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { query } from '../builder.ts';
import GraphStore from '../../graph.ts';
import { AliasResolver } from '../../messaging/alias-resolver.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { ProgramManager } from '../../entities/program.ts';

describe('Query DSL Helpers', () => {
  let store: GraphStore;
  let aliasResolver: AliasResolver;
  let router: MessageRouter;
  let programManager: ProgramManager;

  beforeEach(async () => {
    // Use unique data directory for each test run
    const testDir = `./data-test-dsl-helpers-${Date.now()}-${Math.random()}`;
    store = new GraphStore(testDir);
    await store.initialize();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);
    aliasResolver = new AliasResolver(store);
  });

  afterEach(async () => {
    // Clean up the store
    if (store) {
      await store.shutdown();
    }
  });

  describe('matchPath()', () => {
    test('returns QueryDefinition with path pattern metadata', () => {
      const results = query().matchPath('/workflows/*/tasks');

      expect(results).toBeArrayOfSize(1);
      expect(results[0].metadata?.pathPattern).toBe('/workflows/*/tasks');
      expect(results[0].metadata?.isPathQuery).toBe(true);
    });

    test('handles single wildcard pattern', () => {
      const results = query().matchPath('/domain/*');

      expect(results).toBeArrayOfSize(1);
      expect(results[0].metadata?.pathPattern).toBe('/domain/*');
    });

    test('handles recursive wildcard pattern', () => {
      const results = query().matchPath('/domain/**');

      expect(results).toBeArrayOfSize(1);
      expect(results[0].metadata?.pathPattern).toBe('/domain/**');
    });

    test('handles alternatives pattern', () => {
      const results = query().matchPath('/services/{llm,executor}');

      expect(results).toBeArrayOfSize(1);
      expect(results[0].metadata?.pathPattern).toBe('/services/{llm,executor}');
    });

    test('handles complex pattern with multiple wildcards', () => {
      const results = query().matchPath('/workflows/**/tasks/{test,build}');

      expect(results).toBeArrayOfSize(1);
      expect(results[0].metadata?.pathPattern).toBe('/workflows/**/tasks/{test,build}');
    });

    test('preserves existing query metadata', () => {
      const results = query()
        .withMetadata({ custom: 'value' })
        .matchPath('/domain/*');

      expect(results[0].metadata?.custom).toBe('value');
      expect(results[0].metadata?.pathPattern).toBe('/domain/*');
    });

    test('handles empty pattern', () => {
      const results = query().matchPath('');

      expect(results).toBeArrayOfSize(1);
      expect(results[0].metadata?.pathPattern).toBe('');
    });

    test('handles pattern without leading slash', () => {
      const results = query().matchPath('domain/inference');

      expect(results).toBeArrayOfSize(1);
      expect(results[0].metadata?.pathPattern).toBe('domain/inference');
    });
  });

  describe('resolveAlias()', () => {
    test('resolves valid alias to canonical path', async () => {
      // Create alias
      await aliasResolver.createAlias('services/llm', 'domain/inference', {
        priority: 1,
        context: { role: 'llm-service' },
      });

      // Resolve alias
      const resolved = await query().resolveAlias('services/llm', aliasResolver);

      expect(resolved).toBe('domain/inference');
    });

    test('resolves chained aliases', async () => {
      // Create chained aliases
      await aliasResolver.createAlias('services/llm', 'domain/inference');
      await aliasResolver.createAlias('llm', 'services/llm');

      // Resolve from the top
      const resolved = await query().resolveAlias('llm', aliasResolver);

      expect(resolved).toBe('domain/inference');
    });

    test('returns original path for non-alias', async () => {
      // Resolve a path that is not an alias
      const resolved = await query().resolveAlias('domain/inference', aliasResolver);

      expect(resolved).toBe('domain/inference');
    });

    test('throws error if resolver not provided', async () => {
      await expect(
        query().resolveAlias('services/llm', null)
      ).rejects.toThrow('AliasResolver instance required');
    });

    test('throws error if resolver is invalid', async () => {
      await expect(
        query().resolveAlias('services/llm', { invalid: 'object' })
      ).rejects.toThrow('AliasResolver instance required');
    });

    test('throws error on cyclic alias', async () => {
      // Create cyclic aliases manually (bypassing cycle detection in createAlias)
      await store.addNode('alias-a', 'alias', {
        alias_path: 'a',
        canonical_path: 'b',
        priority: 0,
        context: {},
      });
      await store.addNode('alias-b', 'alias', {
        alias_path: 'b',
        canonical_path: 'a',
        priority: 0,
        context: {},
      });

      // Should detect cycle
      await expect(
        query().resolveAlias('a', aliasResolver)
      ).rejects.toThrow('Alias cycle detected');
    });

    test('handles alias with context metadata', async () => {
      await aliasResolver.createAlias('services/llm', 'domain/inference', {
        priority: 5,
        context: { role: 'llm-service', version: 'stable' },
      });

      const resolved = await query().resolveAlias('services/llm', aliasResolver);

      expect(resolved).toBe('domain/inference');
    });

    test('resolves multiple different aliases', async () => {
      await aliasResolver.createAlias('services/llm', 'domain/inference');
      await aliasResolver.createAlias('services/executor', 'domain/executor');

      const resolved1 = await query().resolveAlias('services/llm', aliasResolver);
      const resolved2 = await query().resolveAlias('services/executor', aliasResolver);

      expect(resolved1).toBe('domain/inference');
      expect(resolved2).toBe('domain/executor');
    });
  });

  describe('Integration', () => {
    test('matchPath can be chained with resolveAlias', async () => {
      await aliasResolver.createAlias('services/llm', 'domain/inference');

      const resolved = await query().resolveAlias('services/llm', aliasResolver);
      const queries = query().matchPath(`${resolved}/**`);

      expect(queries).toBeArrayOfSize(1);
      expect(queries[0].metadata?.pathPattern).toBe('domain/inference/**');
    });

    test('helpers can be combined with regular query methods', async () => {
      const resolved = await query().resolveAlias('domain/inference', aliasResolver);
      const queries = query().matchPath(`${resolved}/*`);

      // Build a full query with the path pattern
      const fullQuery = query()
        .withMetadata(queries[0].metadata!)
        .return(['actor'])
        .build();

      expect(fullQuery.metadata?.pathPattern).toBeDefined();
      expect(fullQuery.returns).toContain('actor');
    });
  });

  describe('Performance', () => {
    test('matchPath completes in <5ms', () => {
      const start = performance.now();

      for (let i = 0; i < 1000; i++) {
        query().matchPath('/domain/**');
      }

      const duration = performance.now() - start;
      expect(duration).toBeLessThan(5);
    });

    test('resolveAlias completes in <5ms', async () => {
      await aliasResolver.createAlias('test', 'domain/test');

      const start = performance.now();

      for (let i = 0; i < 100; i++) {
        await query().resolveAlias('test', aliasResolver);
      }

      const duration = performance.now() - start;
      expect(duration / 100).toBeLessThan(5);
    });
  });
});
