#!/usr/bin/env bun
/**
 * Alias Resolution Tests
 *
 * Tests for graph-based alias resolution with multiple paths,
 * context injection, priority resolution, and cycle detection.
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import GraphStore from '@src/graph.ts';
import {
  AliasResolver,
  AliasError,
  type Alias,
  type ResolvedPath,
} from './alias-resolver';

describe('Alias Resolver', () => {
  let graph: GraphStore;
  let resolver: AliasResolver;

  beforeEach(async () => {
    // Use unique data directory per test run to avoid state pollution
    const testId = Date.now() + Math.random();
    graph = new GraphStore(`./data/test-alias-${testId}`);
    await graph.initialize();
    resolver = new AliasResolver(graph);
  });

  describe('createAlias', () => {
    test('creates simple alias', async () => {
      await resolver.createAlias('services/llm', 'domain/inference');

      const resolved = await resolver.resolve('services/llm');
      expect(resolved.path).toBe('domain/inference');
      expect(resolved.wasAlias).toBe(true);
    });

    test('creates alias with priority', async () => {
      await resolver.createAlias('services/llm', 'domain/inference', {
        priority: 5,
      });

      const resolved = await resolver.resolve('services/llm');
      expect(resolved.priority).toBe(5);
    });

    test('creates alias with context', async () => {
      await resolver.createAlias('services/llm', 'domain/inference', {
        context: {
          role: 'llm-service',
          version: 'stable',
        },
      });

      const resolved = await resolver.resolve('services/llm');
      expect(resolved.context).toEqual({
        role: 'llm-service',
        version: 'stable',
      });
    });

    test('creates alias with description', async () => {
      await resolver.createAlias('services/llm', 'domain/inference', {
        description: 'LLM service alias',
      });

      const aliases = await resolver.listAliases();
      expect(aliases[0].description).toBe('LLM service alias');
    });

    test('rejects duplicate alias', async () => {
      await resolver.createAlias('services/llm', 'domain/inference');

      await expect(
        resolver.createAlias('services/llm', 'domain/executor')
      ).rejects.toThrow(AliasError);
    });

    test('rejects invalid alias path', async () => {
      await expect(
        resolver.createAlias('services/../domain', 'domain/inference')
      ).rejects.toThrow(AliasError);
    });

    test('rejects invalid canonical path', async () => {
      await expect(
        resolver.createAlias('services/llm', 'domain/../inference')
      ).rejects.toThrow(AliasError);
    });

    test('detects direct cycles', async () => {
      await resolver.createAlias('a', 'b');

      // Creating b → a would create a cycle
      await expect(
        resolver.createAlias('b', 'a')
      ).rejects.toThrow(AliasError);
    });
  });

  describe('resolve', () => {
    test('resolves direct alias', async () => {
      await resolver.createAlias('services/llm', 'domain/inference');

      const resolved = await resolver.resolve('services/llm');
      expect(resolved).toEqual({
        path: 'domain/inference',
        context: {},
        priority: 0,
        wasAlias: true,
      });
    });

    test('resolves non-alias path (passthrough)', async () => {
      const resolved = await resolver.resolve('domain/inference');
      expect(resolved).toEqual({
        path: 'domain/inference',
        context: {},
        priority: 0,
        wasAlias: false,
      });
    });

    test('resolves chained aliases', async () => {
      await resolver.createAlias('a', 'b');
      await resolver.createAlias('b', 'c');
      await resolver.createAlias('c', 'domain/inference');

      const resolved = await resolver.resolve('a');
      expect(resolved.path).toBe('domain/inference');
      expect(resolved.wasAlias).toBe(true);
    });

    test('merges context from chained aliases', async () => {
      await resolver.createAlias('a', 'b', {
        context: { layer: '1', foo: 'a' },
      });
      await resolver.createAlias('b', 'c', {
        context: { layer: '2', bar: 'b' },
      });
      await resolver.createAlias('c', 'domain/inference', {
        context: { layer: '3', baz: 'c' },
      });

      const resolved = await resolver.resolve('a');
      expect(resolved.context).toEqual({
        layer: '3', // Deeper context overrides
        foo: 'a',
        bar: 'b',
        baz: 'c',
      });
    });

    test('uses highest priority from chain', async () => {
      await resolver.createAlias('a', 'b', { priority: 1 });
      await resolver.createAlias('b', 'c', { priority: 5 });
      await resolver.createAlias('c', 'domain/inference', { priority: 3 });

      const resolved = await resolver.resolve('a');
      expect(resolved.priority).toBe(5); // Highest
    });

    test('detects alias cycles', async () => {
      await resolver.createAlias('a', 'b');
      await resolver.createAlias('b', 'c');

      // Manually create cycle (bypassing cycle detection in createAlias)
      const aliasId = `alias-c`;
      graph.addNode(aliasId, 'alias', {
        alias_path: 'c',
        canonical_path: 'a', // Creates cycle: a → b → c → a
        priority: 0,
        context: {},
      });

      await expect(resolver.resolve('a')).rejects.toThrow(AliasError);
      await expect(resolver.resolve('a')).rejects.toThrow('cycle');
    });

    test('prevents infinite resolution (max depth)', async () => {
      // Create long chain
      for (let i = 0; i < 15; i++) {
        await resolver.createAlias(`alias${i}`, `alias${i + 1}`);
      }

      // Should hit max depth (default: 10)
      await expect(
        resolver.resolve('alias0')
      ).rejects.toThrow('exceeded max depth');
    });

    test('respects custom max depth', async () => {
      await resolver.createAlias('a', 'b');
      await resolver.createAlias('b', 'c');
      await resolver.createAlias('c', 'd');

      // Max depth 2 should fail
      await expect(
        resolver.resolve('a', { maxDepth: 2 })
      ).rejects.toThrow('exceeded max depth');

      // Max depth 5 should succeed
      const resolved = await resolver.resolve('a', { maxDepth: 5 });
      expect(resolved.path).toBe('d');
    });
  });

  describe('deleteAlias', () => {
    test('deletes existing alias', async () => {
      await resolver.createAlias('services/llm', 'domain/inference');

      const deleted = await resolver.deleteAlias('services/llm');
      expect(deleted).toBe(true);

      // Should no longer resolve
      const resolved = await resolver.resolve('services/llm');
      expect(resolved.wasAlias).toBe(false);
      expect(resolved.path).toBe('services/llm');
    });

    test('returns false for non-existent alias', async () => {
      const deleted = await resolver.deleteAlias('nonexistent');
      expect(deleted).toBe(false);
    });

    test('deletes alias from chain', async () => {
      await resolver.createAlias('a', 'b');
      await resolver.createAlias('b', 'c');
      await resolver.createAlias('c', 'd');

      // Delete middle alias
      await resolver.deleteAlias('b');

      // a → b should fail (b deleted)
      const resolved = await resolver.resolve('a');
      expect(resolved.path).toBe('b'); // Resolves to deleted alias path
      expect(resolved.wasAlias).toBe(true);
    });
  });

  describe('listAliases', () => {
    test('lists all aliases', async () => {
      await resolver.createAlias('services/llm', 'domain/inference', {
        priority: 1,
        context: { role: 'llm' },
      });
      await resolver.createAlias('ai/claude', 'domain/inference', {
        priority: 2,
        context: { model: 'claude' },
      });

      const aliases = await resolver.listAliases();
      expect(aliases.length).toBe(2);

      expect(aliases[0].aliasPath).toBe('services/llm');
      expect(aliases[0].canonicalPath).toBe('domain/inference');
      expect(aliases[0].priority).toBe(1);

      expect(aliases[1].aliasPath).toBe('ai/claude');
      expect(aliases[1].canonicalPath).toBe('domain/inference');
      expect(aliases[1].priority).toBe(2);
    });

    test('returns empty array when no aliases', async () => {
      const aliases = await resolver.listAliases();
      expect(aliases).toEqual([]);
    });
  });

  describe('findAliasesFor', () => {
    test('finds aliases pointing to canonical path', async () => {
      await resolver.createAlias('services/llm', 'domain/inference');
      await resolver.createAlias('ai/claude', 'domain/inference');
      await resolver.createAlias('roles/assistant', 'domain/inference');
      await resolver.createAlias('services/executor', 'domain/executor');

      const aliases = await resolver.findAliasesFor('domain/inference');
      expect(aliases.length).toBe(3);

      const aliasPaths = aliases.map(a => a.aliasPath).sort();
      expect(aliasPaths).toEqual([
        'ai/claude',
        'roles/assistant',
        'services/llm',
      ]);
    });

    test('returns empty array for non-existent canonical path', async () => {
      await resolver.createAlias('services/llm', 'domain/inference');

      const aliases = await resolver.findAliasesFor('domain/executor');
      expect(aliases).toEqual([]);
    });
  });

  describe('isAlias', () => {
    test('returns true for alias', async () => {
      await resolver.createAlias('services/llm', 'domain/inference');

      const result = await resolver.isAlias('services/llm');
      expect(result).toBe(true);
    });

    test('returns false for non-alias', async () => {
      const result = await resolver.isAlias('domain/inference');
      expect(result).toBe(false);
    });
  });

  describe('integration scenarios', () => {
    test('service aliases for different environments', async () => {
      // Stable service
      await resolver.createAlias('services/stable/inference', 'domain/inference-v1', {
        context: { environment: 'stable', version: 'v1' },
        priority: 1,
      });

      // Canary service
      await resolver.createAlias('services/canary/inference', 'domain/inference-v2', {
        context: { environment: 'canary', version: 'v2' },
        priority: 2,
      });

      const stable = await resolver.resolve('services/stable/inference');
      expect(stable.path).toBe('domain/inference-v1');
      expect(stable.context.environment).toBe('stable');

      const canary = await resolver.resolve('services/canary/inference');
      expect(canary.path).toBe('domain/inference-v2');
      expect(canary.context.environment).toBe('canary');
    });

    test('role-based addressing', async () => {
      await resolver.createAlias('roles/primary-executor', 'domain/executor-1', {
        context: { role: 'primary' },
      });

      await resolver.createAlias('roles/backup-executor', 'domain/executor-2', {
        context: { role: 'backup' },
      });

      const primary = await resolver.resolve('roles/primary-executor');
      expect(primary.path).toBe('domain/executor-1');

      const backup = await resolver.resolve('roles/backup-executor');
      expect(backup.path).toBe('domain/executor-2');
    });

    test('multi-tenant namespacing with aliases', async () => {
      await resolver.createAlias('tenants/acme/service', 'services/shared-service', {
        context: { tenant: 'acme' },
      });

      await resolver.createAlias('tenants/beta/service', 'services/shared-service', {
        context: { tenant: 'beta' },
      });

      const acme = await resolver.resolve('tenants/acme/service');
      expect(acme.context.tenant).toBe('acme');

      const beta = await resolver.resolve('tenants/beta/service');
      expect(beta.context.tenant).toBe('beta');
    });

    test('multiple aliases with different priorities', async () => {
      // Create multiple aliases to same target with different priorities
      await resolver.createAlias('high-priority', 'target', { priority: 10 });
      await resolver.createAlias('low-priority', 'target', { priority: 1 });

      const high = await resolver.resolve('high-priority');
      expect(high.priority).toBe(10);

      const low = await resolver.resolve('low-priority');
      expect(low.priority).toBe(1);
    });
  });

  describe('performance', () => {
    test('resolves 1000 direct aliases quickly', async () => {
      // Create 1000 aliases
      for (let i = 0; i < 1000; i++) {
        await resolver.createAlias(`alias${i}`, `target${i}`);
      }

      const start = Date.now();
      for (let i = 0; i < 1000; i++) {
        await resolver.resolve(`alias${i}`);
      }
      const elapsed = Date.now() - start;

      // Should resolve 1000 aliases in <100ms (avg <0.1ms per resolve)
      expect(elapsed).toBeLessThan(100);
    });

    test('handles deep alias chains', async () => {
      // Create chain of 10 aliases
      for (let i = 0; i < 10; i++) {
        await resolver.createAlias(`alias${i}`, `alias${i + 1}`);
      }
      await resolver.createAlias('alias10', 'final-target');

      const start = Date.now();
      const resolved = await resolver.resolve('alias0', { maxDepth: 15 }); // Need higher maxDepth for 10-link chain
      const elapsed = Date.now() - start;

      expect(resolved.path).toBe('final-target');
      // Should resolve 10-deep chain in <10ms
      expect(elapsed).toBeLessThan(10);
    });
  });
});
