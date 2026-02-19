#!/usr/bin/env bun
/**
 * EmbeddingManager E2E Tests
 *
 * Tests the actual Cloudflare Workers AI provider — HTTP call, response parsing,
 * normalization, and semantic similarity behavior.
 *
 * Required environment variables:
 * - CLOUDFLARE_ACCOUNT_ID
 * - CLOUDFLARE_WORKERS_AI_TOKEN  (Workers AI permission — DIFFERENT from CLOUDFLARE_API_TOKEN)
 *
 * Run: bun test ugs/tests/e2e/embedding.e2e.test.ts
 */

import { test, expect, describe, beforeAll } from 'bun:test';
import { EmbeddingManager } from '../../src/entities/embedding.ts';
import GraphStore from '../../src/graph.ts';

const hasCredentials =
  !!process.env.CLOUDFLARE_ACCOUNT_ID &&
  !!process.env.CLOUDFLARE_WORKERS_AI_TOKEN;

const isE2ERun = !!process.env.RUN_E2E;

// When running the full e2e suite (RUN_E2E=true), missing credentials is a hard failure.
// In the normal test suite, skip gracefully so CI stays green.
const describeE2E = hasCredentials
  ? describe
  : isE2ERun
    ? describe  // will fail in beforeAll with a clear error
    : describe.skip;

describeE2E('EmbeddingManager E2E (requires CLOUDFLARE_WORKERS_AI_TOKEN)', () => {
  beforeAll(() => {
    if (!hasCredentials) {
      throw new Error(
        'Missing credentials: set CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_WORKERS_AI_TOKEN\n' +
        'Note: CLOUDFLARE_WORKERS_AI_TOKEN requires "Workers AI Read" permission,\n' +
        'which is DIFFERENT from CLOUDFLARE_API_TOKEN (AI Gateway unified billing).'
      );
    }
  });

  test('generates real embedding from Cloudflare AI', async () => {
    const store = new GraphStore(`/tmp/ugs-embedding-e2e-${Date.now()}`);
    await store.initialize();
    const manager = new EmbeddingManager(store);

    const embedding = await manager.embed('Hello, this is a test sentence.');

    expect(embedding).toBeDefined();
    expect(embedding.length).toBe(768);
    const norm = Math.sqrt(embedding.reduce((sum, v) => sum + v * v, 0));
    expect(norm).toBeCloseTo(1, 1);
  });

  test('embeds and finds similar nodes with real API', async () => {
    const store = new GraphStore(`/tmp/ugs-embedding-e2e-similar-${Date.now()}`);
    await store.initialize();
    const manager = new EmbeddingManager(store);

    await store.addNode('ml-doc', 'document', {
      title: 'Machine Learning',
      content: 'Machine learning is a subset of artificial intelligence'
    });
    await store.addNode('dl-doc', 'document', {
      title: 'Deep Learning',
      content: 'Deep learning uses neural networks with many layers'
    });
    await store.addNode('cooking-doc', 'document', {
      title: 'Cooking',
      content: 'Recipes for delicious pasta and pizza'
    });

    await manager.embedNode('ml-doc');
    await manager.embedNode('dl-doc');
    await manager.embedNode('cooking-doc');

    const results = await manager.findSimilarToText('artificial intelligence neural networks');

    expect(results.length).toBeGreaterThan(0);
    const mlResult = results.find(r => r.node.id === 'ml-doc');
    const dlResult = results.find(r => r.node.id === 'dl-doc');
    const cookingResult = results.find(r => r.node.id === 'cooking-doc');

    if (mlResult && cookingResult) {
      expect(mlResult.similarity).toBeGreaterThan(cookingResult.similarity);
    }
    if (dlResult && cookingResult) {
      expect(dlResult.similarity).toBeGreaterThan(cookingResult.similarity);
    }
  });
});
