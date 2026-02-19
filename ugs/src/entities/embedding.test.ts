import { test, expect, describe, beforeEach, mock } from 'bun:test';
import { EmbeddingManager } from './embedding.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

// Mock embedding function for unit tests
// Returns a deterministic embedding based on the text (for testing)
function mockEmbed(text: string): number[] {
  // Simple hash-based deterministic embedding for testing
  const embedding = new Array(768).fill(0);
  let hash = 0;
  for (let i = 0; i < text.length; i++) {
    hash = ((hash << 5) - hash) + text.charCodeAt(i);
    hash = hash & hash;
  }

  // Fill embedding with deterministic values
  for (let i = 0; i < 768; i++) {
    embedding[i] = Math.sin(hash + i) * 0.5;
  }

  // Normalize
  const norm = Math.sqrt(embedding.reduce((sum, v) => sum + v * v, 0));
  return embedding.map(v => v / norm);
}

describe('EmbeddingManager', () => {
  let store: GraphStore;
  let manager: EmbeddingManager;

  beforeEach(async () => {
    testCounter++;
    store = new GraphStore(`/tmp/ugs-embedding-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    manager = new EmbeddingManager(store);

    // Mock the embed method to avoid actual API calls
    manager.embed = mock(async (text: string) => mockEmbed(text));
  });

  describe('cosineSimilarity', () => {
    test('returns 1 for identical vectors', () => {
      const a = [1, 0, 0];
      const b = [1, 0, 0];
      expect(manager.cosineSimilarity(a, b)).toBeCloseTo(1, 5);
    });

    test('returns 0 for orthogonal vectors', () => {
      const a = [1, 0, 0];
      const b = [0, 1, 0];
      expect(manager.cosineSimilarity(a, b)).toBeCloseTo(0, 5);
    });

    test('returns -1 for opposite vectors', () => {
      const a = [1, 0, 0];
      const b = [-1, 0, 0];
      expect(manager.cosineSimilarity(a, b)).toBeCloseTo(-1, 5);
    });

    test('handles normalized vectors correctly', () => {
      const a = [0.6, 0.8, 0];
      const b = [0.8, 0.6, 0];
      const similarity = manager.cosineSimilarity(a, b);
      expect(similarity).toBeGreaterThan(0);
      expect(similarity).toBeLessThan(1);
    });

    test('throws error for vectors of different lengths', () => {
      const a = [1, 0, 0];
      const b = [1, 0];
      expect(() => manager.cosineSimilarity(a, b)).toThrow('Vector length mismatch');
    });

    test('returns 0 for zero vectors', () => {
      const a = [0, 0, 0];
      const b = [1, 0, 0];
      expect(manager.cosineSimilarity(a, b)).toBe(0);
    });
  });

  describe('embedNode', () => {
    test('embeds a node with text content', async () => {
      await store.addNode('test-node', 'document', {
        title: 'Test Document',
        content: 'This is test content'
      });

      const embedding = await manager.embedNode('test-node');

      expect(embedding).toBeDefined();
      expect(embedding.length).toBe(768);
    });

    test('stores embedding in node properties', async () => {
      await store.addNode('store-test', 'document', {
        content: 'Test content'
      });

      await manager.embedNode('store-test');

      const node = store.get('store-test');
      expect(node?.properties.get('embedding')).toBeDefined();
      expect(node?.properties.get('embeddingModel')).toBe('@cf/baai/bge-base-en-v1.5');
      expect(node?.properties.get('embeddingTimestamp')).toBeDefined();
    });

    test('stores embedding as base64 with dimensions', async () => {
      await store.addNode('base64-test', 'document', {
        content: 'Test content for base64'
      });

      await manager.embedNode('base64-test');

      const node = store.get('base64-test');
      const embeddingStr = node?.properties.get('embedding');
      const dimensions = node?.properties.get('embeddingDimensions');

      // Should store dimensions
      expect(dimensions).toBe(768);

      // Should be base64, not JSON (base64 doesn't start with '[')
      expect(embeddingStr).toBeDefined();
      expect(embeddingStr.startsWith('[')).toBe(false);

      // Base64 for 768 floats should be ~4KB, JSON would be ~15KB
      const jsonSize = JSON.stringify(mockEmbed('test')).length;
      expect(embeddingStr.length).toBeLessThan(jsonSize);

      // Should decode correctly
      const retrieved = manager.getNodeEmbedding('base64-test');
      expect(retrieved).toBeDefined();
      expect(retrieved!.length).toBe(768);

      // Values should be close to original (float32 precision)
      const original = mockEmbed('base64-test document Test content for base64');
      for (let i = 0; i < 10; i++) {
        expect(retrieved![i]).toBeCloseTo(original[i], 5);
      }
    });

    test('throws error for non-existent node', async () => {
      await expect(manager.embedNode('nonexistent'))
        .rejects.toThrow('Node not found: nonexistent');
    });

    test('uses custom text when provided', async () => {
      await store.addNode('custom-text', 'document', {
        content: 'Original content'
      });

      // Track what text was embedded
      let embeddedText = '';
      manager.embed = mock(async (text: string) => {
        embeddedText = text;
        return mockEmbed(text);
      });

      await manager.embedNode('custom-text', { text: 'Custom embedding text' });

      expect(embeddedText).toBe('Custom embedding text');
    });

    test('embeds node with just an ID (ID counts as text)', async () => {
      await store.addNode('minimal-node', null, {});

      // Node ID is included in text, so embedding should work
      const embedding = await manager.embedNode('minimal-node');
      expect(embedding.length).toBe(768);
    });

    test('emits NODE_EMBEDDED event', async () => {
      await store.addNode('event-test', 'document', {
        content: 'Test content'
      });

      await manager.embedNode('event-test');

      const events = manager.getEmbeddingEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const embedEvent = events.find(e => e.type === 'NODE_EMBEDDED' && e.data.nodeId === 'event-test');
      expect(embedEvent).toBeDefined();
      expect(embedEvent!.data.model).toBe('@cf/baai/bge-base-en-v1.5');
    });
  });

  describe('getNodeEmbedding', () => {
    test('returns null for non-existent node', () => {
      const embedding = manager.getNodeEmbedding('nonexistent');
      expect(embedding).toBeNull();
    });

    test('returns null for node without embedding', async () => {
      await store.addNode('no-embedding', 'document', {});
      const embedding = manager.getNodeEmbedding('no-embedding');
      expect(embedding).toBeNull();
    });

    test('returns stored embedding', async () => {
      await store.addNode('has-embedding', 'document', {
        content: 'Test'
      });
      await manager.embedNode('has-embedding');

      const embedding = manager.getNodeEmbedding('has-embedding');
      expect(embedding).toBeDefined();
      expect(embedding!.length).toBe(768);
    });
  });

  describe('hasEmbedding', () => {
    test('returns false for node without embedding', async () => {
      await store.addNode('no-emb', 'document', {});
      expect(manager.hasEmbedding('no-emb')).toBe(false);
    });

    test('returns true for node with embedding', async () => {
      await store.addNode('has-emb', 'document', { content: 'Test' });
      await manager.embedNode('has-emb');
      expect(manager.hasEmbedding('has-emb')).toBe(true);
    });
  });

  describe('findSimilar', () => {
    test('finds similar nodes by vector', async () => {
      // Create some nodes with embeddings
      await store.addNode('doc1', 'document', { content: 'Apple is a fruit' });
      await store.addNode('doc2', 'document', { content: 'Banana is a fruit' });
      await store.addNode('doc3', 'document', { content: 'Car is a vehicle' });

      await manager.embedNode('doc1');
      await manager.embedNode('doc2');
      await manager.embedNode('doc3');

      const queryVector = mockEmbed('Fruit salad');
      const results = manager.findSimilar(queryVector);

      expect(results.length).toBeGreaterThan(0);
      expect(results[0].node).toBeDefined();
      expect(typeof results[0].similarity).toBe('number');
    });

    test('respects limit option', async () => {
      // Create multiple nodes
      for (let i = 0; i < 20; i++) {
        await store.addNode(`node-${i}`, 'document', { content: `Document ${i}` });
        await manager.embedNode(`node-${i}`);
      }

      const queryVector = mockEmbed('test query');
      const results = manager.findSimilar(queryVector, { limit: 5 });

      expect(results.length).toBe(5);
    });

    test('filters by node type', async () => {
      await store.addNode('article1', 'article', { content: 'News article' });
      await store.addNode('blog1', 'blog', { content: 'Blog post' });
      await store.addNode('article2', 'article', { content: 'Another article' });

      await manager.embedNode('article1');
      await manager.embedNode('blog1');
      await manager.embedNode('article2');

      const queryVector = mockEmbed('article');
      const results = manager.findSimilar(queryVector, { type: 'article' });

      for (const result of results) {
        expect(result.node.type).toBe('article');
      }
    });

    test('respects minSimilarity threshold', async () => {
      await store.addNode('high-sim', 'document', { content: 'Very relevant' });
      await store.addNode('low-sim', 'document', { content: 'Not relevant' });

      await manager.embedNode('high-sim');
      await manager.embedNode('low-sim');

      const queryVector = mockEmbed('test');
      const results = manager.findSimilar(queryVector, { minSimilarity: 0.5 });

      for (const result of results) {
        expect(result.similarity).toBeGreaterThanOrEqual(0.5);
      }
    });

    test('returns empty array when no nodes have embeddings', () => {
      const queryVector = mockEmbed('test');
      const results = manager.findSimilar(queryVector);
      expect(results).toEqual([]);
    });

    test('sorts results by similarity descending', async () => {
      for (let i = 0; i < 10; i++) {
        await store.addNode(`sort-${i}`, 'document', { content: `Document ${i}` });
        await manager.embedNode(`sort-${i}`);
      }

      const queryVector = mockEmbed('test');
      const results = manager.findSimilar(queryVector);

      for (let i = 1; i < results.length; i++) {
        expect(results[i - 1].similarity).toBeGreaterThanOrEqual(results[i].similarity);
      }
    });
  });

  describe('findSimilarToNode', () => {
    test('finds nodes similar to a given node', async () => {
      await store.addNode('source', 'document', { content: 'Source document' });
      await store.addNode('similar1', 'document', { content: 'Similar document' });
      await store.addNode('similar2', 'document', { content: 'Another similar' });

      await manager.embedNode('source');
      await manager.embedNode('similar1');
      await manager.embedNode('similar2');

      const results = await manager.findSimilarToNode('source');

      expect(results.length).toBeGreaterThan(0);
      // Should not include the source node itself
      for (const result of results) {
        expect(result.node.id).not.toBe('source');
      }
    });

    test('auto-embeds node if not embedded', async () => {
      await store.addNode('auto-embed', 'document', { content: 'Auto embed test' });
      await store.addNode('other', 'document', { content: 'Other document' });
      await manager.embedNode('other');

      // Should not throw, should auto-embed
      await manager.findSimilarToNode('auto-embed');
      expect(manager.hasEmbedding('auto-embed')).toBe(true);
    });
  });

  describe('findSimilarToText', () => {
    test('finds nodes similar to query text', async () => {
      await store.addNode('text-search1', 'document', { content: 'Machine learning' });
      await store.addNode('text-search2', 'document', { content: 'Deep learning' });
      await store.addNode('text-search3', 'document', { content: 'Cooking recipes' });

      await manager.embedNode('text-search1');
      await manager.embedNode('text-search2');
      await manager.embedNode('text-search3');

      const results = await manager.findSimilarToText('AI and neural networks');

      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('getStats', () => {
    test('returns correct statistics', async () => {
      await store.addNode('stats1', 'document', { content: 'Doc 1' });
      await store.addNode('stats2', 'document', { content: 'Doc 2' });
      await store.addNode('stats3', 'document', {});

      await manager.embedNode('stats1');
      await manager.embedNode('stats2');

      const stats = manager.getStats();

      expect(stats.totalNodes).toBeGreaterThanOrEqual(3);
      expect(stats.embeddedNodes).toBe(2);
      expect(stats.model).toBe('@cf/baai/bge-base-en-v1.5');
      expect(stats.dimensions).toBe(768);
    });
  });

  describe('embedNodes (batch)', () => {
    test('embeds multiple nodes', async () => {
      await store.addNode('batch1', 'document', { content: 'Batch 1' });
      await store.addNode('batch2', 'document', { content: 'Batch 2' });
      await store.addNode('batch3', 'document', { content: 'Batch 3' });

      const results = await manager.embedNodes(['batch1', 'batch2', 'batch3']);

      expect(results.size).toBe(3);
      expect(results.get('batch1')).toBeDefined();
      expect(results.get('batch2')).toBeDefined();
      expect(results.get('batch3')).toBeDefined();
    });

    test('continues on individual failures', async () => {
      await store.addNode('batch-ok', 'document', { content: 'OK' });
      // batch-fail doesn't exist

      const results = await manager.embedNodes(['batch-ok', 'batch-fail']);

      expect(results.size).toBe(1);
      expect(results.get('batch-ok')).toBeDefined();
    });
  });

  describe('getEmbeddingEvents', () => {
    test('returns all events', async () => {
      await store.addNode('evt1', 'document', { content: 'Event 1' });
      await store.addNode('evt2', 'document', { content: 'Event 2' });

      await manager.embedNode('evt1');
      await manager.embedNode('evt2');

      const events = manager.getEmbeddingEvents();
      expect(events.length).toBeGreaterThanOrEqual(2);
    });

    test('respects limit parameter', async () => {
      for (let i = 0; i < 10; i++) {
        await store.addNode(`limit-${i}`, 'document', { content: `Content ${i}` });
        await manager.embedNode(`limit-${i}`);
      }

      const events = manager.getEmbeddingEvents(3);
      expect(events.length).toBe(3);
    });
  });
});
