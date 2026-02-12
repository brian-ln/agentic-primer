import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import GraphStore, { Node, Edge, Address, $ } from './graph';
import { rm, mkdir, readFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';

const TEST_DATA_DIR = join(import.meta.dir, '../.test-data');

async function cleanTestDir() {
  try {
    await rm(TEST_DATA_DIR, { recursive: true });
  } catch (e) {
    // Directory doesn't exist
  }
  await mkdir(TEST_DATA_DIR, { recursive: true });
}

// ============================================================================
// ADDRESS CLASS TESTS
// ============================================================================

describe('Address', () => {
  describe('constructor', () => {
    test('creates address with id only', () => {
      const addr = new Address('node123');
      expect(addr.id).toBe('node123');
      expect(addr.version).toBeNull();
      expect(addr._isAddress).toBe(true);
    });

    test('creates address with id and version', () => {
      const addr = new Address('node123', 'v2');
      expect(addr.id).toBe('node123');
      expect(addr.version).toBe('v2');
    });
  });

  describe('toString', () => {
    test('formats address without version', () => {
      const addr = new Address('mynode');
      expect(addr.toString()).toBe('@(mynode)');
    });

    test('formats address with version', () => {
      const addr = new Address('mynode', '1.0');
      expect(addr.toString()).toBe('@(mynode:1.0)');
    });
  });

  describe('isAddress', () => {
    test('returns true for Address instance', () => {
      const addr = new Address('test');
      expect(Address.isAddress(addr)).toBe(true);
    });

    test('returns false for plain object with same shape', () => {
      const fake = { id: 'test', version: null, _isAddress: false };
      expect(Address.isAddress(fake)).toBe(false);
    });

    test('returns false for null', () => {
      expect(Address.isAddress(null)).toBe(false);
    });

    test('returns false for undefined', () => {
      expect(Address.isAddress(undefined)).toBe(false);
    });

    test('returns false for string', () => {
      expect(Address.isAddress('node123')).toBe(false);
    });
  });
});

describe('$ helper', () => {
  test('creates Address with id only', () => {
    const addr = $('mynode');
    expect(addr).toBeInstanceOf(Address);
    expect(addr.id).toBe('mynode');
    expect(addr.version).toBeNull();
  });

  test('creates Address with id and version', () => {
    const addr = $('mynode', 'v3');
    expect(addr).toBeInstanceOf(Address);
    expect(addr.id).toBe('mynode');
    expect(addr.version).toBe('v3');
  });
});

// ============================================================================
// NODE CLASS TESTS
// ============================================================================

describe('Node', () => {
  describe('constructor', () => {
    test('creates node with minimal args', () => {
      const node = new Node('n1');
      expect(node.id).toBe('n1');
      expect(node.type).toBeNull();
      expect(node.properties.size).toBe(0);
      expect(node.data).toBeNull();
      expect(node.created).toBeLessThanOrEqual(Date.now());
      expect(node.modified).toBeLessThanOrEqual(Date.now());
    });

    test('creates node with all args', () => {
      const node = new Node('n1', 'person', { name: 'Alice' }, { extra: true });
      expect(node.id).toBe('n1');
      expect(node.type).toBe('person');
      expect(node.properties.get('name')).toBe('Alice');
      expect(node.data).toEqual({ extra: true });
    });
  });

  describe('toJSON', () => {
    test('serializes node correctly', () => {
      const node = new Node('n1', 'task', { priority: 'high' }, { nested: [1, 2] });
      const json = node.toJSON();

      expect(json.id).toBe('n1');
      expect(json.type).toBe('task');
      expect(json.properties).toEqual({ priority: 'high' });
      expect(json.data).toEqual({ nested: [1, 2] });
      expect(json.created).toBe(node.created);
      expect(json.modified).toBe(node.modified);
    });
  });
});

// ============================================================================
// EDGE CLASS TESTS
// ============================================================================

describe('Edge', () => {
  describe('constructor', () => {
    test('creates edge with minimal args', () => {
      const edge = new Edge('e1', 'n1', 'n2');
      expect(edge.id).toBe('e1');
      expect(edge.from).toBe('n1');
      expect(edge.to).toBe('n2');
      expect(edge.type).toBeNull();
      expect(edge.weight).toBe(1);
      expect(edge.properties.size).toBe(0);
    });

    test('creates edge with all args', () => {
      const edge = new Edge('e1', 'n1', 'n2', 'follows', { since: 2020 }, 0.8);
      expect(edge.type).toBe('follows');
      expect(edge.weight).toBe(0.8);
      expect(edge.properties.get('since')).toBe(2020);
    });
  });

  describe('toJSON', () => {
    test('serializes edge correctly', () => {
      const edge = new Edge('e1', 'from_node', 'to_node', 'connects', { label: 'link' }, 2.5);
      const json = edge.toJSON();

      expect(json.id).toBe('e1');
      expect(json.from).toBe('from_node');
      expect(json.to).toBe('to_node');
      expect(json.type).toBe('connects');
      expect(json.weight).toBe(2.5);
      expect(json.properties).toEqual({ label: 'link' });
      expect(json.created).toBe(edge.created);
    });
  });
});

// ============================================================================
// GRAPHSTORE CRUD TESTS
// ============================================================================

describe('GraphStore CRUD', () => {
  let store: GraphStore;

  beforeEach(async () => {
    await cleanTestDir();
    store = new GraphStore(TEST_DATA_DIR);
    await store.initialize();
  });

  describe('addNode', () => {
    test('adds node and returns it', async () => {
      const node = await store.addNode('n1', 'task', { title: 'Test' });
      expect(node.id).toBe('n1');
      expect(node.type).toBe('task');
      expect(store.nodes.size).toBe(1);
    });

    test('indexes node by type', async () => {
      await store.addNode('n1', 'task');
      await store.addNode('n2', 'task');
      await store.addNode('n3', 'person');

      const tasks = store.getByType('task');
      expect(tasks.length).toBe(2);
    });
  });

  describe('addEdge', () => {
    test('adds edge and returns it', async () => {
      await store.addNode('n1', 'task');
      await store.addNode('n2', 'task');
      const edge = await store.addEdge('e1', 'n1', 'n2', 'depends_on');

      expect(edge.id).toBe('e1');
      expect(edge.from).toBe('n1');
      expect(edge.to).toBe('n2');
      expect(store.edges.size).toBe(1);
    });

    test('indexes edge in adjacency lists', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2', 'link');

      expect(store.adjacencyOut.get('n1')?.length).toBe(1);
      expect(store.adjacencyIn.get('n2')?.length).toBe(1);
    });
  });

  describe('deleteNode', () => {
    test('deletes existing node and returns true', async () => {
      await store.addNode('n1', 'task');
      const result = await store.deleteNode('n1');

      expect(result).toBe(true);
      expect(store.nodes.size).toBe(0);
    });

    test('returns false for non-existent node', async () => {
      const result = await store.deleteNode('nonexistent');
      expect(result).toBe(false);
    });

    test('removes node from type index', async () => {
      await store.addNode('n1', 'task');
      await store.deleteNode('n1');

      const tasks = store.getByType('task');
      expect(tasks.length).toBe(0);
    });

    test('removes node from property index', async () => {
      await store.addNode('n1', 'task', { status: 'active' });
      await store.deleteNode('n1');

      const active = store.getByProperty('status', 'active');
      expect(active.length).toBe(0);
    });

    test('emits NodeDeleted event', async () => {
      await store.addNode('n1', 'task');
      await store.deleteNode('n1');

      const events = store.getEventHistory();
      const deleteEvent = events.find(e => e.type === 'NodeDeleted');
      expect(deleteEvent).toBeDefined();
      expect(deleteEvent!.data.id).toBe('n1');
    });
  });

  describe('deleteEdge', () => {
    test('deletes existing edge and returns true', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2');
      const result = await store.deleteEdge('e1');

      expect(result).toBe(true);
      expect(store.edges.size).toBe(0);
    });

    test('returns false for non-existent edge', async () => {
      const result = await store.deleteEdge('nonexistent');
      expect(result).toBe(false);
    });

    test('removes edge from adjacency lists', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2');
      await store.deleteEdge('e1');

      expect(store.adjacencyOut.get('n1')?.length ?? 0).toBe(0);
      expect(store.adjacencyIn.get('n2')?.length ?? 0).toBe(0);
    });

    test('emits EdgeDeleted event', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2');
      await store.deleteEdge('e1');

      const events = store.getEventHistory();
      const deleteEvent = events.find(e => e.type === 'EdgeDeleted');
      expect(deleteEvent).toBeDefined();
      expect(deleteEvent!.data.id).toBe('e1');
    });
  });

  describe('updateNode', () => {
    test('updates node properties', async () => {
      await store.addNode('n1', 'task', { status: 'pending' });
      const updated = await store.updateNode('n1', { status: 'done', priority: 'high' });

      expect(updated).not.toBeNull();
      expect(updated!.properties.get('status')).toBe('done');
      expect(updated!.properties.get('priority')).toBe('high');
    });

    test('returns null for non-existent node', async () => {
      const result = await store.updateNode('nonexistent', { x: 1 });
      expect(result).toBeNull();
    });

    test('updates property index on change', async () => {
      await store.addNode('n1', 'task', { status: 'pending' });
      await store.updateNode('n1', { status: 'done' });

      expect(store.getByProperty('status', 'pending').length).toBe(0);
      expect(store.getByProperty('status', 'done').length).toBe(1);
    });
  });

  describe('get', () => {
    test('retrieves node by id', async () => {
      await store.addNode('n1', 'task');
      const node = store.get('n1');
      expect(node).toBeInstanceOf(Node);
    });

    test('retrieves edge by id', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2');
      const edge = store.get('e1');
      expect(edge).toBeInstanceOf(Edge);
    });

    test('returns undefined for non-existent id', () => {
      const result = store.get('nonexistent');
      expect(result).toBeUndefined();
    });
  });
});

// ============================================================================
// QUERY METHODS TESTS
// ============================================================================

describe('GraphStore Query Methods', () => {
  let store: GraphStore;

  beforeEach(async () => {
    await cleanTestDir();
    store = new GraphStore(TEST_DATA_DIR);
    await store.initialize();
  });

  describe('getByType', () => {
    test('returns all nodes of a type', async () => {
      await store.addNode('t1', 'task');
      await store.addNode('t2', 'task');
      await store.addNode('p1', 'person');

      const tasks = store.getByType('task');
      expect(tasks.length).toBe(2);
      expect(tasks.every(n => n.type === 'task')).toBe(true);
    });

    test('returns empty array for unknown type', () => {
      const results = store.getByType('unknown');
      expect(results).toEqual([]);
    });
  });

  describe('getByProperty', () => {
    test('returns nodes with matching property', async () => {
      await store.addNode('n1', 'task', { status: 'active' });
      await store.addNode('n2', 'task', { status: 'active' });
      await store.addNode('n3', 'task', { status: 'done' });

      const active = store.getByProperty('status', 'active');
      expect(active.length).toBe(2);
    });

    test('returns empty array when no match', () => {
      const results = store.getByProperty('nonexistent', 'value');
      expect(results).toEqual([]);
    });
  });

  describe('search', () => {
    test('finds nodes by text content', async () => {
      await store.addNode('n1', 'task', { title: 'Important meeting' });
      await store.addNode('n2', 'task', { title: 'Regular task' });

      const results = store.search('important');
      expect(results.length).toBe(1);
      expect(results[0].id).toBe('n1');
    });

    test('returns intersection for multiple terms', async () => {
      await store.addNode('n1', 'task', { title: 'important urgent meeting' });
      await store.addNode('n2', 'task', { title: 'important regular task' });

      const results = store.search('important urgent');
      expect(results.length).toBe(1);
      expect(results[0].id).toBe('n1');
    });

    test('returns empty for short query', () => {
      const results = store.search('ab');
      expect(results).toEqual([]);
    });

    test('is case insensitive', async () => {
      await store.addNode('n1', 'task', { title: 'URGENT Task' });
      const results = store.search('urgent');
      expect(results.length).toBe(1);
    });
  });
});

// ============================================================================
// PATHFINDING TESTS
// ============================================================================

describe('GraphStore Pathfinding', () => {
  let store: GraphStore;

  beforeEach(async () => {
    await cleanTestDir();
    store = new GraphStore(TEST_DATA_DIR);
    await store.initialize();
  });

  describe('findShortestPath', () => {
    test('returns self path for same node', async () => {
      await store.addNode('n1');
      const result = store.findShortestPath('n1', 'n1');

      expect(result).not.toBeNull();
      expect(result!.path).toEqual(['n1']);
      expect(result!.distance).toBe(0);
      expect(result!.nodes.length).toBe(1);
    });

    test('finds direct path between connected nodes', async () => {
      await store.addNode('a');
      await store.addNode('b');
      await store.addEdge('e1', 'a', 'b');

      const result = store.findShortestPath('a', 'b');

      expect(result).not.toBeNull();
      expect(result!.path).toEqual(['a', 'b']);
      expect(result!.distance).toBe(1);
    });

    test('finds multi-hop path', async () => {
      await store.addNode('a');
      await store.addNode('b');
      await store.addNode('c');
      await store.addEdge('e1', 'a', 'b');
      await store.addEdge('e2', 'b', 'c');

      const result = store.findShortestPath('a', 'c');

      expect(result).not.toBeNull();
      expect(result!.path).toEqual(['a', 'b', 'c']);
      expect(result!.distance).toBe(2);
    });

    test('considers edge weights', async () => {
      // a -> b (weight 10) -> c
      // a -> d (weight 1) -> c (weight 1)
      await store.addNode('a');
      await store.addNode('b');
      await store.addNode('c');
      await store.addNode('d');
      await store.addEdge('e1', 'a', 'b', null, {}, 10);
      await store.addEdge('e2', 'b', 'c', null, {}, 1);
      await store.addEdge('e3', 'a', 'd', null, {}, 1);
      await store.addEdge('e4', 'd', 'c', null, {}, 1);

      const result = store.findShortestPath('a', 'c');

      expect(result).not.toBeNull();
      expect(result!.path).toEqual(['a', 'd', 'c']); // Shorter weighted path
      expect(result!.distance).toBe(2);
    });

    test('returns null when no path exists', async () => {
      await store.addNode('a');
      await store.addNode('b');
      // No edge between them

      const result = store.findShortestPath('a', 'b');
      expect(result).toBeNull();
    });

    test('handles disconnected components', async () => {
      await store.addNode('a');
      await store.addNode('b');
      await store.addEdge('e1', 'a', 'b');

      await store.addNode('c');
      await store.addNode('d');
      await store.addEdge('e2', 'c', 'd');

      // a-b is disconnected from c-d
      const result = store.findShortestPath('a', 'c');
      expect(result).toBeNull();
    });

    test('finds path in larger graph', async () => {
      // Create a chain: 1 -> 2 -> 3 -> 4 -> 5
      for (let i = 1; i <= 5; i++) {
        await store.addNode(`n${i}`);
      }
      for (let i = 1; i < 5; i++) {
        await store.addEdge(`e${i}`, `n${i}`, `n${i + 1}`);
      }

      const result = store.findShortestPath('n1', 'n5');

      expect(result).not.toBeNull();
      expect(result!.path).toEqual(['n1', 'n2', 'n3', 'n4', 'n5']);
      expect(result!.distance).toBe(4);
    });
  });
});

// ============================================================================
// TRAVERSAL TESTS
// ============================================================================

describe('GraphStore Traversal', () => {
  let store: GraphStore;

  beforeEach(async () => {
    await cleanTestDir();
    store = new GraphStore(TEST_DATA_DIR);
    await store.initialize();

    // Build a test graph:
    //     n1 --follows--> n2 --follows--> n3
    //      \--likes-----> n4
    await store.addNode('n1', 'person');
    await store.addNode('n2', 'person');
    await store.addNode('n3', 'person');
    await store.addNode('n4', 'post');
    await store.addEdge('e1', 'n1', 'n2', 'follows');
    await store.addEdge('e2', 'n2', 'n3', 'follows');
    await store.addEdge('e3', 'n1', 'n4', 'likes');
  });

  describe('traverse', () => {
    test('BFS traversal (default)', async () => {
      const results = store.traverse('n1');

      expect(results.length).toBeGreaterThan(0);
      expect(results[0].node.id).toBe('n1');
      expect(results[0].depth).toBe(0);

      // BFS should visit n2 and n4 at depth 1 before n3 at depth 2
      const depths = results.map(r => ({ id: r.node.id, depth: r.depth }));
      expect(depths.find(d => d.id === 'n2')?.depth).toBe(1);
      expect(depths.find(d => d.id === 'n4')?.depth).toBe(1);
    });

    test('DFS traversal', async () => {
      const results = store.traverse('n1', { algorithm: 'dfs' });

      expect(results.length).toBeGreaterThan(0);
      expect(results[0].node.id).toBe('n1');
    });

    test('respects maxDepth', async () => {
      const results = store.traverse('n1', { maxDepth: 1 });

      // Should only reach depth 0 (n1) and depth 1 (n2, n4)
      const maxDepth = Math.max(...results.map(r => r.depth));
      expect(maxDepth).toBeLessThanOrEqual(1);
      expect(results.find(r => r.node.id === 'n3')).toBeUndefined();
    });

    test('direction "out" follows outgoing edges', async () => {
      const results = store.traverse('n1', { direction: 'out', maxDepth: 1 });

      const ids = results.map(r => r.node.id);
      expect(ids).toContain('n1');
      expect(ids).toContain('n2');
      expect(ids).toContain('n4');
    });

    test('direction "in" follows incoming edges', async () => {
      const results = store.traverse('n3', { direction: 'in', maxDepth: 2 });

      const ids = results.map(r => r.node.id);
      expect(ids).toContain('n3');
      expect(ids).toContain('n2');
      expect(ids).toContain('n1');
    });

    test('direction "both" follows all edges', async () => {
      const results = store.traverse('n2', { direction: 'both', maxDepth: 1 });

      const ids = results.map(r => r.node.id);
      expect(ids).toContain('n2');
      expect(ids).toContain('n1'); // incoming
      expect(ids).toContain('n3'); // outgoing
    });

    test('filters by edge type', async () => {
      const results = store.traverse('n1', { edgeType: 'follows', maxDepth: 10 });

      const ids = results.map(r => r.node.id);
      expect(ids).toContain('n1');
      expect(ids).toContain('n2');
      expect(ids).toContain('n3');
      expect(ids).not.toContain('n4'); // n4 is connected via 'likes', not 'follows'
    });

    test('respects maxResults', async () => {
      const results = store.traverse('n1', { maxResults: 2 });

      expect(results.length).toBe(2);
    });

    test('includes path in results', async () => {
      const results = store.traverse('n1', { maxDepth: 2 });

      const n3Result = results.find(r => r.node.id === 'n3');
      expect(n3Result).toBeDefined();
      expect(n3Result!.path).toEqual(['n1', 'n2', 'n3']);
    });

    test('handles isolated node', async () => {
      await store.addNode('isolated');
      const results = store.traverse('isolated');

      expect(results.length).toBe(1);
      expect(results[0].node.id).toBe('isolated');
      expect(results[0].depth).toBe(0);
    });
  });
});

// ============================================================================
// ANALYTICS TESTS
// ============================================================================

describe('GraphStore Analytics', () => {
  let store: GraphStore;

  beforeEach(async () => {
    await cleanTestDir();
    store = new GraphStore(TEST_DATA_DIR);
    await store.initialize();

    // Build test graph:
    // hub has 3 outgoing, 0 incoming
    // n1, n2, n3 have 0 outgoing, 1 incoming each
    // n4 has 1 outgoing to hub, 1 incoming from hub
    await store.addNode('hub');
    await store.addNode('n1');
    await store.addNode('n2');
    await store.addNode('n3');
    await store.addNode('n4');
    await store.addEdge('e1', 'hub', 'n1');
    await store.addEdge('e2', 'hub', 'n2');
    await store.addEdge('e3', 'hub', 'n3');
    await store.addEdge('e4', 'hub', 'n4');
    await store.addEdge('e5', 'n4', 'hub');
  });

  describe('getNodeDegree', () => {
    test('returns correct in/out/total for hub node', () => {
      const degree = store.getNodeDegree('hub');

      expect(degree.out).toBe(4);
      expect(degree.in).toBe(1); // n4 -> hub
      expect(degree.total).toBe(5);
    });

    test('returns correct degree for leaf node', () => {
      const degree = store.getNodeDegree('n1');

      expect(degree.out).toBe(0);
      expect(degree.in).toBe(1);
      expect(degree.total).toBe(1);
    });

    test('returns zeros for node with no edges', async () => {
      await store.addNode('isolated');
      const degree = store.getNodeDegree('isolated');

      expect(degree.in).toBe(0);
      expect(degree.out).toBe(0);
      expect(degree.total).toBe(0);
    });

    test('returns zeros for non-existent node', () => {
      const degree = store.getNodeDegree('nonexistent');

      expect(degree.in).toBe(0);
      expect(degree.out).toBe(0);
      expect(degree.total).toBe(0);
    });
  });

  describe('getMostConnected', () => {
    test('returns nodes sorted by total degree', () => {
      const most = store.getMostConnected(3);

      expect(most[0].id).toBe('hub');
      expect(most[0].total).toBe(5);
    });

    test('respects limit parameter', () => {
      const most = store.getMostConnected(2);
      expect(most.length).toBe(2);
    });

    test('returns all nodes when limit exceeds node count', () => {
      const most = store.getMostConnected(100);
      expect(most.length).toBe(5);
    });

    test('defaults to limit 10', () => {
      const most = store.getMostConnected();
      expect(most.length).toBeLessThanOrEqual(10);
    });
  });
});

// ============================================================================
// UTILITY METHODS TESTS
// ============================================================================

describe('GraphStore Utilities', () => {
  let store: GraphStore;

  beforeEach(async () => {
    await cleanTestDir();
    store = new GraphStore(TEST_DATA_DIR);
    await store.initialize();
  });

  describe('shutdown', () => {
    test('creates snapshot on shutdown', async () => {
      await store.addNode('n1', 'task');
      await store.shutdown();

      expect(existsSync(join(TEST_DATA_DIR, 'snapshot.json'))).toBe(true);
    });

    test('snapshot contains node data', async () => {
      await store.addNode('n1', 'task', { name: 'Test' });
      await store.shutdown();

      const snapshot = JSON.parse(await readFile(join(TEST_DATA_DIR, 'snapshot.json'), 'utf-8'));
      expect(snapshot.nodes.length).toBe(1);
      expect(snapshot.nodes[0].id).toBe('n1');
    });
  });

  describe('getEventHistory', () => {
    test('returns events in order', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2');

      const events = store.getEventHistory();

      expect(events.length).toBe(3);
      expect(events[0].type).toBe('NodeAdded');
      expect(events[1].type).toBe('NodeAdded');
      expect(events[2].type).toBe('EdgeAdded');
    });

    test('respects limit parameter', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addNode('n3');

      const events = store.getEventHistory(2);

      expect(events.length).toBe(2);
      // Should return the last 2 events
      expect(events[0].data.id).toBe('n2');
      expect(events[1].data.id).toBe('n3');
    });

    test('returns empty array for new store', () => {
      const events = store.getEventHistory();
      expect(events).toEqual([]);
    });
  });

  describe('resolve', () => {
    test('resolves Address to Node', async () => {
      await store.addNode('n1', 'task', { title: 'Test' });
      const addr = $('n1');

      const resolved = store.resolve(addr);

      expect(resolved).toBeInstanceOf(Node);
      expect((resolved as Node).id).toBe('n1');
    });

    test('resolves Address to Edge', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2');
      const addr = $('e1');

      const resolved = store.resolve(addr);

      expect(resolved).toBeInstanceOf(Edge);
      expect((resolved as Edge).id).toBe('e1');
    });

    test('returns non-Address values unchanged', () => {
      const plainValue = { foo: 'bar' };
      const resolved = store.resolve(plainValue);

      expect(resolved).toBe(plainValue);
    });

    test('returns string unchanged', () => {
      const str = 'hello';
      const resolved = store.resolve(str);
      expect(resolved).toBe(str);
    });

    test('returns undefined for Address pointing to non-existent entity', async () => {
      const addr = $('nonexistent');
      const resolved = store.resolve(addr);
      expect(resolved).toBeUndefined();
    });
  });

  describe('stats', () => {
    test('tracks node count', async () => {
      await store.addNode('n1');
      await store.addNode('n2');

      expect(store.stats.nodeCount).toBe(2);
    });

    test('tracks edge count', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.addEdge('e1', 'n1', 'n2');

      expect(store.stats.edgeCount).toBe(1);
    });

    test('updates on delete', async () => {
      await store.addNode('n1');
      await store.addNode('n2');
      await store.deleteNode('n1');

      expect(store.stats.nodeCount).toBe(1);
    });
  });
});

// ============================================================================
// PERSISTENCE TESTS
// ============================================================================

describe('GraphStore Persistence', () => {
  beforeEach(async () => {
    await cleanTestDir();
  });

  describe('WAL replay', () => {
    test('recovers nodes after restart', async () => {
      const store1 = new GraphStore(TEST_DATA_DIR);
      await store1.initialize();
      await store1.addNode('n1', 'task', { title: 'Persisted' });

      // Simulate restart
      const store2 = new GraphStore(TEST_DATA_DIR);
      await store2.initialize();

      expect(store2.nodes.size).toBe(1);
      expect(store2.nodes.get('n1')?.properties.get('title')).toBe('Persisted');
    });

    test('recovers edges after restart', async () => {
      const store1 = new GraphStore(TEST_DATA_DIR);
      await store1.initialize();
      await store1.addNode('n1');
      await store1.addNode('n2');
      await store1.addEdge('e1', 'n1', 'n2', 'connects');

      const store2 = new GraphStore(TEST_DATA_DIR);
      await store2.initialize();

      expect(store2.edges.size).toBe(1);
      expect(store2.edges.get('e1')?.type).toBe('connects');
    });

    test('maintains index integrity after recovery', async () => {
      const store1 = new GraphStore(TEST_DATA_DIR);
      await store1.initialize();
      await store1.addNode('n1', 'task');
      await store1.addNode('n2', 'task');

      const store2 = new GraphStore(TEST_DATA_DIR);
      await store2.initialize();

      const tasks = store2.getByType('task');
      expect(tasks.length).toBe(2);
    });
  });

  describe('snapshot + WAL', () => {
    test('recovers from snapshot plus subsequent events', async () => {
      const store1 = new GraphStore(TEST_DATA_DIR);
      await store1.initialize();
      await store1.addNode('n1', 'task');
      await store1.shutdown(); // Creates snapshot

      await store1.addNode('n2', 'task'); // After snapshot

      const store2 = new GraphStore(TEST_DATA_DIR);
      await store2.initialize();

      expect(store2.nodes.size).toBe(2);
    });
  });
});

// ============================================================================
// TAG OPERATIONS TESTS
// ============================================================================

describe('GraphStore Tag Operations', () => {
  let store: GraphStore;

  beforeEach(async () => {
    await cleanTestDir();
    store = new GraphStore(TEST_DATA_DIR);
    await store.initialize();
  });

  describe('addTags', () => {
    test('adds tags to a node without existing tags', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node' });

      const result = await store.addTags('node1', ['tag1', 'tag2']);

      expect(result).not.toBeNull();
      expect(result!.properties.get('tags')).toEqual(['tag1', 'tag2']);
    });

    test('adds tags to a node with existing tags', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['existing'] });

      const result = await store.addTags('node1', ['tag1', 'tag2']);

      expect(result).not.toBeNull();
      expect(result!.properties.get('tags')).toEqual(['existing', 'tag1', 'tag2']);
    });

    test('does not add duplicate tags', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1'] });

      const result = await store.addTags('node1', ['tag1', 'tag2']);

      expect(result).not.toBeNull();
      expect(result!.properties.get('tags')).toEqual(['tag1', 'tag2']);
    });

    test('returns null for non-existent node', async () => {
      const result = await store.addTags('nonexistent', ['tag1']);

      expect(result).toBeNull();
    });

    test('returns node unchanged when all tags already exist', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1', 'tag2'] });

      const result = await store.addTags('node1', ['tag1', 'tag2']);

      expect(result).not.toBeNull();
      expect(result!.properties.get('tags')).toEqual(['tag1', 'tag2']);
    });

    test('emits NodeTagged event', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node' });

      await store.addTags('node1', ['tag1', 'tag2']);

      const events = store.getEventHistory(10);
      const tagEvent = events.find(e => e.type === 'NodeTagged');

      expect(tagEvent).toBeDefined();
      expect(tagEvent!.data.id).toBe('node1');
      expect(tagEvent!.data.tags).toEqual(['tag1', 'tag2']);
    });
  });

  describe('removeTags', () => {
    test('removes tags from a node', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1', 'tag2', 'tag3'] });

      const result = await store.removeTags('node1', ['tag2']);

      expect(result).not.toBeNull();
      expect(result!.properties.get('tags')).toEqual(['tag1', 'tag3']);
    });

    test('removes multiple tags', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1', 'tag2', 'tag3'] });

      const result = await store.removeTags('node1', ['tag1', 'tag3']);

      expect(result).not.toBeNull();
      expect(result!.properties.get('tags')).toEqual(['tag2']);
    });

    test('removes tags property when all tags removed', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1'] });

      const result = await store.removeTags('node1', ['tag1']);

      expect(result).not.toBeNull();
      expect(result!.properties.has('tags')).toBe(false);
    });

    test('returns null for non-existent node', async () => {
      const result = await store.removeTags('nonexistent', ['tag1']);

      expect(result).toBeNull();
    });

    test('returns node unchanged when no tags match', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1'] });

      const result = await store.removeTags('node1', ['nonexistent']);

      expect(result).not.toBeNull();
      expect(result!.properties.get('tags')).toEqual(['tag1']);
    });

    test('emits NodeUntagged event', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1', 'tag2'] });

      await store.removeTags('node1', ['tag1']);

      const events = store.getEventHistory(10);
      const untagEvent = events.find(e => e.type === 'NodeUntagged');

      expect(untagEvent).toBeDefined();
      expect(untagEvent!.data.id).toBe('node1');
      expect(untagEvent!.data.tags).toEqual(['tag1']);
    });
  });

  describe('getByTag', () => {
    test('returns nodes with specific tag', async () => {
      await store.addNode('node1', 'test', { name: 'Node 1', tags: ['important'] });
      await store.addNode('node2', 'test', { name: 'Node 2', tags: ['important', 'vip'] });
      await store.addNode('node3', 'test', { name: 'Node 3', tags: ['other'] });

      const results = store.getByTag('important');

      expect(results.length).toBe(2);
      const ids = results.map(n => n.id);
      expect(ids).toContain('node1');
      expect(ids).toContain('node2');
    });

    test('returns empty array when no nodes have tag', async () => {
      await store.addNode('node1', 'test', { name: 'Node 1', tags: ['other'] });

      const results = store.getByTag('nonexistent');

      expect(results).toEqual([]);
    });

    test('returns empty array when no nodes have tags', async () => {
      await store.addNode('node1', 'test', { name: 'Node 1' });

      const results = store.getByTag('any');

      expect(results).toEqual([]);
    });
  });

  describe('getByTags', () => {
    beforeEach(async () => {
      await store.addNode('node1', 'test', { name: 'Node 1', tags: ['a', 'b'] });
      await store.addNode('node2', 'test', { name: 'Node 2', tags: ['b', 'c'] });
      await store.addNode('node3', 'test', { name: 'Node 3', tags: ['a', 'b', 'c'] });
      await store.addNode('node4', 'test', { name: 'Node 4', tags: ['d'] });
    });

    test('returns nodes matching ANY tag (matchAll=false)', async () => {
      const results = store.getByTags(['a', 'c'], false);

      expect(results.length).toBe(3);
      const ids = results.map(n => n.id);
      expect(ids).toContain('node1'); // has 'a'
      expect(ids).toContain('node2'); // has 'c'
      expect(ids).toContain('node3'); // has both
    });

    test('returns nodes matching ALL tags (matchAll=true)', async () => {
      const results = store.getByTags(['a', 'b'], true);

      expect(results.length).toBe(2);
      const ids = results.map(n => n.id);
      expect(ids).toContain('node1'); // has 'a' and 'b'
      expect(ids).toContain('node3'); // has 'a', 'b', and 'c'
    });

    test('returns empty array when no nodes match all tags', async () => {
      const results = store.getByTags(['a', 'd'], true);

      expect(results).toEqual([]);
    });

    test('returns empty array for empty tags array', async () => {
      const results = store.getByTags([]);

      expect(results).toEqual([]);
    });

    test('defaults to matchAll=false', async () => {
      const results = store.getByTags(['a']);

      expect(results.length).toBe(2);
    });
  });

  describe('getNodeTags', () => {
    test('returns tags for node with tags', async () => {
      await store.addNode('node1', 'test', { name: 'Test', tags: ['tag1', 'tag2'] });

      const tags = store.getNodeTags('node1');

      expect(tags).toEqual(['tag1', 'tag2']);
    });

    test('returns empty array for node without tags', async () => {
      await store.addNode('node1', 'test', { name: 'Test' });

      const tags = store.getNodeTags('node1');

      expect(tags).toEqual([]);
    });

    test('returns empty array for non-existent node', async () => {
      const tags = store.getNodeTags('nonexistent');

      expect(tags).toEqual([]);
    });
  });

  describe('tag persistence', () => {
    test('tags persist after shutdown and reload', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node' });
      await store.addTags('node1', ['persistent', 'tag']);
      await store.shutdown();

      // Create new store instance and reload
      const store2 = new GraphStore(TEST_DATA_DIR);
      await store2.initialize();

      const tags = store2.getNodeTags('node1');
      expect(tags).toEqual(['persistent', 'tag']);
    });

    test('tag removal persists after shutdown and reload', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['tag1', 'tag2'] });
      await store.removeTags('node1', ['tag1']);
      await store.shutdown();

      // Create new store instance and reload
      const store2 = new GraphStore(TEST_DATA_DIR);
      await store2.initialize();

      const tags = store2.getNodeTags('node1');
      expect(tags).toEqual(['tag2']);
    });
  });

  describe('tag indexing', () => {
    test('tags are searchable via text index', async () => {
      await store.addNode('node1', 'test', { name: 'Test Node', tags: ['important', 'urgent'] });

      // Tags should be indexed as text
      const results = store.search('important');

      expect(results.length).toBe(1);
      expect(results[0].id).toBe('node1');
    });
  });
});
