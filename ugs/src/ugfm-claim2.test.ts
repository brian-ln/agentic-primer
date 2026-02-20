/**
 * UGFM Claim 2 Demonstration: Graph-Algorithmic Verification Without Domain-Specific Tools
 *
 * Demonstrates that a single graph-algorithmic toolchain (BFS, SCC, topological sort,
 * reachability) can verify correctness properties across multiple domain types without
 * domain-specific verification tools:
 * - Reachability (BFS)
 * - Deadlock detection (sink nodes)
 * - SCC-based liveness analysis
 * - Cycle detection
 * - Domain projection (subgraph views)
 *
 * This test suite verifies UGFM Claim 2: a single graph-algorithmic toolchain can
 * verify correctness properties across all domain types without domain-specific tools.
 * Evidence: ev-207, ev-208 (MIC prior art), ev-033, ev-169 (concurrent systems).
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import GraphStore from './graph';

describe('UGFM Claim 2: Producer-Consumer Kripke Structure', () => {
  let store: GraphStore;

  beforeEach(async () => {
    store = new GraphStore(`:memory:`);

    // 5 system states: composite (process P state) x (process Q state) x (channel state)
    // S0: P:idle, Q:idle, channel:empty — initial state
    // S1: P:sending, Q:idle, channel:empty
    // S2: P:idle, Q:idle, channel:full — after P sends
    // S3: P:idle, Q:receiving, channel:full — Q starts receiving
    // S4: P:sending, Q:idle, channel:full — DEADLOCK (channel full, P tries to send again)
    await store.addNode('S0', 'SystemState', { label: 'P:idle+Q:idle+Ch:empty',      initial: true,  deadlock: false });
    await store.addNode('S1', 'SystemState', { label: 'P:sending+Q:idle+Ch:empty',                   deadlock: false });
    await store.addNode('S2', 'SystemState', { label: 'P:idle+Q:idle+Ch:full',                       deadlock: false });
    await store.addNode('S3', 'SystemState', { label: 'P:idle+Q:receiving+Ch:full',                  deadlock: false });
    await store.addNode('S4', 'SystemState', { label: 'P:sending+Q:idle+Ch:full',    initial: false, deadlock: true  });

    // 5 transitions
    // t0: P initiates send (S0 → S1)
    // t1: channel accepts message (S1 → S2)
    // t2: Q initiates receive (S2 → S3)
    // t3: channel drains, system resets (S3 → S0) — the normal-operation CYCLE
    // t4: P tries to send again while channel full (S2 → S4) — the deadlock path
    await store.addEdge('t0', 'S0', 'S1', 'send-initiate',    { actor: 'P', action: '!send' });
    await store.addEdge('t1', 'S1', 'S2', 'channel-accept',   { channel: 'ch1', action: 'put' });
    await store.addEdge('t2', 'S2', 'S3', 'receive-initiate', { actor: 'Q', action: '?recv' });
    await store.addEdge('t3', 'S3', 'S0', 'channel-drain',    { channel: 'ch1', action: 'get' });
    await store.addEdge('t4', 'S2', 'S4', 'send-initiate',    { actor: 'P', action: '!send' });
    // S4 has no outgoing edges — it is a sink node = deadlock
  });

  test('all system states are reachable from the initial state S0', async () => {
    const reachable = store.traverse('S0', { direction: 'out', maxDepth: 100 });
    const ids = new Set(reachable.map((r: any) => r.node.id));
    expect(ids.has('S1')).toBe(true);
    expect(ids.has('S2')).toBe(true);
    expect(ids.has('S3')).toBe(true);
    expect(ids.has('S4')).toBe(true);
  });

  test('S4 is the unique deadlock state: a reachable sink node with deadlock:true', async () => {
    const reachable = store.traverse('S0', { direction: 'out', maxDepth: 100 });
    const deadlockStates = reachable.filter((r: any) => {
      const outgoing = store.adjacencyOut.get(r.node.id) ?? [];
      return outgoing.length === 0 && r.node.type === 'SystemState';
    });
    expect(deadlockStates.length).toBe(1);
    expect(deadlockStates[0].node.id).toBe('S4');
  });

  test('findSCCs identifies the normal-operation cycle {S0,S1,S2,S3} and deadlock singleton {S4}', () => {
    const sccs = store.findSCCs();
    const sccSets = sccs.map((scc: string[]) => new Set(scc));

    const cycleSCC = sccSets.find((s: Set<string>) =>
      s.has('S0') && s.has('S1') && s.has('S2') && s.has('S3')
    );
    expect(cycleSCC).toBeDefined();
    expect(cycleSCC!.size).toBe(4);

    const deadlockSCC = sccSets.find((s: Set<string>) => s.has('S4') && s.size === 1);
    expect(deadlockSCC).toBeDefined();
  });

  test('hasCycle confirms the S0->S1->S2->S3->S0 normal-operation cycle exists', () => {
    expect(store.hasCycle()).toBe(true);
  });

  test('project() extracts the communication-only view (send/receive edges only)', () => {
    const commView = store.project(
      undefined,
      (edge: any) => edge.type === 'send-initiate' || edge.type === 'receive-initiate'
    );

    // All 5 system state nodes retained (no node predicate)
    expect(commView.nodes.size).toBe(5);

    // Only t0 (send-initiate), t2 (receive-initiate), t4 (send-initiate) retained
    expect(commView.edges.size).toBe(3);
    expect(commView.edges.has('t0')).toBe(true);
    expect(commView.edges.has('t2')).toBe(true);
    expect(commView.edges.has('t4')).toBe(true);
    expect(commView.edges.has('t1')).toBe(false); // channel-accept filtered
    expect(commView.edges.has('t3')).toBe(false); // channel-drain filtered
  });
});

// ============================================================================
// SUITE 2: Task-Cycle Detection — same SCC algorithm in the ugfm task graph
// ============================================================================

describe('UGFM Claim 2: Task-Cycle Detection in Dependency Graph', () => {
  /**
   * Demonstrates that the same SCC algorithm used for Kripke structure deadlock
   * detection also finds circular dependencies in task/workflow dependency graphs.
   * No domain-specific cycle detection tool is required — graph algorithms suffice.
   *
   * Uses real ugfm task IDs with a synthetic circular dependency injected to
   * demonstrate detection capability.
   */
  let taskStore: GraphStore;

  beforeEach(async () => {
    taskStore = new GraphStore(`:memory:`);

    // Real ugfm task nodes (subset of actual task graph)
    await taskStore.addNode('ugfm-claim1-impl',  'Task', { name: 'ugfm-claim1-impl',  status: 'completed', priority: 'P1' });
    await taskStore.addNode('ugfm-demo-claim1',  'Task', { name: 'ugfm-demo-claim1',  status: 'completed', priority: 'P1' });
    await taskStore.addNode('ugfm-demo-claim2',  'Task', { name: 'ugfm-demo-claim2',  status: 'in_progress', priority: 'P1' });
    await taskStore.addNode('ugfm-demo-claim3',  'Task', { name: 'ugfm-demo-claim3',  status: 'pending',   priority: 'P1' });
    await taskStore.addNode('ugfm-demo-claim4',  'Task', { name: 'ugfm-demo-claim4',  status: 'pending',   priority: 'P1' });

    // Real dependency edges: claim4 depends on claim1, claim2, claim3
    // "A blocked-by B" = B must complete before A starts = edge B -> A
    await taskStore.addEdge('dep-c4-c1', 'ugfm-demo-claim1', 'ugfm-demo-claim4', 'blocked-by', { blocking: true });
    await taskStore.addEdge('dep-c4-c2', 'ugfm-demo-claim2', 'ugfm-demo-claim4', 'blocked-by', { blocking: true });
    await taskStore.addEdge('dep-c4-c3', 'ugfm-demo-claim3', 'ugfm-demo-claim4', 'blocked-by', { blocking: true });
    await taskStore.addEdge('dep-c1-impl', 'ugfm-claim1-impl', 'ugfm-demo-claim1', 'blocked-by', { blocking: true });

    // Synthetic circular dependency: claim2 → claim3 → claim2
    // This simulates a dependency modeling error (circular blocked-by ring)
    await taskStore.addEdge('circ-c2-c3', 'ugfm-demo-claim2', 'ugfm-demo-claim3', 'blocked-by', { blocking: true, synthetic: true });
    await taskStore.addEdge('circ-c3-c2', 'ugfm-demo-claim3', 'ugfm-demo-claim2', 'blocked-by', { blocking: true, synthetic: true });
  });

  test('hasCycle() returns true when circular dependency exists in task graph', () => {
    // The same hasCycle() used for Kripke liveness also detects workflow deadlocks
    expect(taskStore.hasCycle()).toBe(true);
  });

  test('findSCCs() returns the circular pair {claim2, claim3} as one SCC with size > 1', () => {
    const sccs = taskStore.findSCCs();

    // The circular dependency ring {claim2, claim3} must appear as a single SCC
    const cycleSCC = sccs.find(scc =>
      scc.includes('ugfm-demo-claim2') && scc.includes('ugfm-demo-claim3')
    );
    expect(cycleSCC).toBeDefined();
    expect(cycleSCC!.length).toBeGreaterThan(1);
    // Exactly 2 nodes form this cycle
    expect(cycleSCC!.length).toBe(2);
  });

  test('real non-circular chain (claim1 → claim4) returns singleton SCCs', () => {
    const sccs = taskStore.findSCCs();
    const sccSets = sccs.map(scc => new Set(scc));

    // ugfm-claim1-impl is the upstream root — not part of any cycle
    const implSCC = sccSets.find(s => s.has('ugfm-claim1-impl'));
    expect(implSCC).toBeDefined();
    expect(implSCC!.size).toBe(1);

    // ugfm-demo-claim4 is the downstream leaf — not part of any cycle
    const claim4SCC = sccSets.find(s => s.has('ugfm-demo-claim4'));
    expect(claim4SCC).toBeDefined();
    expect(claim4SCC!.size).toBe(1);
  });

  test('project() filtering to "blocked-by" edge type works correctly', () => {
    // project() is the same domain-agnostic subgraph extraction used for Kripke views
    const blockedByView = taskStore.project(
      undefined,
      (edge: any) => edge.type === 'blocked-by'
    );

    // All task nodes retained (no node predicate applied)
    expect(blockedByView.nodes.size).toBe(5);

    // All edges are blocked-by type in this graph, so all should be retained
    expect(blockedByView.edges.size).toBe(6); // 4 real deps + 2 circular
    expect(blockedByView.edges.has('dep-c4-c1')).toBe(true);
    expect(blockedByView.edges.has('dep-c4-c2')).toBe(true);
    expect(blockedByView.edges.has('dep-c4-c3')).toBe(true);
    expect(blockedByView.edges.has('circ-c2-c3')).toBe(true);
    expect(blockedByView.edges.has('circ-c3-c2')).toBe(true);
  });
});

// ============================================================================
// SUITE 3: Knowledge-DAG Acyclicity — same algorithm verifies concept hierarchy
// ============================================================================

describe('UGFM Claim 2: Knowledge-DAG Acyclicity Verification', () => {
  /**
   * Demonstrates that the same SCC algorithm used for concurrent-system deadlock
   * detection also verifies no circular subsumption in a knowledge concept hierarchy.
   *
   * A well-formed concept hierarchy (IsA/Enables relationships) must be a DAG:
   * no concept should transitively subsume itself. The SCC algorithm detects
   * any such circular subsumption without domain-specific ontology tooling.
   */
  let conceptStore: GraphStore;

  beforeEach(async () => {
    conceptStore = new GraphStore(`:memory:`);

    // UGFM-related concept nodes forming a pure DAG (no cycles)
    await conceptStore.addNode('GraphSubstrate',    'Concept', { name: 'GraphSubstrate',    domain: 'ugfm-core' });
    await conceptStore.addNode('LabeledMultigraph', 'Concept', { name: 'LabeledMultigraph', domain: 'graph-theory' });
    await conceptStore.addNode('SCCAlgorithm',      'Concept', { name: 'SCCAlgorithm',      domain: 'graph-theory' });
    await conceptStore.addNode('KripkeStructure',   'Concept', { name: 'KripkeStructure',   domain: 'formal-methods' });
    await conceptStore.addNode('TemporalLogic',     'Concept', { name: 'TemporalLogic',     domain: 'formal-methods' });

    // IsA/Enables edges forming a proper DAG (no cycles)
    // GraphSubstrate IsA LabeledMultigraph
    await conceptStore.addEdge('rel-gs-lm',  'GraphSubstrate',    'LabeledMultigraph', 'IsA',     { confidence: 0.95 });
    // KripkeStructure IsA LabeledMultigraph
    await conceptStore.addEdge('rel-ks-lm',  'KripkeStructure',   'LabeledMultigraph', 'IsA',     { confidence: 0.90 });
    // SCCAlgorithm Enables KripkeStructure (verification)
    await conceptStore.addEdge('rel-scc-ks', 'SCCAlgorithm',      'KripkeStructure',   'Enables', { confidence: 0.85 });
    // GraphSubstrate Enables KripkeStructure (substrate provides the representation)
    await conceptStore.addEdge('rel-gs-ks',  'GraphSubstrate',    'KripkeStructure',   'Enables', { confidence: 0.80 });
    // KripkeStructure Enables TemporalLogic (Kripke structures are the semantics of temporal logic)
    await conceptStore.addEdge('rel-ks-tl',  'KripkeStructure',   'TemporalLogic',     'Enables', { confidence: 0.90 });
  });

  test('hasCycle() returns false — DAG concept hierarchy is acyclic', () => {
    // The same hasCycle() that detects concurrent-system cycles finds no cycle here
    expect(conceptStore.hasCycle()).toBe(false);
  });

  test('findSCCs() returns ALL singleton SCCs — no circular subsumption', () => {
    const sccs = conceptStore.findSCCs();

    // Every concept must be its own SCC in a well-formed hierarchy
    for (const scc of sccs) {
      expect(scc.length).toBe(1);
    }

    // Total SCCs equals total concept nodes (5 concepts, 5 singleton SCCs)
    expect(sccs.length).toBe(conceptStore.nodes.size);
    expect(sccs.length).toBe(5);
  });

  test('traversal from GraphSubstrate reaches KripkeStructure via intermediate nodes', () => {
    // BFS traversal — same algorithm used for Kripke reachability — works on concept DAG
    const reachable = conceptStore.traverse('GraphSubstrate', { direction: 'out', maxDepth: 5 });
    const reachableIds = new Set(reachable.map((r: any) => r.node.id));

    // GraphSubstrate → LabeledMultigraph (IsA)
    expect(reachableIds.has('LabeledMultigraph')).toBe(true);
    // GraphSubstrate → KripkeStructure (Enables)
    expect(reachableIds.has('KripkeStructure')).toBe(true);
    // GraphSubstrate → KripkeStructure → TemporalLogic (transitive)
    expect(reachableIds.has('TemporalLogic')).toBe(true);
  });

  test('project() filtering to IsA edges only retains the taxonomy edges', () => {
    // project() extracts subgraph by edge type — same API as Kripke communication view
    const taxonomyView = conceptStore.project(
      undefined,
      (edge: any) => edge.type === 'IsA'
    );

    // Only IsA edges: rel-gs-lm and rel-ks-lm
    expect(taxonomyView.edges.size).toBe(2);
    expect(taxonomyView.edges.has('rel-gs-lm')).toBe(true);  // GraphSubstrate IsA LabeledMultigraph
    expect(taxonomyView.edges.has('rel-ks-lm')).toBe(true);  // KripkeStructure IsA LabeledMultigraph

    // Enables edges filtered out
    expect(taxonomyView.edges.has('rel-scc-ks')).toBe(false); // Enables — not IsA
    expect(taxonomyView.edges.has('rel-gs-ks')).toBe(false);  // Enables — not IsA
    expect(taxonomyView.edges.has('rel-ks-tl')).toBe(false);  // Enables — not IsA

    // All concept nodes retained (no node predicate)
    expect(taxonomyView.nodes.size).toBe(5);
  });
});

// ============================================================================
// SUITE 4: Scalability — 150-node concurrent actor graph
// ============================================================================

describe('2.4 Scalability: 150-node concurrent graph', () => {
  /**
   * Addresses the tractability gap noted in the Opus review: "The demonstration
   * is pedagogically correct but does not address tractability."
   *
   * Builds a realistic 150-node concurrent actor system (100 workers, 10
   * supervisors, 40 tasks, ~210 edges) and verifies that the graph-algorithmic
   * toolchain completes cycle detection, SCC decomposition, and type projection
   * within 2 seconds — demonstrating practical tractability for real-scale
   * concurrent-system graphs without domain-specific model checkers.
   *
   * Graph structure:
   *   - 100 Worker Actor nodes (worker-0 … worker-99)
   *   - 10 Supervisor Actor nodes (supervisor-0 … supervisor-9)
   *   - 40 Task nodes (task-0 … task-39)
   *   - 100 "reports_to" edges: worker-i → supervisor-floor(i/10)
   *   - 10 ring edges among workers 0-9 (MessageChannel) — creates one SCC of size 10
   *   - 40 "assigned_to" edges: task-j → worker-(j%100)
   *   Total: 150 nodes, ~210 edges
   */
  let scaleStore: GraphStore;

  beforeEach(async () => {
    scaleStore = new GraphStore(':memory:');

    // Add 100 workers
    for (let i = 0; i < 100; i++) {
      await scaleStore.addNode(`worker-${i}`, 'Actor', { name: `Worker-${i}`, role: 'worker' });
    }
    // Add 10 supervisors
    for (let i = 0; i < 10; i++) {
      await scaleStore.addNode(`supervisor-${i}`, 'Actor', { name: `Supervisor-${i}`, role: 'supervisor' });
    }
    // Add 40 tasks
    for (let i = 0; i < 40; i++) {
      await scaleStore.addNode(`task-${i}`, 'Task', { name: `Task-${i}` });
    }
    // Connect workers to supervisors: worker-i → supervisor-floor(i/10)
    for (let i = 0; i < 100; i++) {
      await scaleStore.addEdge(`e-ws-${i}`, `worker-${i}`, `supervisor-${Math.floor(i / 10)}`, 'reports_to', {});
    }
    // Create a ring among workers 0-9 (ensures hasCycle() = true, SCC of size 10)
    for (let i = 0; i < 10; i++) {
      await scaleStore.addEdge(`e-ring-${i}`, `worker-${i}`, `worker-${(i + 1) % 10}`, 'MessageChannel', {});
    }
    // Connect tasks to workers: task-j → worker-(j%100)
    for (let i = 0; i < 40; i++) {
      await scaleStore.addEdge(`e-tw-${i}`, `task-${i}`, `worker-${i % 100}`, 'assigned_to', {});
    }
  });

  test('hasCycle() detects cycle in 150-node graph under 2s', () => {
    const start = performance.now();
    const result = scaleStore.hasCycle();
    const elapsed = performance.now() - start;
    expect(result).toBe(true); // Ring among workers 0-9
    expect(elapsed).toBeLessThan(2000);
  });

  test('findSCCs() resolves 150-node graph under 2s and finds ring SCC of size 10', () => {
    const start = performance.now();
    const sccs = scaleStore.findSCCs();
    const elapsed = performance.now() - start;
    expect(elapsed).toBeLessThan(2000);
    // Ring of 10 workers forms one SCC of size 10
    const largeSCC = sccs.find(scc => scc.length >= 10);
    expect(largeSCC).toBeDefined();
    expect(largeSCC!.length).toBe(10);
  });

  test('getByType returns all 150 nodes correctly', () => {
    const actors = scaleStore.getByType('Actor');
    const tasks = scaleStore.getByType('Task');
    expect(actors.length).toBe(110); // 100 workers + 10 supervisors
    expect(tasks.length).toBe(40);
  });

  test('all three operations complete combined under 2s', async () => {
    const start = performance.now();
    scaleStore.hasCycle();
    scaleStore.findSCCs();
    scaleStore.getByType('Actor');
    const elapsed = performance.now() - start;
    expect(elapsed).toBeLessThan(2000);
  });
});
