/**
 * UGFM Claim 1 Demonstration: Multi-Domain Substrate
 *
 * UGFM Claim 1: A single labeled multigraph (nodes + typed edges) provides
 * a sufficiently faithful representation of multiple domain types using the
 * same API surface — without domain-specific storage, query languages, or
 * data models.
 *
 * This test demonstrates three distinct domain types, each modeled in its
 * own ugs GraphStore instance using identical API calls:
 *
 *   Domain A — Concurrent/distributed systems: Actor communication topology
 *   Domain B — Workflow/coordination:          Task dependency graph
 *   Domain C — Knowledge/information:          UGFM concept graph
 *
 * The key assertion: addNode / addEdge / traverse / findSCCs / project work
 * identically across all three domains. No domain-specific code is required.
 *
 * Evidence: ev-022 (π-calculus→LTS), ev-083 (knowledge graphs), ev-170 (Petri nets),
 *           ev-039 (workflow reachability), ev-197 (10/15 formal method areas).
 *
 * Domains NOT yet covered by agentic-primer code:
 *   - Physical/complex systems (no code exists; graph substrate applies in theory)
 *   - ML/neural networks (no code exists; computation DAG representation applies)
 *   - Numerical methods (no code exists; stencil/mesh graph representation applies)
 * These three gaps are honestly documented — Claim 1 demonstrates breadth, not
 * completeness. The three covered domains represent the systems agentic-primer
 * actually runs.
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import GraphStore from './graph';

// ============================================================================
// DOMAIN A: Concurrent/Distributed Systems — Actor Communication Topology
// ============================================================================

describe('UGFM Claim 1 Domain A: Actor Communication Topology (concurrent/distributed)', () => {
  let actorStore: GraphStore;

  beforeEach(async () => {
    actorStore = new GraphStore(`:memory:`);
    // Note: :memory: store does not call initialize() — adjacency maps are ready.
    // We initialize() only for persistence-backed stores.

    // Actors drawn from agentic-primer's actual actor system roles
    // (packages/actors/src/actor-system.ts, router.ts)
    await actorStore.addNode('orchestrator', 'Actor', { name: 'orchestrator', role: 'orchestrator' });
    await actorStore.addNode('router',       'Actor', { name: 'router',       role: 'router' });
    await actorStore.addNode('logger',       'Actor', { name: 'logger',       role: 'logger' });
    await actorStore.addNode('worker-1',     'Actor', { name: 'worker-1',     role: 'worker' });
    await actorStore.addNode('worker-2',     'Actor', { name: 'worker-2',     role: 'worker' });
    await actorStore.addNode('supervisor',   'Actor', { name: 'supervisor',   role: 'supervisor' });

    // Message channels — typed directed edges representing actor-to-actor messaging
    // orchestrator -> router: dispatch commands
    await actorStore.addEdge('ch-orch-router',    'orchestrator', 'router',     'MessageChannel', { messageType: 'dispatch',        pattern: 'tell' });
    // router -> worker-1: assign tasks
    await actorStore.addEdge('ch-router-w1',      'router',       'worker-1',   'MessageChannel', { messageType: 'task-assign',     pattern: 'ask' });
    // router -> worker-2: assign tasks
    await actorStore.addEdge('ch-router-w2',      'router',       'worker-2',   'MessageChannel', { messageType: 'task-assign',     pattern: 'ask' });
    // worker-1 -> orchestrator: report results (ask pattern creates bidirectional flow)
    await actorStore.addEdge('ch-w1-orch',        'worker-1',     'orchestrator', 'MessageChannel', { messageType: 'task-result',   pattern: 'ask' });
    // worker-2 -> orchestrator: report results
    await actorStore.addEdge('ch-w2-orch',        'worker-2',     'orchestrator', 'MessageChannel', { messageType: 'task-result',   pattern: 'ask' });
    // All actors -> logger: log messages (fire-and-forget, tell pattern)
    await actorStore.addEdge('ch-orch-log',    'orchestrator', 'logger',   'MessageChannel', { messageType: 'log.info',   pattern: 'tell' });
    await actorStore.addEdge('ch-router-log',  'router',       'logger',   'MessageChannel', { messageType: 'log.debug',  pattern: 'tell' });
    await actorStore.addEdge('ch-w1-log',      'worker-1',     'logger',   'MessageChannel', { messageType: 'log.info',   pattern: 'tell' });
    await actorStore.addEdge('ch-w2-log',      'worker-2',     'logger',   'MessageChannel', { messageType: 'log.info',   pattern: 'tell' });
    // supervisor -> worker-1/2: supervision (restart/stop directives)
    await actorStore.addEdge('ch-sup-w1',      'supervisor',   'worker-1', 'MessageChannel', { messageType: 'ACTOR_FAILURE', pattern: 'tell' });
    await actorStore.addEdge('ch-sup-w2',      'supervisor',   'worker-2', 'MessageChannel', { messageType: 'ACTOR_FAILURE', pattern: 'tell' });
    // orchestrator -> supervisor: escalate critical failures
    await actorStore.addEdge('ch-orch-sup',    'orchestrator', 'supervisor', 'MessageChannel', { messageType: 'escalate', pattern: 'tell' });
  });

  test('addNode API: actor nodes stored with correct type and properties', async () => {
    const actors = actorStore.getByType('Actor');
    expect(actors.length).toBe(6);

    const orchestrator = actorStore.nodes.get('orchestrator');
    expect(orchestrator).toBeDefined();
    expect(orchestrator!.type).toBe('Actor');
    expect(orchestrator!.properties.get('role')).toBe('orchestrator');
  });

  test('addEdge API: message channels stored with correct type and properties', async () => {
    const edge = actorStore.edges.get('ch-orch-router');
    expect(edge).toBeDefined();
    expect(edge!.type).toBe('MessageChannel');
    expect(edge!.properties.get('messageType')).toBe('dispatch');
    expect(edge!.properties.get('pattern')).toBe('tell');
  });

  test('traverse API: BFS from orchestrator reaches all connected actors', async () => {
    const reachable = actorStore.traverse('orchestrator', { direction: 'out', maxDepth: 10 });
    const reachableIds = new Set(reachable.map(r => r.node.id));

    // orchestrator -> router -> worker-1, worker-2
    expect(reachableIds.has('router')).toBe(true);
    expect(reachableIds.has('worker-1')).toBe(true);
    expect(reachableIds.has('worker-2')).toBe(true);
    expect(reachableIds.has('logger')).toBe(true);
    expect(reachableIds.has('supervisor')).toBe(true);
  });

  test('findSCCs: detects bidirectional messaging cycle (orchestrator <-> workers)', () => {
    // orchestrator->router->worker-1->orchestrator forms a cycle via task assignments + results
    // orchestrator->router->worker-2->orchestrator same
    const sccs = actorStore.findSCCs();

    // At least one SCC should have size > 1 (the orchestrator<->worker cycle)
    const nonSingletons = sccs.filter(scc => scc.length > 1);
    expect(nonSingletons.length).toBeGreaterThan(0);

    // The cycle SCC should contain orchestrator and at least one worker
    const cycleSCC = nonSingletons.find(scc =>
      scc.includes('orchestrator') && (scc.includes('worker-1') || scc.includes('worker-2'))
    );
    expect(cycleSCC).toBeDefined();
  });

  test('project API: filters to ask-pattern channels only', async () => {
    const askView = actorStore.project(
      undefined,
      edge => edge.properties.get('pattern') === 'ask'
    );

    // Only ask-pattern edges: ch-router-w1, ch-router-w2, ch-w1-orch, ch-w2-orch
    expect(askView.edges.size).toBe(4);
    expect(askView.edges.has('ch-orch-router')).toBe(false); // tell pattern
    expect(askView.edges.has('ch-router-w1')).toBe(true);   // ask pattern
    expect(askView.edges.has('ch-w1-orch')).toBe(true);     // ask pattern
  });

  test('hasCycle: actor graph has messaging cycles (expected for request-response actors)', () => {
    expect(actorStore.hasCycle()).toBe(true);
  });
});

// ============================================================================
// DOMAIN B: Workflow/Coordination — Task Dependency Graph
// ============================================================================

describe('UGFM Claim 1 Domain B: Task Dependency Graph (workflow/coordination)', () => {
  let taskStore: GraphStore;

  beforeEach(async () => {
    taskStore = new GraphStore(`:memory:`);

    // Model a subset of actual ugfm synthesis tasks (mirroring the real task graph)
    await taskStore.addNode('ugfm-synthesis',    'Task', { name: 'ugfm-synthesis',    status: 'completed', priority: 'P1' });
    await taskStore.addNode('ugfm-synth-001',    'Task', { name: 'ugfm-synth-001',    status: 'completed', priority: 'P1' });
    await taskStore.addNode('ugfm-synth-002',    'Task', { name: 'ugfm-synth-002',    status: 'completed', priority: 'P1' });
    await taskStore.addNode('ugfm-synth-004',    'Task', { name: 'ugfm-synth-004',    status: 'completed', priority: 'P1' });
    await taskStore.addNode('ugfm-synth-005',    'Task', { name: 'ugfm-synth-005',    status: 'completed', priority: 'P2' });
    await taskStore.addNode('ugfm-demo-claim1',  'Task', { name: 'ugfm-demo-claim1',  status: 'pending',   priority: 'P1' });
    await taskStore.addNode('ugfm-demo-claim2',  'Task', { name: 'ugfm-demo-claim2',  status: 'pending',   priority: 'P1' });
    await taskStore.addNode('ugfm-demo-claim3',  'Task', { name: 'ugfm-demo-claim3',  status: 'pending',   priority: 'P1' });
    await taskStore.addNode('ugfm-demo-claim4',  'Task', { name: 'ugfm-demo-claim4',  status: 'pending',   priority: 'P1' });

    // Dependency edges: "B depends on A" = A->B (A must complete before B starts)
    await taskStore.addEdge('dep-synth-001',  'ugfm-synthesis',   'ugfm-synth-001',   'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-synth-002',  'ugfm-synth-001',   'ugfm-synth-002',   'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-synth-004',  'ugfm-synth-002',   'ugfm-synth-004',   'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-synth-005',  'ugfm-synth-004',   'ugfm-synth-005',   'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-demo1',      'ugfm-synth-005',   'ugfm-demo-claim1', 'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-demo2',      'ugfm-synth-005',   'ugfm-demo-claim2', 'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-demo3',      'ugfm-synth-005',   'ugfm-demo-claim3', 'DependsOn', { blocking: false });
    await taskStore.addEdge('dep-demo4-1',    'ugfm-demo-claim1', 'ugfm-demo-claim4', 'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-demo4-2',    'ugfm-demo-claim2', 'ugfm-demo-claim4', 'DependsOn', { blocking: true });
    await taskStore.addEdge('dep-demo4-3',    'ugfm-demo-claim3', 'ugfm-demo-claim4', 'DependsOn', { blocking: true });

    // Introduce a CIRCULAR DEPENDENCY to test cycle detection
    // (simulating a dependency modeling error: claim4 -> claim1 creates a ring)
    await taskStore.addNode('bad-dep-test-A', 'Task', { name: 'bad-dep-test-A', status: 'pending', priority: 'P3' });
    await taskStore.addNode('bad-dep-test-B', 'Task', { name: 'bad-dep-test-B', status: 'pending', priority: 'P3' });
    await taskStore.addNode('bad-dep-test-C', 'Task', { name: 'bad-dep-test-C', status: 'pending', priority: 'P3' });
    await taskStore.addEdge('circ-AB', 'bad-dep-test-A', 'bad-dep-test-B', 'DependsOn', { blocking: true });
    await taskStore.addEdge('circ-BC', 'bad-dep-test-B', 'bad-dep-test-C', 'DependsOn', { blocking: true });
    await taskStore.addEdge('circ-CA', 'bad-dep-test-C', 'bad-dep-test-A', 'DependsOn', { blocking: true }); // circular!
  });

  test('addNode API: task nodes stored with correct type and lifecycle properties', async () => {
    const tasks = taskStore.getByType('Task');
    expect(tasks.length).toBe(12); // 9 real + 3 circular test tasks

    const demo1 = taskStore.nodes.get('ugfm-demo-claim1');
    expect(demo1).toBeDefined();
    expect(demo1!.type).toBe('Task');
    expect(demo1!.properties.get('status')).toBe('pending');
    expect(demo1!.properties.get('priority')).toBe('P1');
  });

  test('traverse API: all demo tasks reachable from ugfm-synthesis root', async () => {
    const reachable = taskStore.traverse('ugfm-synthesis', { direction: 'out', maxDepth: 20 });
    const ids = new Set(reachable.map(r => r.node.id));

    expect(ids.has('ugfm-synth-001')).toBe(true);
    expect(ids.has('ugfm-synth-004')).toBe(true);
    expect(ids.has('ugfm-demo-claim1')).toBe(true);
    expect(ids.has('ugfm-demo-claim4')).toBe(true);
  });

  test('findSCCs: detects the circular dependency ring {A,B,C}', () => {
    // The same findSCCs algorithm used for deadlock detection in Kripke structures
    // also detects circular dependencies in task/workflow graphs.
    const sccs = taskStore.findSCCs();

    const cycleSCC = sccs.find(scc =>
      scc.includes('bad-dep-test-A') &&
      scc.includes('bad-dep-test-B') &&
      scc.includes('bad-dep-test-C')
    );
    expect(cycleSCC).toBeDefined();
    expect(cycleSCC!.length).toBe(3);
  });

  test('findSCCs: real task nodes are singletons (no cycles in valid dependency chain)', () => {
    const sccs = taskStore.findSCCs();
    const sccSets = sccs.map(scc => new Set(scc));

    // ugfm-synthesis is not in any cycle — it is the root
    const synthSCC = sccSets.find(s => s.has('ugfm-synthesis'));
    expect(synthSCC).toBeDefined();
    expect(synthSCC!.size).toBe(1);

    // ugfm-demo-claim4 is not in any cycle
    const claim4SCC = sccSets.find(s => s.has('ugfm-demo-claim4'));
    expect(claim4SCC).toBeDefined();
    expect(claim4SCC!.size).toBe(1);
  });

  test('hasCycle: overall task graph has cycles (due to the bad circular dependency)', () => {
    expect(taskStore.hasCycle()).toBe(true);
  });

  test('project API: extract only blocking dependencies', async () => {
    const blockingView = taskStore.project(
      undefined,
      edge => edge.properties.get('blocking') === true
    );

    // Non-blocking edge: dep-demo3 (ugfm-synth-005 -> ugfm-demo-claim3, blocking:false)
    expect(blockingView.edges.has('dep-demo3')).toBe(false);
    // Blocking edges are retained
    expect(blockingView.edges.has('dep-synth-001')).toBe(true);
    expect(blockingView.edges.has('dep-demo4-1')).toBe(true);
  });
});

// ============================================================================
// DOMAIN C: Knowledge/Information — UGFM Concept Graph
// ============================================================================

describe('UGFM Claim 1 Domain C: Knowledge Concept Graph (knowledge/information)', () => {
  let knowledgeStore: GraphStore;

  beforeEach(async () => {
    knowledgeStore = new GraphStore(`:memory:`);

    // UGFM concepts modeled as a knowledge graph
    // No cycles — this is a proper concept hierarchy (DAG)
    await knowledgeStore.addNode('GraphSubstrate',          'Concept', { name: 'GraphSubstrate',          domain: 'ugfm-core' });
    await knowledgeStore.addNode('VerificationProperty',    'Concept', { name: 'VerificationProperty',    domain: 'ugfm-core' });
    await knowledgeStore.addNode('GraphAlgorithm',          'Concept', { name: 'GraphAlgorithm',          domain: 'graph-theory' });
    await knowledgeStore.addNode('SCCAlgorithm',            'Concept', { name: 'SCCAlgorithm',            domain: 'graph-theory' });
    await knowledgeStore.addNode('TopologicalSort',         'Concept', { name: 'TopologicalSort',         domain: 'graph-theory' });
    await knowledgeStore.addNode('DeadlockFreedom',         'Concept', { name: 'DeadlockFreedom',         domain: 'concurrent-systems' });
    await knowledgeStore.addNode('DAGConsistency',          'Concept', { name: 'DAGConsistency',          domain: 'knowledge-representation' });
    await knowledgeStore.addNode('LabeledMultigraph',       'Concept', { name: 'LabeledMultigraph',       domain: 'graph-theory' });
    await knowledgeStore.addNode('KripkeStructure',         'Concept', { name: 'KripkeStructure',         domain: 'formal-methods' });
    await knowledgeStore.addNode('TaskDependencyGraph',     'Concept', { name: 'TaskDependencyGraph',     domain: 'workflow' });

    // Relationships — typed edges with confidence scores
    // GraphSubstrate IsA LabeledMultigraph
    await knowledgeStore.addEdge('rel-01', 'GraphSubstrate',       'LabeledMultigraph',    'IsA',       { confidence: 0.95 });
    // KripkeStructure IsA LabeledMultigraph
    await knowledgeStore.addEdge('rel-02', 'KripkeStructure',      'LabeledMultigraph',    'IsA',       { confidence: 0.90 });
    // TaskDependencyGraph IsA LabeledMultigraph
    await knowledgeStore.addEdge('rel-03', 'TaskDependencyGraph',  'LabeledMultigraph',    'IsA',       { confidence: 0.90 });
    // GraphSubstrate Enables VerificationProperty
    await knowledgeStore.addEdge('rel-04', 'GraphSubstrate',       'VerificationProperty', 'Enables',   { confidence: 0.80 });
    // GraphAlgorithm Enables VerificationProperty
    await knowledgeStore.addEdge('rel-05', 'GraphAlgorithm',       'VerificationProperty', 'Enables',   { confidence: 0.85 });
    // SCCAlgorithm IsA GraphAlgorithm
    await knowledgeStore.addEdge('rel-06', 'SCCAlgorithm',         'GraphAlgorithm',       'IsA',       { confidence: 0.99 });
    // TopologicalSort IsA GraphAlgorithm
    await knowledgeStore.addEdge('rel-07', 'TopologicalSort',      'GraphAlgorithm',       'IsA',       { confidence: 0.99 });
    // SCCAlgorithm Enables DeadlockFreedom (detects deadlock via sink SCC check)
    await knowledgeStore.addEdge('rel-08', 'SCCAlgorithm',         'DeadlockFreedom',      'Enables',   { confidence: 0.90 });
    // TopologicalSort Enables DAGConsistency
    await knowledgeStore.addEdge('rel-09', 'TopologicalSort',      'DAGConsistency',       'Enables',   { confidence: 0.95 });
    // SCCAlgorithm Enables DAGConsistency (cycle detection IS DAG consistency check)
    await knowledgeStore.addEdge('rel-10', 'SCCAlgorithm',         'DAGConsistency',       'Enables',   { confidence: 0.90 });
    // GraphSubstrate RelatedTo KripkeStructure
    await knowledgeStore.addEdge('rel-11', 'GraphSubstrate',       'KripkeStructure',      'RelatedTo', { confidence: 0.75 });
    // GraphSubstrate RelatedTo TaskDependencyGraph
    await knowledgeStore.addEdge('rel-12', 'GraphSubstrate',       'TaskDependencyGraph',  'RelatedTo', { confidence: 0.75 });
  });

  test('addNode API: concept nodes stored with correct type and domain properties', async () => {
    const concepts = knowledgeStore.getByType('Concept');
    expect(concepts.length).toBe(10);

    const scc = knowledgeStore.nodes.get('SCCAlgorithm');
    expect(scc).toBeDefined();
    expect(scc!.type).toBe('Concept');
    expect(scc!.properties.get('domain')).toBe('graph-theory');
  });

  test('traverse API: from GraphSubstrate reaches VerificationProperty, KripkeStructure, TaskDependencyGraph', async () => {
    const reachable = knowledgeStore.traverse('GraphSubstrate', { direction: 'out', maxDepth: 5 });
    const ids = new Set(reachable.map(r => r.node.id));

    expect(ids.has('LabeledMultigraph')).toBe(true);
    expect(ids.has('VerificationProperty')).toBe(true);
    expect(ids.has('KripkeStructure')).toBe(true);
    expect(ids.has('TaskDependencyGraph')).toBe(true);
  });

  test('findSCCs: all singleton SCCs — knowledge graph is a proper DAG (no circular subsumption)', () => {
    const sccs = knowledgeStore.findSCCs();

    // Every SCC must be a singleton — no cycles in a well-formed concept hierarchy
    for (const scc of sccs) {
      expect(scc.length).toBe(1);
    }

    // Total SCCs = total nodes (each is its own SCC in a DAG)
    expect(sccs.length).toBe(knowledgeStore.nodes.size);
  });

  test('hasCycle: knowledge DAG has no cycles (consistent concept hierarchy)', () => {
    expect(knowledgeStore.hasCycle()).toBe(false);
  });

  test('project API: extract only high-confidence relationships (confidence >= 0.90)', async () => {
    const highConfView = knowledgeStore.project(
      undefined,
      edge => (edge.properties.get('confidence') ?? 0) >= 0.90
    );

    // Low confidence edges excluded: rel-04 (0.80), rel-11 (0.75), rel-12 (0.75)
    expect(highConfView.edges.has('rel-04')).toBe(false);
    expect(highConfView.edges.has('rel-11')).toBe(false);
    expect(highConfView.edges.has('rel-12')).toBe(false);

    // High confidence edges retained: rel-01 (0.95), rel-06 (0.99), rel-09 (0.95), etc.
    expect(highConfView.edges.has('rel-01')).toBe(true);
    expect(highConfView.edges.has('rel-06')).toBe(true);
    expect(highConfView.edges.has('rel-07')).toBe(true);
    expect(highConfView.edges.has('rel-09')).toBe(true);
  });

  test('project API: extract IsA relationships only (concept hierarchy view)', async () => {
    const hierarchyView = knowledgeStore.project(
      undefined,
      edge => edge.type === 'IsA'
    );

    // Only IsA edges: rel-01, rel-02, rel-03, rel-06, rel-07
    expect(hierarchyView.edges.size).toBe(5);
    expect(hierarchyView.edges.has('rel-01')).toBe(true); // GraphSubstrate IsA LabeledMultigraph
    expect(hierarchyView.edges.has('rel-06')).toBe(true); // SCCAlgorithm IsA GraphAlgorithm
    expect(hierarchyView.edges.has('rel-05')).toBe(false); // Enables — not IsA
  });
});

// ============================================================================
// CROSS-DOMAIN ASSERTION: Same API, Three Domains
// ============================================================================

describe('UGFM Claim 1: Cross-Domain API Identity', () => {
  /**
   * This test group proves the core Claim 1 assertion: the same five API calls
   * work identically across all three domain types. No domain-specific code paths.
   *
   * The function below is the "shared toolchain" — it takes any GraphStore and
   * runs the same operations regardless of what domain the store models.
   */
  function runUniversalToolchain(store: GraphStore, startNodeId: string, nodeType: string) {
    // API call 1: getByType — returns domain nodes by their type label
    const domainNodes = store.getByType(nodeType);

    // API call 2: traverse — BFS from a start node (works for actors, tasks, concepts)
    const reachable = store.traverse(startNodeId, { direction: 'out', maxDepth: 20 });

    // API call 3: findSCCs — detects cycles in any domain graph
    const sccs = store.findSCCs();
    const nonSingletonSCCs = sccs.filter(scc => scc.length > 1);
    const hasCycles = nonSingletonSCCs.length > 0;

    // API call 4: project — filter to any subset (here: all nodes, no predicate)
    const projection = store.project(n => n.type === nodeType);

    return {
      nodeCount: domainNodes.length,
      reachableCount: reachable.length,
      sccCount: sccs.length,
      hasCycles,
      projectedNodeCount: projection.nodes.size,
    };
  }

  test('universal toolchain runs identically on actor graph (Domain A)', async () => {
    const store = new GraphStore(`:memory:`);
    await store.addNode('orch',  'Actor', { name: 'orchestrator', role: 'orchestrator' });
    await store.addNode('rtr',   'Actor', { name: 'router',       role: 'router' });
    await store.addNode('wkr',   'Actor', { name: 'worker',       role: 'worker' });
    await store.addEdge('e1', 'orch', 'rtr',  'MessageChannel', { messageType: 'dispatch' });
    await store.addEdge('e2', 'rtr',  'wkr',  'MessageChannel', { messageType: 'task-assign' });
    await store.addEdge('e3', 'wkr',  'orch', 'MessageChannel', { messageType: 'task-result' }); // cycle

    const result = runUniversalToolchain(store, 'orch', 'Actor');
    expect(result.nodeCount).toBe(3);
    expect(result.reachableCount).toBeGreaterThan(0);
    expect(result.hasCycles).toBe(true); // request-response cycle
    expect(result.projectedNodeCount).toBe(3);
  });

  test('universal toolchain runs identically on task graph (Domain B)', async () => {
    const store = new GraphStore(`:memory:`);
    await store.addNode('t-root', 'Task', { name: 'root',   status: 'completed', priority: 'P1' });
    await store.addNode('t-mid',  'Task', { name: 'middle', status: 'pending',   priority: 'P1' });
    await store.addNode('t-leaf', 'Task', { name: 'leaf',   status: 'pending',   priority: 'P2' });
    await store.addEdge('d1', 't-root', 't-mid',  'DependsOn', { blocking: true });
    await store.addEdge('d2', 't-mid',  't-leaf', 'DependsOn', { blocking: true });
    // No cycle — valid dependency chain

    const result = runUniversalToolchain(store, 't-root', 'Task');
    expect(result.nodeCount).toBe(3);
    expect(result.reachableCount).toBeGreaterThan(0);
    expect(result.hasCycles).toBe(false); // valid DAG — no circular dependencies
    expect(result.projectedNodeCount).toBe(3);
  });

  test('universal toolchain runs identically on knowledge graph (Domain C)', async () => {
    const store = new GraphStore(`:memory:`);
    await store.addNode('c-graph',  'Concept', { name: 'GraphSubstrate',       domain: 'ugfm-core' });
    await store.addNode('c-verify', 'Concept', { name: 'VerificationProperty', domain: 'ugfm-core' });
    await store.addNode('c-algo',   'Concept', { name: 'GraphAlgorithm',       domain: 'graph-theory' });
    await store.addEdge('r1', 'c-graph',  'c-verify', 'Enables', { confidence: 0.80 });
    await store.addEdge('r2', 'c-algo',   'c-verify', 'Enables', { confidence: 0.85 });
    // No cycle — proper concept hierarchy

    const result = runUniversalToolchain(store, 'c-graph', 'Concept');
    expect(result.nodeCount).toBe(3);
    expect(result.hasCycles).toBe(false); // concept DAG — no circular subsumption
    expect(result.projectedNodeCount).toBe(3);
  });

  test('cycle detection result differs correctly across domains with/without cycles', async () => {
    // Domain with cycle (actor graph with bidirectional messaging)
    const cycleStore = new GraphStore(`:memory:`);
    await cycleStore.addNode('a', 'Actor', { name: 'a', role: 'orchestrator' });
    await cycleStore.addNode('b', 'Actor', { name: 'b', role: 'worker' });
    await cycleStore.addEdge('e1', 'a', 'b', 'MessageChannel', { messageType: 'send' });
    await cycleStore.addEdge('e2', 'b', 'a', 'MessageChannel', { messageType: 'reply' });

    // Domain without cycle (knowledge concept DAG)
    const dagStore = new GraphStore(`:memory:`);
    await dagStore.addNode('parent', 'Concept', { name: 'parent', domain: 'd' });
    await dagStore.addNode('child',  'Concept', { name: 'child',  domain: 'd' });
    await dagStore.addEdge('r1', 'parent', 'child', 'IsA', { confidence: 0.99 });

    const cycleResult = runUniversalToolchain(cycleStore, 'a', 'Actor');
    const dagResult   = runUniversalToolchain(dagStore, 'parent', 'Concept');

    // The SAME algorithm produces correct results for both domain types
    expect(cycleResult.hasCycles).toBe(true);
    expect(dagResult.hasCycles).toBe(false);
  });

  test('getByType acts as domain lens — same store, different domain views', async () => {
    // A mixed store with multiple domain types (heterogeneous substrate)
    const mixedStore = new GraphStore(`:memory:`);

    // Domain A nodes
    await mixedStore.addNode('actor-orch',   'Actor',   { name: 'orchestrator', role: 'orchestrator' });
    await mixedStore.addNode('actor-worker', 'Actor',   { name: 'worker',       role: 'worker' });

    // Domain B nodes
    await mixedStore.addNode('task-1', 'Task', { name: 'task-1', status: 'pending', priority: 'P1' });
    await mixedStore.addNode('task-2', 'Task', { name: 'task-2', status: 'pending', priority: 'P2' });

    // Domain C nodes
    await mixedStore.addNode('concept-1', 'Concept', { name: 'GraphSubstrate', domain: 'ugfm' });

    // Cross-domain: a task depends on an actor completing its work
    await mixedStore.addEdge('cross-1', 'actor-orch', 'task-1', 'Triggers', { reason: 'actor-completes' });

    // getByType acts as a domain lens on the shared substrate
    const actors   = mixedStore.getByType('Actor');
    const tasks    = mixedStore.getByType('Task');
    const concepts = mixedStore.getByType('Concept');

    expect(actors.length).toBe(2);
    expect(tasks.length).toBe(2);
    expect(concepts.length).toBe(1);

    // All 5 nodes coexist in one substrate
    expect(mixedStore.nodes.size).toBe(5);
  });
});
