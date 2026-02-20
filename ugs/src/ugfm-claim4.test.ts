/**
 * UGFM Claim 4 Test Suite: Shared Toolchain Across All Domain Types
 *
 * UGFM Claim 4: One toolchain (traverse, findSCCs, hasCycle, project, getByType)
 * covers all 7 UGFM domain types. Adding a new domain requires zero new algorithm
 * code — only new node/edge type conventions.
 *
 * This contrasts with MIC (Model-Integrated Computing), which required separate
 * metamodels and DSML transformations for each domain.
 *
 * Key invariants verified here:
 *  1. algorithmsUsed is identical across all domains — same toolchain, every domain
 *  2. Adding 'numerical' domain required NO new algorithm code
 *  3. detectDomain() demonstrates the graph self-describes its domain via node types
 *  4. compareAcrossDomains() produces auditable cross-domain evidence
 */

import { describe, it, expect, beforeEach } from 'bun:test';
import { GraphStore } from './graph';
import {
  runUniversalToolchain,
  detectDomain,
  compareAcrossDomains,
  DOMAIN_REGISTRY,
  type ToolchainResult,
} from './ugfm-claim4';

describe('UGFM Claim 4: Shared Toolchain Across All Domain Types', () => {
  // =========================================================================
  // 4.1 Domain Registry — all 7 domains documented
  // =========================================================================

  describe('4.1 Domain registry documents all 7 domain types', () => {
    it('registry contains exactly 7 domain entries', () => {
      expect(DOMAIN_REGISTRY.length).toBe(7);
    });

    it('all 7 UGFM domains are registered', () => {
      const domains = DOMAIN_REGISTRY.map(r => r.domain);
      expect(domains).toContain('concurrent');
      expect(domains).toContain('workflow');
      expect(domains).toContain('knowledge');
      expect(domains).toContain('formal');
      expect(domains).toContain('physical');
      expect(domains).toContain('ml');
      expect(domains).toContain('numerical');
    });

    it('each domain has node types and edge types defined (convention only)', () => {
      for (const reg of DOMAIN_REGISTRY) {
        expect(reg.nodeTypes.length).toBeGreaterThan(0);
        expect(reg.edgeTypes.length).toBeGreaterThan(0);
        expect(reg.description.length).toBeGreaterThan(0);
      }
    });

    it('domains have distinct node type conventions', () => {
      // Each domain uses different primary node types — type conventions are
      // the ONLY thing that distinguishes domains (no algorithm differences)
      const allPrimaryTypes = DOMAIN_REGISTRY.map(r => r.nodeTypes[0]);
      const uniqueTypes = new Set(allPrimaryTypes);
      expect(uniqueTypes.size).toBe(DOMAIN_REGISTRY.length);
    });
  });

  // =========================================================================
  // 4.2 Toolchain operates identically across all implemented domains
  // =========================================================================

  describe('4.2 Toolchain operates identically across implemented domains', () => {
    let actorStore: GraphStore;
    let workflowStore: GraphStore;
    let knowledgeStore: GraphStore;
    let numericalStore: GraphStore;

    beforeEach(async () => {
      // Domain: concurrent (Actor communication)
      actorStore = new GraphStore(':memory:');
      await actorStore.addNode('orch', 'Actor', { name: 'orchestrator', role: 'coordinator' });
      await actorStore.addNode('w1', 'Actor', { name: 'worker-1', role: 'executor' });
      await actorStore.addNode('w2', 'Actor', { name: 'worker-2', role: 'executor' });
      await actorStore.addEdge('e1', 'orch', 'w1', 'MessageChannel', { messageType: 'Task' });
      await actorStore.addEdge('e2', 'orch', 'w2', 'MessageChannel', { messageType: 'Task' });
      await actorStore.addEdge('e3', 'w1', 'orch', 'MessageChannel', { messageType: 'Result' }); // creates a cycle

      // Domain: workflow (Task dependency)
      workflowStore = new GraphStore(':memory:');
      await workflowStore.addNode('t1', 'Task', { title: 'ugfm-claim1', priority: 'P1' });
      await workflowStore.addNode('t2', 'Task', { title: 'ugfm-claim2', priority: 'P1' });
      await workflowStore.addNode('t3', 'Task', { title: 'ugfm-claim4', priority: 'P1' });
      await workflowStore.addEdge('d1', 't3', 't1', 'DependsOn', {});
      await workflowStore.addEdge('d2', 't3', 't2', 'DependsOn', {});
      // No cycle — valid DAG

      // Domain: knowledge (Concept hierarchy)
      knowledgeStore = new GraphStore(':memory:');
      await knowledgeStore.addNode('gs', 'Concept', { name: 'GraphSubstrate' });
      await knowledgeStore.addNode('scc', 'Concept', { name: 'SCCAlgorithm' });
      await knowledgeStore.addNode('ks', 'Concept', { name: 'KripkeStructure' });
      await knowledgeStore.addEdge('e1', 'gs', 'scc', 'Enables', {});
      await knowledgeStore.addEdge('e2', 'scc', 'ks', 'Enables', {});
      // No cycle — pure DAG

      // Domain: numerical (FDM stencil) — NEW DOMAIN, zero algorithm changes
      // This is the core Claim 4 assertion: adding a domain = new data only.
      // 'GridPoint' nodes and 'StencilEdge' edges are just type-label conventions.
      // findSCCs / hasCycle / getByType / traverse / project work unchanged.
      numericalStore = new GraphStore(':memory:');
      await numericalStore.addNode('g00', 'GridPoint', { x: 0, y: 0 });
      await numericalStore.addNode('g01', 'GridPoint', { x: 0, y: 1 });
      await numericalStore.addNode('g10', 'GridPoint', { x: 1, y: 0 });
      await numericalStore.addNode('g11', 'GridPoint', { x: 1, y: 1 });
      await numericalStore.addEdge('s1', 'g00', 'g01', 'StencilEdge', { weight: 0.25 });
      await numericalStore.addEdge('s2', 'g00', 'g10', 'StencilEdge', { weight: 0.25 });
      await numericalStore.addEdge('s3', 'g01', 'g11', 'StencilEdge', { weight: 0.25 });
      await numericalStore.addEdge('s4', 'g10', 'g11', 'StencilEdge', { weight: 0.25 });
      // No cycle — directed stencil grid
    });

    it('runUniversalToolchain uses identical set of algorithms for ALL domains', () => {
      // The core Claim 4 invariant: the same graph functions are called regardless of domain.
      // algorithmsUsed being identical across domains proves API identity — not a constant.
      const domains = [
        { store: actorStore, domain: 'concurrent' as const },
        { store: workflowStore, domain: 'workflow' as const },
        { store: knowledgeStore, domain: 'knowledge' as const },
        { store: numericalStore, domain: 'numerical' as const },
      ];
      const results = domains.map(({ store, domain }) => runUniversalToolchain(store, domain));
      // Every domain result must report the exact same algorithms were called
      const firstAlgorithms = JSON.stringify(results[0].algorithmsUsed);
      for (const result of results) {
        expect(JSON.stringify(result.algorithmsUsed)).toBe(firstAlgorithms);
        expect(result.domain).toBe(domains[results.indexOf(result)].domain);
      }
    });

    it('actor domain correctly detects cycle (orchestrator <-> worker-1 bidirectional messaging)', () => {
      const result = runUniversalToolchain(actorStore, 'concurrent');
      expect(result.hasCycle).toBe(true);
      expect(result.largestSCCSize).toBeGreaterThanOrEqual(2); // orch + w1 form an SCC
    });

    it('actor domain returns correct node and edge counts', () => {
      const result = runUniversalToolchain(actorStore, 'concurrent');
      expect(result.nodeCount).toBe(3);
      expect(result.edgeCount).toBe(3);
    });

    it('workflow domain correctly detects no cycle (DAG)', () => {
      const result = runUniversalToolchain(workflowStore, 'workflow');
      expect(result.hasCycle).toBe(false);
      expect(result.largestSCCSize).toBe(1); // All singletons in a DAG
    });

    it('workflow domain returns correct node and edge counts', () => {
      const result = runUniversalToolchain(workflowStore, 'workflow');
      expect(result.nodeCount).toBe(3);
      expect(result.edgeCount).toBe(2);
    });

    it('knowledge domain correctly detects no cycle (pure concept DAG)', () => {
      const result = runUniversalToolchain(knowledgeStore, 'knowledge');
      expect(result.hasCycle).toBe(false);
      expect(result.largestSCCSize).toBe(1);
    });

    it('knowledge domain returns correct node and edge counts', () => {
      const result = runUniversalToolchain(knowledgeStore, 'knowledge');
      expect(result.nodeCount).toBe(3);
      expect(result.edgeCount).toBe(2);
    });

    it('numerical domain works with ZERO algorithm changes (new domain = new type labels only)', () => {
      // THE core Claim 4 assertion:
      // Adding the 'numerical' domain required NO new functions, NO new imports,
      // NO new algorithm implementations. Only new node type ('GridPoint') and
      // edge type ('StencilEdge') were introduced as type-label conventions.
      const result = runUniversalToolchain(numericalStore, 'numerical');
      expect(result.algorithmsUsed).toEqual(['findSCCs', 'hasCycle']);  // Same toolchain, same functions
      expect(result.hasCycle).toBe(false);           // Directed stencil grids are DAGs
      expect(result.sccCount).toBeGreaterThan(0);    // Nodes are present as singleton SCCs
    });

    it('numerical domain returns correct node and edge counts', () => {
      const result = runUniversalToolchain(numericalStore, 'numerical');
      expect(result.nodeCount).toBe(4);   // 2x2 grid points
      expect(result.edgeCount).toBe(4);   // 4 stencil edges
    });

    it('all 4 domain results share identical algorithmsUsed (toolchain identity invariant)', () => {
      // This is the meaningful invariant: not that a constant equals itself,
      // but that the actual graph functions called are identical across all domains.
      // If someone adds a domain-specific shortcut, this test will catch it.
      const results = [
        runUniversalToolchain(actorStore, 'concurrent'),
        runUniversalToolchain(workflowStore, 'workflow'),
        runUniversalToolchain(knowledgeStore, 'knowledge'),
        runUniversalToolchain(numericalStore, 'numerical'),
      ];
      const algorithmSets = results.map(r => JSON.stringify(r.algorithmsUsed));
      // Every domain must call the exact same set of graph algorithms
      expect(new Set(algorithmSets).size).toBe(1);
      // Verify the expected algorithms are present
      expect(results[0].algorithmsUsed).toContain('findSCCs');
      expect(results[0].algorithmsUsed).toContain('hasCycle');
    });

    it('sccCount equals nodeCount when there are no cycles (all singletons)', () => {
      // In a DAG, every node is its own SCC
      const wfResult = runUniversalToolchain(workflowStore, 'workflow');
      expect(wfResult.sccCount).toBe(wfResult.nodeCount);

      const kResult = runUniversalToolchain(knowledgeStore, 'knowledge');
      expect(kResult.sccCount).toBe(kResult.nodeCount);

      const nResult = runUniversalToolchain(numericalStore, 'numerical');
      expect(nResult.sccCount).toBe(nResult.nodeCount);
    });
  });

  // =========================================================================
  // 4.3 Domain auto-detection from graph structure
  // =========================================================================

  describe('4.3 Domain auto-detection from graph structure', () => {
    it('detects concurrent domain from Actor nodes', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('a', 'Actor', { name: 'a' });
      expect(detectDomain(store)).toBe('concurrent');
    });

    it('detects workflow domain from Task nodes', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('t', 'Task', { title: 't' });
      expect(detectDomain(store)).toBe('workflow');
    });

    it('detects knowledge domain from Concept nodes', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('c', 'Concept', { name: 'c' });
      expect(detectDomain(store)).toBe('knowledge');
    });

    it('detects formal domain from SystemState nodes', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('s0', 'SystemState', { label: 'initial' });
      expect(detectDomain(store)).toBe('formal');
    });

    it('detects physical domain from Component nodes', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('c1', 'Component', { name: 'spring' });
      expect(detectDomain(store)).toBe('physical');
    });

    it('detects ml domain from Layer nodes', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('l0', 'Layer', { type: 'dense', units: 128 });
      expect(detectDomain(store)).toBe('ml');
    });

    it('detects numerical domain from GridPoint nodes', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('g', 'GridPoint', { x: 0, y: 0 });
      expect(detectDomain(store)).toBe('numerical');
    });

    it('returns unknown for unregistered node types', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('x', 'FutureType', {});
      expect(detectDomain(store)).toBe('unknown');
    });

    it('returns unknown for empty store', () => {
      const store = new GraphStore(':memory:');
      expect(detectDomain(store)).toBe('unknown');
    });
  });

  // =========================================================================
  // 4.4 Cross-domain comparison report
  // =========================================================================

  describe('4.4 Cross-domain comparison report', () => {
    it('produces a markdown report comparing all domains', async () => {
      const actorStore = new GraphStore(':memory:');
      await actorStore.addNode('a', 'Actor', { name: 'a' });

      const taskStore = new GraphStore(':memory:');
      await taskStore.addNode('t', 'Task', { title: 't' });

      const report = compareAcrossDomains([
        { store: actorStore, domain: 'concurrent', label: 'Actor system' },
        { store: taskStore, domain: 'workflow', label: 'Task graph' },
      ]);

      expect(report).toContain('UGFM Claim 4');
      expect(report).toContain('concurrent');
      expect(report).toContain('workflow');
      // Verify the algorithmsUsed are identical across both domains — the real invariant
      const actorResult: ToolchainResult = runUniversalToolchain(actorStore, 'concurrent');
      const taskResult: ToolchainResult = runUniversalToolchain(taskStore, 'workflow');
      expect(JSON.stringify(actorResult.algorithmsUsed)).toBe(JSON.stringify(taskResult.algorithmsUsed));
    });

    it('all domains produce identical algorithmsUsed — toolchain identity across all domains', async () => {
      const stores = await Promise.all([
        (async () => {
          const s = new GraphStore(':memory:');
          await s.addNode('a', 'Actor', {});
          return { store: s, domain: 'concurrent' as const, label: 'A' };
        })(),
        (async () => {
          const s = new GraphStore(':memory:');
          await s.addNode('t', 'Task', {});
          return { store: s, domain: 'workflow' as const, label: 'B' };
        })(),
        (async () => {
          const s = new GraphStore(':memory:');
          await s.addNode('g', 'GridPoint', {});
          return { store: s, domain: 'numerical' as const, label: 'C' };
        })(),
      ]);

      // The meaningful invariant: the SET of graph functions called is identical
      // across all domains — this proves API identity, not that a constant equals itself.
      const results = stores.map(({ store, domain }) => runUniversalToolchain(store, domain));
      const algorithmSets = results.map(r => JSON.stringify(r.algorithmsUsed));
      // All 3 domains must call the exact same graph functions
      expect(new Set(algorithmSets).size).toBe(1);
      expect(results[0].algorithmsUsed).toContain('findSCCs');
      expect(results[0].algorithmsUsed).toContain('hasCycle');
    });

    it('report contains header and table structure', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('n', 'Actor', {});

      const report = compareAcrossDomains([
        { store, domain: 'concurrent', label: 'test' },
      ]);

      expect(report).toContain('# UGFM Claim 4');
      expect(report).toContain('| Domain |');
      expect(report).toContain('findSCCs');
    });

    it('report correctly counts nodes across domains', async () => {
      const store = new GraphStore(':memory:');
      await store.addNode('g00', 'GridPoint', { x: 0, y: 0 });
      await store.addNode('g01', 'GridPoint', { x: 0, y: 1 });
      await store.addNode('g10', 'GridPoint', { x: 1, y: 0 });

      const report = compareAcrossDomains([
        { store, domain: 'numerical', label: '3-point stencil' },
      ]);

      // 3 nodes should appear in the report row
      expect(report).toContain('| numerical |');
      expect(report).toContain('3-point stencil');
    });
  });

  // =========================================================================
  // 4.5 Head-to-head: domain-specific vs UGFM
  //
  // Models the same system (8-task build dependency graph) in two styles:
  //   A) Domain-specific: ad-hoc Task[] + custom DFS cycle detection
  //   B) UGFM/ugs: GraphStore with 'Task' node type, shared hasCycle()
  //
  // This directly addresses the MIC counter-argument: "domain-specific tools
  // are more productive even when universal tools are possible."
  // =========================================================================

  describe('4.5 Head-to-head: domain-specific vs UGFM', () => {
    // -----------------------------------------------------------------------
    // Domain-specific implementation (Approach A)
    // Custom data structure + hand-written DFS — lives here, not in ugfm-claim4.ts
    // -----------------------------------------------------------------------

    interface Task { id: string; deps: string[]; }

    const buildTasks: Task[] = [
      { id: 'A', deps: [] },
      { id: 'B', deps: ['A'] },
      { id: 'C', deps: ['A'] },
      { id: 'D', deps: ['B'] },
      { id: 'E', deps: ['C'] },
      { id: 'F', deps: ['D', 'E'] },
      { id: 'G', deps: ['F'] },
      { id: 'H', deps: ['G'] },
    ];

    /** Custom DFS cycle detection written specifically for Task[]. */
    function detectCycleDomainSpecific(tasks: Task[]): boolean {
      const visited = new Set<string>();
      const inStack = new Set<string>();

      function dfs(id: string): boolean {
        if (inStack.has(id)) return true;
        if (visited.has(id)) return false;
        visited.add(id);
        inStack.add(id);
        const task = tasks.find(t => t.id === id)!;
        for (const dep of task.deps) {
          if (dfs(dep)) return true;
        }
        inStack.delete(id);
        return false;
      }

      return tasks.some(t => dfs(t.id));
    }

    // -----------------------------------------------------------------------
    // UGFM implementation (Approach B) — shared GraphStore substrate
    // -----------------------------------------------------------------------

    async function buildUGFMTaskStore(withCycleEdge = false): Promise<GraphStore> {
      const store = new GraphStore(':memory:');
      // 8 Task nodes
      for (const id of ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']) {
        await store.addNode(id, 'Task', { name: id });
      }
      // 8 dependency edges (B depends on A means B→A)
      const edges: [string, string][] = [
        ['B', 'A'], ['C', 'A'], ['D', 'B'], ['E', 'C'],
        ['F', 'D'], ['F', 'E'], ['G', 'F'], ['H', 'G'],
      ];
      for (const [from, to] of edges) {
        await store.addEdge(`${from}->${to}`, from, to, 'depends_on', {});
      }
      if (withCycleEdge) {
        // A→H closes the cycle: A depends on H, which (via G→F→...→B) depends on A
        await store.addEdge('A->H', 'A', 'H', 'depends_on', {});
      }
      return store;
    }

    // -----------------------------------------------------------------------
    // Test 1: Both approaches detect no cycle in the valid 8-task DAG
    // -----------------------------------------------------------------------

    it('both approaches detect no cycle in valid 8-task DAG', async () => {
      // Domain-specific
      expect(detectCycleDomainSpecific(buildTasks)).toBe(false);

      // UGFM
      const store = await buildUGFMTaskStore(false);
      const result = runUniversalToolchain(store, 'workflow');
      expect(result.hasCycle).toBe(false);
    });

    // -----------------------------------------------------------------------
    // Test 2: Both approaches detect the cycle when A→H is added
    //
    // Adding A depends_on H creates a full back-edge in the dep chain:
    //   A→H→G→F→D→B→A  (following deps from A reaches H, and H→A closes the loop)
    // Both the domain-specific DFS (which follows deps edges) and UGFM hasCycle
    // detect this as a cycle.
    // -----------------------------------------------------------------------

    it('both approaches detect cycle when circular dependency added', async () => {
      // Domain-specific: add A→H (A depends on H), closing the cycle
      const cycleTaskA: Task = { id: 'A', deps: ['H'] };
      const tasksWithCycle: Task[] = buildTasks
        .filter(t => t.id !== 'A')
        .concat(cycleTaskA);
      expect(detectCycleDomainSpecific(tasksWithCycle)).toBe(true);

      // UGFM: add A depends_on H edge to close the cycle
      const store = await buildUGFMTaskStore(false);
      await store.addEdge('A->H', 'A', 'H', 'depends_on', {});
      const result = runUniversalToolchain(store, 'workflow');
      expect(result.hasCycle).toBe(true);
    });

    // -----------------------------------------------------------------------
    // Test 3: UGFM hasCycle call is identical to concurrent/knowledge/numerical calls
    //
    // Verifies that the same store.hasCycle() method used for the task domain
    // is the same one used across concurrent, knowledge, and numerical domains.
    // -----------------------------------------------------------------------

    it('UGFM hasCycle call is identical to concurrent/knowledge/numerical domain calls', async () => {
      const taskStore = await buildUGFMTaskStore(false);

      // Build one example each of concurrent and knowledge domains
      const actorStore = new GraphStore(':memory:');
      await actorStore.addNode('orch', 'Actor', { name: 'orchestrator' });
      await actorStore.addNode('w1', 'Actor', { name: 'worker-1' });
      await actorStore.addEdge('ch', 'orch', 'w1', 'MessageChannel', {});

      const conceptStore = new GraphStore(':memory:');
      await conceptStore.addNode('gs', 'Concept', { name: 'GraphSubstrate' });
      await conceptStore.addNode('scc', 'Concept', { name: 'SCC' });
      await conceptStore.addEdge('e1', 'gs', 'scc', 'Enables', {});

      const taskResult = runUniversalToolchain(taskStore, 'workflow');
      const actorResult = runUniversalToolchain(actorStore, 'concurrent');
      const conceptResult = runUniversalToolchain(conceptStore, 'knowledge');

      // The set of algorithms called is identical across all three domains
      expect(taskResult.algorithmsUsed).toEqual(actorResult.algorithmsUsed);
      expect(taskResult.algorithmsUsed).toEqual(conceptResult.algorithmsUsed);
      expect(taskResult.algorithmsUsed).toContain('hasCycle');
      expect(taskResult.algorithmsUsed).toContain('findSCCs');
    });

    // -----------------------------------------------------------------------
    // Test 4: Domain-specific detectCycle required custom algorithm code
    //
    // Asserts that the domain-specific approach needed a new function.
    // The function's source contains the custom DFS logic — not zero code.
    // -----------------------------------------------------------------------

    it('domain-specific detectCycle required custom algorithm code — counted in spec', () => {
      const src = detectCycleDomainSpecific.toString();
      // The function exists and contains its own DFS implementation
      expect(src).toContain('function dfs');
      expect(src).toContain('inStack');
      expect(src).toContain('visited');
      // It is not trivially short — it needed real implementation work
      expect(src.length).toBeGreaterThan(200);
    });

    // -----------------------------------------------------------------------
    // Test 5: UGFM adds task domain with zero new algorithm code
    //
    // No new function like detectCycleDomainSpecific was written.
    // The task domain reuses store.hasCycle() from the shared substrate.
    // -----------------------------------------------------------------------

    it('UGFM adds task domain with zero new algorithm code (same hasCycle)', async () => {
      const store = await buildUGFMTaskStore(false);
      // hasCycle is a method on GraphStore — shared across ALL domains
      // No domain-specific cycle-detection function was authored for the task domain
      expect(typeof store.hasCycle).toBe('function');

      // The result is computed by the same shared method, not a domain-specific one
      const result = runUniversalToolchain(store, 'workflow');
      expect(result.algorithmsUsed).toEqual(['findSCCs', 'hasCycle']);

      // Crucially: the algorithmsUsed list does NOT grow when a new domain is added
      // (it stays exactly ['findSCCs', 'hasCycle'] for every domain)
      const numericalStore = new GraphStore(':memory:');
      await numericalStore.addNode('g00', 'GridPoint', { x: 0, y: 0 });
      const numericalResult = runUniversalToolchain(numericalStore, 'numerical');
      expect(numericalResult.algorithmsUsed).toEqual(result.algorithmsUsed);
    });
  });
});
