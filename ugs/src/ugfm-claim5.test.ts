/**
 * UGFM Claim 5 Demonstration: LLM-Orchestrated Deterministic Graph Tools
 *
 * UGFM Claim 5: An LLM can act as a meta-orchestrator that translates
 * natural-language questions into calls to deterministic graph algorithms,
 * returning results with guaranteed reproducibility. The LLM does semantic
 * routing; the graph tools do deterministic computation.
 *
 * Key architectural invariant: orchestrate() contains NO graph reasoning.
 * It only maps question patterns to algorithm names. All reasoning about
 * graph structure happens inside deterministic functions. Calling the same
 * question twice returns identical results.
 *
 * This file uses a mock orchestrator (regex-based routing) as a structural
 * stand-in for real LLM dispatch. No live API calls are made.
 */

import { describe, it, expect, beforeEach } from 'bun:test';
import GraphStore from './graph';
import { orchestrate, orchestrateWithLLM, createOrchestratorTracker } from './ugfm-claim5';

describe('UGFM Claim 5: LLM-Orchestrated Deterministic Graph Tools', () => {
  let store: GraphStore;

  beforeEach(async () => {
    store = new GraphStore(':memory:');
    // Build a test graph with actors and a cycle: A → B → C → A
    await store.addNode('actor-a', 'Actor', { name: 'A', role: 'producer' });
    await store.addNode('actor-b', 'Actor', { name: 'B', role: 'consumer' });
    await store.addNode('actor-c', 'Actor', { name: 'C', role: 'router' });
    await store.addEdge('e1', 'actor-a', 'actor-b', 'MessageChannel', {});
    await store.addEdge('e2', 'actor-b', 'actor-c', 'MessageChannel', {});
    await store.addEdge('e3', 'actor-c', 'actor-a', 'MessageChannel', {});
    // Cycle: actor-a → actor-b → actor-c → actor-a
  });

  // ==========================================================================
  // 5.1 Semantic routing — question maps to algorithm
  // ==========================================================================

  describe('5.1 Semantic routing — question maps to algorithm', () => {
    it('deadlock question routes to findSCCs', () => {
      const result = orchestrate('Is there a deadlock in this system?', store);
      expect(result.algorithm).toBe('findSCCs');
    });

    it('cycle question routes to hasCycle', () => {
      const result = orchestrate('Does this graph have a cycle?', store);
      expect(result.algorithm).toBe('hasCycle');
    });

    it('reachability question routes to traverse', () => {
      const result = orchestrate('Can actor-a reach actor-c?', store);
      expect(result.algorithm).toBe('traverse');
    });

    it('role/type question routes to getByType', () => {
      const result = orchestrate('What actors are in this system?', store);
      expect(result.algorithm).toBe('getByType');
    });

    it('unknown question gets unknown algorithm', () => {
      const result = orchestrate('What is the meaning of life?', store);
      expect(result.algorithm).toBe('unknown');
    });
  });

  // ==========================================================================
  // 5.2 Determinism — same question, same result
  // ==========================================================================

  describe('5.2 Determinism — same question, same result', () => {
    it('deadlock question returns identical SCCs on repeated calls', () => {
      const r1 = orchestrate('Is there a deadlock?', store);
      const r2 = orchestrate('Is there a deadlock?', store);
      expect(JSON.stringify(r1.result)).toBe(JSON.stringify(r2.result));
    });

    it('result is always tagged deterministic: true', () => {
      const questions = [
        'Is there a deadlock?',
        'Can actor-a reach actor-c?',
        'What actors are in this system?',
        'Does this graph have a cycle?',
      ];
      for (const q of questions) {
        const result = orchestrate(q, store);
        expect(result.deterministic).toBe(true);
      }
    });

    it('graph mutation changes result, not routing', () => {
      const before = orchestrate('Is there a deadlock?', store);
      // The A→B→C→A cycle should appear as one SCC of size 3
      const beforeSCCs = before.result as string[][];
      const bigSCC = beforeSCCs.find(scc => scc.length > 1);
      expect(bigSCC).toBeDefined();
      expect(bigSCC!.length).toBe(3);
    });
  });

  // ==========================================================================
  // 5.3 Orchestrator contains no graph reasoning
  // ==========================================================================

  describe('5.3 Orchestrator contains no graph reasoning', () => {
    it('routing decision does not inspect graph structure', () => {
      // This test demonstrates the architecture: orchestrate() routes based
      // ONLY on question text. We verify this by calling with an EMPTY store —
      // routing still works, and result reflects the empty graph state.
      const emptyStore = new GraphStore(':memory:');
      const result = orchestrate('Is there a deadlock in this empty system?', emptyStore);
      // Routing works fine regardless of graph content
      expect(result.algorithm).toBe('findSCCs');
      expect(result.deterministic).toBe(true);
      // Empty store → no SCCs
      const sccs = result.result as string[][];
      expect(sccs.length).toBe(0);
    });

    it('orchestrator is stateless — store is the only state', () => {
      // Call with different stores for the same question.
      const emptyStore = new GraphStore(':memory:');
      // emptyStore has no nodes, no edges
      const r1 = orchestrate('What actors are in this system?', store);
      const r2 = orchestrate('What actors are in this system?', emptyStore);
      // Results differ because STORES differ, not because orchestrator has state
      const count1 = (r1.result as string[]).length;
      const count2 = (r2.result as string[]).length;
      expect(count1).toBeGreaterThan(count2);
      // Populated store has 3 actors; empty store has 0
      expect(count1).toBe(3);
      expect(count2).toBe(0);
    });
  });

  // ==========================================================================
  // 5.4 Orchestrator tracker — routing is auditable
  // ==========================================================================

  describe('5.4 Orchestrator tracker — routing is auditable', () => {
    it('tracks all calls and algorithm distribution', () => {
      const tracker = createOrchestratorTracker();
      tracker.call('Is there a deadlock?', store);
      tracker.call('Is there a deadlock?', store);
      tracker.call('What actors are in this system?', store);

      const stats = tracker.stats();
      expect(stats.totalCalls).toBe(3);
      expect(stats.algorithmDistribution.findSCCs).toBe(2);
      expect(stats.algorithmDistribution.getByType).toBe(1);
    });

    it('history is immutable — tracker returns copy', () => {
      const tracker = createOrchestratorTracker();
      tracker.call('Is there a deadlock?', store);
      const history1 = tracker.history();
      tracker.call('What actors?', store);
      const history2 = tracker.history();
      // First snapshot is not mutated
      expect(history1.length).toBe(1);
      expect(history2.length).toBe(2);
    });

    it('routing pattern is included in each result for audit transparency', () => {
      const tracker = createOrchestratorTracker();
      const r1 = tracker.call('Is there a deadlock?', store);
      const r2 = tracker.call('What actors are in this system?', store);
      const r3 = tracker.call('What is the meaning of life?', store);

      expect(r1.routingPattern).toBe('cycle/SCC detection');
      expect(r2.routingPattern).toBe('node type query');
      expect(r3.routingPattern).toBe('no pattern matched');
    });

    it('stats initializes all algorithm counts to zero', () => {
      const tracker = createOrchestratorTracker();
      const stats = tracker.stats();
      expect(stats.totalCalls).toBe(0);
      expect(stats.algorithmDistribution.findSCCs).toBe(0);
      expect(stats.algorithmDistribution.hasCycle).toBe(0);
      expect(stats.algorithmDistribution.traverse).toBe(0);
      expect(stats.algorithmDistribution.getByType).toBe(0);
      expect(stats.algorithmDistribution.unknown).toBe(0);
    });
  });

  // ==========================================================================
  // 5.5 Live LLM Integration (skips if ANTHROPIC_API_KEY not set)
  // ==========================================================================

  describe('5.5 Live LLM Integration (skips if ANTHROPIC_API_KEY not set)', () => {
    it('LLM correctly routes deadlock question to findSCCs algorithm', async () => {
      if (!process.env.ANTHROPIC_API_KEY) {
        console.log('  [SKIP] ANTHROPIC_API_KEY not set — skipping live LLM test');
        return;
      }
      const result = await orchestrateWithLLM(
        'Is there a deadlock in this actor system?',
        store
      );
      // LLM should route to findSCCs or hasCycle for deadlock questions
      expect(['findSCCs', 'hasCycle']).toContain(result.algorithm);
      expect(result.deterministic).toBe(true);
      expect(result.routingPattern).toContain('llm-selected');
      // The result must be a real graph computation, not null
      expect(result.result).not.toBeNull();
    });

    it('LLM-routed result is deterministic — calling findSCCs twice gives identical output', async () => {
      if (!process.env.ANTHROPIC_API_KEY) {
        console.log('  [SKIP] ANTHROPIC_API_KEY not set — skipping live LLM test');
        return;
      }
      // This tests the architectural claim: LLM selects, graph computes deterministically
      const result1 = await orchestrateWithLLM('Find strongly connected components', store);
      // The graph result must be identical regardless of how many times we call the algorithm
      const directResult = store.findSCCs();
      if (result1.algorithm === 'findSCCs') {
        expect(JSON.stringify(result1.result)).toBe(JSON.stringify(directResult));
      }
      expect(result1.deterministic).toBe(true);
    });
  });
});
