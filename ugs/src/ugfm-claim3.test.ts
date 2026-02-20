/**
 * UGFM Claim 3 Demonstration: Producer-Consumer Kripke Structure
 *
 * Demonstrates that a formal model (Kripke state-transition graph) expressed
 * as ugs nodes and edges supports graph-algorithmic verification:
 * - Reachability (BFS)
 * - Deadlock detection (sink nodes)
 * - SCC-based liveness analysis
 * - Cycle detection
 * - Domain projection (subgraph views)
 *
 * This test suite verifies UGFM Claim 3: implementation artifacts can be
 * verified from a formal graph model without domain-specific tools.
 * Evidence: ev-207, ev-208 (MIC prior art), ev-033, ev-169 (concurrent systems).
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import GraphStore from './graph';

describe('UGFM Claim 3: Producer-Consumer Kripke Structure', () => {
  let store: GraphStore;

  beforeEach(async () => {
    store = new GraphStore(`/tmp/ugfm-claim3-${Date.now()}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();

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
