/**
 * ugfm-datalog.ts — Minimal Datalog/CTL Evaluator over GraphStore
 *
 * Implements CTL (Computation Tree Logic) model checking as fixed-point
 * iteration over GraphStore adjacency maps, establishing the equivalence
 * between Datalog with stratified negation and CTL model checking.
 *
 * Theoretical basis:
 *   - Emerson & Clarke (1980): CTL reduces to mu-calculus fixed points
 *   - Immerman (1986): Datalog with stratified negation captures PTIME
 *   - The four CTL modalities map to two fixed-point classes:
 *       Least fixed points (LFP): EF, AF  — forward/backward reachability
 *       Greatest fixed points (GFP): EG, AG — SCC-based invariance
 *
 * GraphStore API used:
 *   store.nodes            Map<id, Node>
 *   store.adjacencyOut     Map<id, Array<{to, edgeId, type, weight}>>
 *   store.adjacencyIn      Map<id, Array<{from, edgeId, type, weight}>>
 *   store.findSCCs()       string[][]  (Tarjan's, reverse topological order)
 *
 * All evaluators operate on any GraphStore — not Kripke-specific.
 * The caller supplies a predicate function that maps node ids to booleans.
 */

import type GraphStore from './graph';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/** A state predicate: given a node id, returns true/false. */
export type Predicate = (nodeId: string) => boolean;

/** Result of a CTL check: which node ids satisfy the formula. */
export type CTLResult = Set<string>;

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/**
 * Build the initial satisfaction set from a predicate over all nodes
 * in the store.
 */
function initialSat(store: GraphStore, pred: Predicate): Set<string> {
  const sat = new Set<string>();
  for (const id of store.nodes.keys()) {
    if (pred(id)) sat.add(id);
  }
  return sat;
}

/**
 * Compute predecessors of a node set under adjacencyIn (one step backward).
 * Returns all node ids that have at least one outgoing edge into `targets`.
 */
function onePredecessor(store: GraphStore, targets: Set<string>): Set<string> {
  const preds = new Set<string>();
  for (const target of targets) {
    const inEdges = store.adjacencyIn.get(target) ?? [];
    for (const { from } of inEdges) {
      preds.add(from);
    }
  }
  return preds;
}


/**
 * Check whether every successor of `nodeId` is in `sat`.
 * Nodes with no successors (sinks) vacuously satisfy "all successors in sat".
 */
function allSuccessorsIn(store: GraphStore, nodeId: string, sat: Set<string>): boolean {
  const outEdges = store.adjacencyOut.get(nodeId) ?? [];
  // A sink (no outgoing edges) vacuously satisfies "all successors" — important:
  // in temporal logic, a sink node has no successor states, so universally-
  // quantified path conditions are vacuously true. This is the standard
  // Kripke semantics.
  if (outEdges.length === 0) return true;
  return outEdges.every(({ to }) => sat.has(to));
}

/**
 * Check whether at least one successor of `nodeId` is in `sat`.
 */
function someSuccessorIn(store: GraphStore, nodeId: string, sat: Set<string>): boolean {
  const outEdges = store.adjacencyOut.get(nodeId) ?? [];
  return outEdges.some(({ to }) => sat.has(to));
}

// ---------------------------------------------------------------------------
// CTL fixed-point evaluators
// ---------------------------------------------------------------------------

/**
 * EF P — "There EXists a path where P is eventually true (Future)"
 *
 * Least fixed-point computation (backward reachability):
 *   EF P = lfp Z. P ∪ EX(Z)
 *
 * Datalog rule:
 *   ef(X) :- p(X).
 *   ef(X) :- edge(X, Y), ef(Y).
 *
 * Algorithm: start with all nodes satisfying P, then iteratively add
 * nodes that have at least one successor already in the set.
 * This is backward reachability: a node satisfies EF P if P holds now,
 * or there exists a next state where EF P holds.
 */
export function checkEF(store: GraphStore, pred: Predicate): CTLResult {
  const sat = initialSat(store, pred);
  let changed = true;

  while (changed) {
    changed = false;
    // Add all predecessors of current sat set (nodes that can reach sat in one step)
    const preds = onePredecessor(store, sat);
    for (const id of preds) {
      if (!sat.has(id)) {
        sat.add(id);
        changed = true;
      }
    }
  }

  return sat;
}

/**
 * AF P — "On All paths, P is eventually true (Future)"
 *
 * Least fixed-point computation (backward):
 *   AF P = lfp Z. P ∪ AX(Z)
 *   where AX(Z) = {s | all successors of s are in Z}
 *
 * Datalog rule (with stratified negation for "all successors"):
 *   af(X) :- p(X).
 *   af(X) :- node(X), not sink(X), forall Y: edge(X,Y) => af(Y).
 *
 * Algorithm: start with nodes satisfying P, then iteratively add
 * nodes where ALL successors are already in the satisfaction set.
 *
 * Note on sink nodes: a sink has no successors. Under standard CTL
 * semantics, AF P at a sink requires P to hold there (since the only
 * "path" from a sink is the empty continuation, which means P must hold
 * at the current state). Sinks where P doesn't hold do NOT satisfy AF P.
 */
export function checkAF(store: GraphStore, pred: Predicate): CTLResult {
  const sat = initialSat(store, pred);
  let changed = true;

  while (changed) {
    changed = false;
    for (const id of store.nodes.keys()) {
      if (sat.has(id)) continue;
      const outEdges = store.adjacencyOut.get(id) ?? [];
      // Sink nodes: only in sat if pred holds (already handled by initialSat)
      if (outEdges.length === 0) continue;
      // Non-sink: all successors must be in sat
      if (outEdges.every(({ to }) => sat.has(to))) {
        sat.add(id);
        changed = true;
      }
    }
  }

  return sat;
}

/**
 * EG P — "There Exists a path where P is always true (Globally)"
 *
 * Greatest fixed-point computation (SCC-based):
 *   EG P = gfp Z. P ∩ EX(Z)
 *
 * Datalog rule:
 *   eg(X) :- p(X), not (exists Y: edge(X,Y), not eg(Y)).
 *
 * Algorithm (SCC-based, equivalent to greatest fixed-point iteration):
 *   1. Restrict graph to nodes satisfying P (only P-states can be on such a path)
 *   2. Find SCCs in the restricted graph
 *   3. A node satisfies EG P if there is a path in the restricted graph to
 *      a "non-trivial" component: either a cycle (SCC of size > 1) or a
 *      singleton SCC with a self-loop.
 *   4. Propagate backward within the P-restricted subgraph: any node with
 *      a successor that satisfies EG P also satisfies EG P.
 *
 * This is equivalent to: exists an infinite path (in the Kripke structure)
 * starting from s where P holds at every state.
 */
export function checkEG(store: GraphStore, pred: Predicate): CTLResult {
  // Step 1: restrict to P-satisfying nodes
  const pNodes = initialSat(store, pred);

  // Step 2: find SCCs restricted to P-nodes
  // We use the full store's adjacency but filter edges to P-nodes only.
  // Build a local adjacency map for the P-restricted subgraph.
  const restrictedOut = new Map<string, string[]>();
  for (const id of pNodes) {
    const outEdges = store.adjacencyOut.get(id) ?? [];
    restrictedOut.set(id, outEdges.filter(({ to }) => pNodes.has(to)).map(({ to }) => to));
  }

  // Step 3: find "recurrent" nodes — nodes in non-trivial SCCs of P-subgraph
  // A non-trivial SCC is: size > 1, OR size == 1 with a self-loop
  const recurrent = new Set<string>();

  // Tarjan's SCC on restricted subgraph
  const index = new Map<string, number>();
  const lowlink = new Map<string, number>();
  const onStack = new Set<string>();
  const stack: string[] = [];
  let counter = 0;

  const strongconnect = (root: string) => {
    const callStack: Array<{ nodeId: string; neighborIdx: number }> = [];
    index.set(root, counter);
    lowlink.set(root, counter);
    counter++;
    stack.push(root);
    onStack.add(root);
    callStack.push({ nodeId: root, neighborIdx: 0 });

    while (callStack.length > 0) {
      const frame = callStack[callStack.length - 1];
      const { nodeId } = frame;
      const neighbors = restrictedOut.get(nodeId) ?? [];

      if (frame.neighborIdx < neighbors.length) {
        const w = neighbors[frame.neighborIdx];
        frame.neighborIdx++;
        if (!index.has(w)) {
          index.set(w, counter);
          lowlink.set(w, counter);
          counter++;
          stack.push(w);
          onStack.add(w);
          callStack.push({ nodeId: w, neighborIdx: 0 });
        } else if (onStack.has(w)) {
          lowlink.set(nodeId, Math.min(lowlink.get(nodeId)!, index.get(w)!));
        }
      } else {
        callStack.pop();
        if (callStack.length > 0) {
          const parent = callStack[callStack.length - 1].nodeId;
          lowlink.set(parent, Math.min(lowlink.get(parent)!, lowlink.get(nodeId)!));
        }
        if (lowlink.get(nodeId) === index.get(nodeId)) {
          const scc: string[] = [];
          let w: string;
          do {
            w = stack.pop()!;
            onStack.delete(w);
            scc.push(w);
          } while (w !== nodeId);

          // Mark as recurrent if non-trivial
          if (scc.length > 1) {
            for (const id of scc) recurrent.add(id);
          } else {
            // Singleton: check for self-loop
            const id = scc[0];
            const selfLoop = (restrictedOut.get(id) ?? []).includes(id);
            if (selfLoop) recurrent.add(id);
          }
        }
      }
    }
  };

  for (const id of pNodes) {
    if (!index.has(id)) strongconnect(id);
  }

  // Step 4: backward reachability within P-subgraph from recurrent nodes
  // A node satisfies EG P if it can reach a recurrent node while staying in P-nodes
  const sat = new Set<string>(recurrent);
  let changed = true;
  while (changed) {
    changed = false;
    for (const id of pNodes) {
      if (sat.has(id)) continue;
      const succs = restrictedOut.get(id) ?? [];
      if (succs.some(s => sat.has(s))) {
        sat.add(id);
        changed = true;
      }
    }
  }

  return sat;
}

/**
 * AG P — "On All paths, P is always true (Globally)"
 *
 * Computed via duality:
 *   AG P = ¬EF(¬P)
 *
 * Datalog rule (using stratified negation):
 *   ag(X) :- not ef_not_p(X).
 *   ef_not_p(X) :- not p(X).
 *   ef_not_p(X) :- edge(X, Y), ef_not_p(Y).
 *
 * Algorithm:
 *   1. Compute EF(¬P): nodes from which ¬P is reachable
 *   2. AG P = all nodes NOT in EF(¬P)
 *
 * Semantics: AG P holds at s iff P holds at every state reachable from s
 * on any path (including s itself). Equivalently, there is no reachable
 * state where P fails.
 */
export function checkAG(store: GraphStore, pred: Predicate): CTLResult {
  // ¬P: nodes where pred is false
  const notPred: Predicate = (id) => !pred(id);

  // EF(¬P): nodes that can reach a state where P fails
  const efNotP = checkEF(store, notPred);

  // AG P = complement of EF(¬P) over all nodes
  const sat = new Set<string>();
  for (const id of store.nodes.keys()) {
    if (!efNotP.has(id)) {
      sat.add(id);
    }
  }
  return sat;
}

// ---------------------------------------------------------------------------
// Compound CTL properties
// ---------------------------------------------------------------------------

/**
 * GF P — "Globally eventually P" (infinitely often P on all paths)
 *
 * GF P = AG(AF P)
 *
 * This is the CTL liveness property: every path visits a P-state infinitely
 * often. Computed by composing AG and AF.
 *
 * Relationship to Büchi automata: GF P is equivalent to EG(AF P) in CTL.
 * The checkGFLiveness helper in suite 2.5 checks it via SCC terminal analysis;
 * here we express it directly in CTL as AG(AF P).
 */
export function checkGF(store: GraphStore, pred: Predicate): CTLResult {
  const afP = checkAF(store, pred);
  // GF P = AG(AF P): on all paths, AF P is always true
  return checkAG(store, (id) => afP.has(id));
}

/**
 * EU (P Until Q) — "There Exists a path where P holds Until Q"
 *
 * Least fixed-point:
 *   E[P U Q] = lfp Z. Q ∪ (P ∩ EX(Z))
 *
 * Datalog rule:
 *   eu(X) :- q(X).
 *   eu(X) :- p(X), edge(X, Y), eu(Y).
 *
 * Algorithm: backward reachability from Q-states through P-states.
 */
export function checkEU(store: GraphStore, pPred: Predicate, qPred: Predicate): CTLResult {
  const sat = initialSat(store, qPred);
  let changed = true;

  while (changed) {
    changed = false;
    for (const id of store.nodes.keys()) {
      if (sat.has(id)) continue;
      if (!pPred(id)) continue; // P must hold at this node
      // Exists a successor in sat
      if (someSuccessorIn(store, id, sat)) {
        sat.add(id);
        changed = true;
      }
    }
  }

  return sat;
}

/**
 * AU (P Until Q) — "On All paths, P holds Until Q"
 *
 * Least fixed-point:
 *   A[P U Q] = lfp Z. Q ∪ (P ∩ AX(Z))
 *
 * Equivalent to: AF Q (when we ignore the P constraint... not quite).
 * More precisely: A[P U Q] ⊆ AF Q, but the P constraint prevents
 * paths from staying in P-states indefinitely without reaching Q.
 *
 * Datalog rule:
 *   au(X) :- q(X).
 *   au(X) :- p(X), node(X), forall Y: edge(X,Y) => au(Y).
 */
export function checkAU(store: GraphStore, pPred: Predicate, qPred: Predicate): CTLResult {
  const sat = initialSat(store, qPred);
  let changed = true;

  while (changed) {
    changed = false;
    for (const id of store.nodes.keys()) {
      if (sat.has(id)) continue;
      if (!pPred(id)) continue; // P must hold at this node
      const outEdges = store.adjacencyOut.get(id) ?? [];
      // Sink node where P holds but Q doesn't: A[P U Q] is false (no path reaches Q)
      if (outEdges.length === 0) continue;
      // All successors must satisfy A[P U Q]
      if (allSuccessorsIn(store, id, sat)) {
        sat.add(id);
        changed = true;
      }
    }
  }

  return sat;
}

// ---------------------------------------------------------------------------
// Convenience wrapper
// ---------------------------------------------------------------------------

/**
 * CTL formula specification for the checkCTL convenience wrapper.
 *
 * Supports all six operators implemented above plus the compound GF and until.
 */
export type CTLFormula =
  | { op: 'EF'; pred: Predicate }
  | { op: 'AF'; pred: Predicate }
  | { op: 'EG'; pred: Predicate }
  | { op: 'AG'; pred: Predicate }
  | { op: 'GF'; pred: Predicate }
  | { op: 'EU'; pPred: Predicate; qPred: Predicate }
  | { op: 'AU'; pPred: Predicate; qPred: Predicate }
  | { op: 'NOT'; inner: CTLFormula }
  | { op: 'AND'; left: CTLFormula; right: CTLFormula }
  | { op: 'OR'; left: CTLFormula; right: CTLFormula };

/**
 * checkCTL — evaluate a CTLFormula on a GraphStore.
 *
 * Returns the set of node ids satisfying the formula.
 * Supports recursive composition of CTL operators.
 *
 * @example
 *   // AG(not-deadlock): all states on all paths are non-sinks
 *   const result = checkCTL(store, {
 *     op: 'AG',
 *     pred: (id) => (store.adjacencyOut.get(id)?.length ?? 0) > 0
 *   });
 *   const holds = result.has('initialState');
 */
export function checkCTL(store: GraphStore, formula: CTLFormula): CTLResult {
  switch (formula.op) {
    case 'EF':
      return checkEF(store, formula.pred);

    case 'AF':
      return checkAF(store, formula.pred);

    case 'EG':
      return checkEG(store, formula.pred);

    case 'AG':
      return checkAG(store, formula.pred);

    case 'GF':
      return checkGF(store, formula.pred);

    case 'EU':
      return checkEU(store, formula.pPred, formula.qPred);

    case 'AU':
      return checkAU(store, formula.pPred, formula.qPred);

    case 'NOT': {
      const inner = checkCTL(store, formula.inner);
      const result = new Set<string>();
      for (const id of store.nodes.keys()) {
        if (!inner.has(id)) result.add(id);
      }
      return result;
    }

    case 'AND': {
      const left = checkCTL(store, formula.left);
      const right = checkCTL(store, formula.right);
      const result = new Set<string>();
      for (const id of left) {
        if (right.has(id)) result.add(id);
      }
      return result;
    }

    case 'OR': {
      const left = checkCTL(store, formula.left);
      const right = checkCTL(store, formula.right);
      const result = new Set<string>([...left, ...right]);
      return result;
    }
  }
}

// ---------------------------------------------------------------------------
// Utility: check if a CTL property holds at a specific initial state
// ---------------------------------------------------------------------------

/**
 * Convenience: check whether a CTL formula holds at a given initial state.
 *
 * @param store      The GraphStore (Kripke structure)
 * @param formula    The CTL formula
 * @param initialId  The initial state id
 * @returns true if the formula holds at the initial state
 */
export function checkCTLAt(store: GraphStore, formula: CTLFormula, initialId: string): boolean {
  return checkCTL(store, formula).has(initialId);
}
