/**
 * ugfm-processgraph.ts — generatePromela()
 *
 * Converts a GraphStore ProcessGraph to a valid Promela specification
 * for SPIN model checking.
 *
 * Node convention (what the function recognizes):
 *   - type === 'process-state'  : a state in a process automaton
 *     - property `label`        : the Promela label name (string, required)
 *     - property `initial`      : true if this is the start state
 *     - property `atomic`       : true if this state is an atomic proposition
 *                                 (used for LTL safety property generation)
 *   - type === 'shared-var'     : a shared variable declaration (optional)
 *     - property `varType`      : 'bool' | 'byte' | 'int' (default 'bool')
 *     - property `varName`      : variable name (required)
 *     - property `initVal`      : initial value (optional)
 *
 * Edge convention:
 *   - type === 'transition'     : a transition between process-states
 *     - property `process`      : which process this transition belongs to
 *                                 (used to group states into proctypes)
 *     - property `condition`    : optional Promela guard expression (string)
 *     - property `action`       : optional Promela action expression (string)
 *
 * Output: a string of valid Promela that SPIN (version 6+) can verify.
 * The output uses goto-based state machines (one proctype per process).
 *
 * When only one process is defined, a single active proctype is emitted.
 * When multiple processes share states (via `process` edge property), one
 * proctype is emitted per process.
 *
 * LTL properties generated automatically:
 *   - Safety: for any two nodes with `atomic: true` in different proctypes,
 *     generate [] !(proc_a@label_a && proc_b@label_b)
 *
 * Limitations (honest boundary):
 *   - Does not generate never-claims for response liveness (G(A→FB))
 *   - Does not handle parallel composition with channel communication
 *   - Shared variable semantics must be manually encoded as conditions/actions
 *   - For full LTL model checking, use SPIN's built-in LTL compiler
 */

import type GraphStore from './graph.js';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface ProcessState {
  nodeId: string;
  label: string;      // Promela label identifier (sanitized)
  initial: boolean;
  atomic: boolean;    // whether this state should appear in LTL properties
  process: string;    // which proctype this belongs to
}

interface ProcessTransition {
  from: string;       // source nodeId
  to: string;         // target nodeId
  condition: string;  // Promela guard (may be empty string = unconditional)
  action: string;     // Promela action (may be empty string)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Sanitize a string to be a valid Promela identifier.
 * Promela identifiers: [a-zA-Z_][a-zA-Z0-9_]*
 */
function sanitizeLabel(s: string): string {
  // Replace all non-alphanumeric/underscore characters with underscore
  // Ensure starts with a letter or underscore
  const cleaned = s.replace(/[^a-zA-Z0-9_]/g, '_');
  if (/^[0-9]/.test(cleaned)) {
    return '_' + cleaned;
  }
  return cleaned || '_state';
}

/**
 * Extract all process-state nodes from the store, grouped by process name.
 * Falls back to a single default process if no `process` property is found
 * on any transition edge.
 */
function extractProcessStates(store: GraphStore): Map<string, ProcessState[]> {
  const byProcess = new Map<string, ProcessState[]>();

  // First pass: determine which process each state belongs to via outgoing edges
  const stateToProcess = new Map<string, string>();
  for (const edge of store.edges.values()) {
    if (edge.type === 'transition') {
      const proc = edge.properties.get('process') as string | undefined;
      if (proc) {
        stateToProcess.set(edge.from, proc);
        stateToProcess.set(edge.to, proc);
      }
    }
  }

  // Second pass: build ProcessState objects for all process-state nodes
  for (const node of store.nodes.values()) {
    if (node.type !== 'process-state') continue;

    const rawLabel = (node.properties.get('label') as string | undefined) ?? node.id;
    const label = sanitizeLabel(rawLabel);
    const initial = node.properties.get('initial') === true;
    const atomic = node.properties.get('atomic') === true;
    const process = stateToProcess.get(node.id) ?? 'proc';

    const state: ProcessState = { nodeId: node.id, label, initial, atomic, process };

    if (!byProcess.has(process)) {
      byProcess.set(process, []);
    }
    byProcess.get(process)!.push(state);
  }

  return byProcess;
}

/**
 * Extract all transition edges as ProcessTransition records.
 */
function extractTransitions(store: GraphStore): ProcessTransition[] {
  const transitions: ProcessTransition[] = [];

  for (const edge of store.edges.values()) {
    if (edge.type !== 'transition') continue;

    const condition = (edge.properties.get('condition') as string | undefined) ?? '';
    const action = (edge.properties.get('action') as string | undefined) ?? '';

    transitions.push({
      from: edge.from,
      to: edge.to,
      condition,
      action,
    });
  }

  return transitions;
}

/**
 * Extract shared variable declarations from 'shared-var' nodes.
 */
function extractSharedVars(store: GraphStore): string[] {
  const decls: string[] = [];

  for (const node of store.nodes.values()) {
    if (node.type !== 'shared-var') continue;

    const varType = (node.properties.get('varType') as string | undefined) ?? 'bool';
    const varName = node.properties.get('varName') as string | undefined;
    if (!varName) continue;

    const initVal = node.properties.get('initVal');
    if (initVal !== undefined && initVal !== null) {
      decls.push(`${varType} ${varName} = ${initVal};`);
    } else {
      decls.push(`${varType} ${varName};`);
    }
  }

  return decls;
}

// ---------------------------------------------------------------------------
// Proctype generator
// ---------------------------------------------------------------------------

/**
 * Generate a single Promela proctype for one process.
 *
 * Strategy: goto-based state machine.
 *   - Each state is a Promela label.
 *   - Transitions become either:
 *       - unconditional: `goto target_label`
 *       - conditional:   `condition -> action; goto target_label`
 *   - Multiple outgoing transitions from one state become an `if` block:
 *       `if :: cond1 -> action1; goto t1 :: cond2 -> action2; goto t2 fi`
 *   - A state with no outgoing transitions gets `goto initial_label` to
 *     create a cycle (models a process that loops after reaching terminal state).
 *
 * The initial state is placed first so SPIN starts there.
 */
function generateProctype(
  processName: string,
  states: ProcessState[],
  transitions: ProcessTransition[],
  nodeIdToLabel: Map<string, string>,
  processNodeIds: Set<string>
): string {
  const lines: string[] = [];

  // Find the initial state; fallback to first state
  const initialState = states.find(s => s.initial) ?? states[0];
  if (!initialState) {
    return `/* No states found for process ${processName} */\n`;
  }

  lines.push(`active proctype ${processName}()`);
  lines.push(`{`);

  // Order states: initial first, then the rest
  const orderedStates = [
    initialState,
    ...states.filter(s => s.nodeId !== initialState.nodeId)
  ];

  for (const state of orderedStates) {
    lines.push(`${state.label}:`);

    // Find outgoing transitions from this state that stay within this process
    const outgoing = transitions.filter(t =>
      t.from === state.nodeId && processNodeIds.has(t.to)
    );

    if (outgoing.length === 0) {
      // Terminal state: loop back to initial to model process restart.
      // `skip;` is required before `goto` in SPIN 6.x when the label has
      // no other statement (pure goto is a "confusing control structure").
      lines.push(`  skip;`);
      lines.push(`  goto ${initialState.label}`);
    } else if (outgoing.length === 1) {
      const t = outgoing[0];
      const targetLabel = nodeIdToLabel.get(t.to) ?? sanitizeLabel(t.to);
      if (t.condition) {
        // Condition is a guard expression: `(cond);` acts as a blocking wait
        lines.push(`  (${t.condition});`);
      }
      if (t.action) {
        // Action is a Promela statement: assignments, sends, receives
        lines.push(`  ${t.action};`);
      }
      if (!t.condition && !t.action) {
        // No condition or action: SPIN requires a non-label statement before goto
        lines.push(`  skip;`);
      }
      lines.push(`  goto ${targetLabel}`);
    } else {
      // Multiple outgoing transitions: use Promela nondeterministic `if` block.
      // SPIN's `if` selects any enabled option; with conditions, only enabled
      // options are selected. This models Peterson's spin-wait and nondeterminism.
      lines.push(`  if`);
      for (const t of outgoing) {
        const targetLabel = nodeIdToLabel.get(t.to) ?? sanitizeLabel(t.to);
        if (t.condition) {
          if (t.action) {
            lines.push(`  :: (${t.condition}) -> ${t.action}; goto ${targetLabel}`);
          } else {
            lines.push(`  :: (${t.condition}) -> goto ${targetLabel}`);
          }
        } else {
          if (t.action) {
            lines.push(`  :: true -> ${t.action}; goto ${targetLabel}`);
          } else {
            lines.push(`  :: true -> skip; goto ${targetLabel}`);
          }
        }
      }
      lines.push(`  fi`);
    }

    lines.push('');
  }

  lines.push(`}`);

  return lines.join('\n');
}

// ---------------------------------------------------------------------------
// LTL property generator
// ---------------------------------------------------------------------------

/**
 * Generate LTL safety property: no two atomic states (from different processes)
 * can hold simultaneously.
 *
 * [] !(proc_a@label_a && proc_b@label_b)
 */
function generateLTLSafety(
  byProcess: Map<string, ProcessState[]>
): string[] {
  const ltlProps: string[] = [];

  // Collect all (process, atomicState) pairs
  const atomicStates: Array<{ process: string; label: string }> = [];
  for (const [proc, states] of byProcess.entries()) {
    for (const state of states) {
      if (state.atomic) {
        atomicStates.push({ process: proc, label: state.label });
      }
    }
  }

  // Generate pairwise mutual exclusion properties
  for (let i = 0; i < atomicStates.length; i++) {
    for (let j = i + 1; j < atomicStates.length; j++) {
      const a = atomicStates[i];
      const b = atomicStates[j];
      if (a.process !== b.process) {
        const propName = `mutex_${a.process}_${a.label}_${b.process}_${b.label}`;
        ltlProps.push(
          `ltl ${propName} { [] !(${a.process}@${a.label} && ${b.process}@${b.label}) }`
        );
      }
    }
  }

  return ltlProps;
}

// ---------------------------------------------------------------------------
// Main export: generatePromela()
// ---------------------------------------------------------------------------

/**
 * generatePromela — Convert a GraphStore ProcessGraph to a Promela specification.
 *
 * @param store        The GraphStore containing process-state nodes and transition edges.
 * @param processName  Optional override for the default process name (used when the
 *                     graph has no `process` property on transition edges). Defaults
 *                     to 'proc'.
 * @returns            A valid Promela string that SPIN can verify.
 *
 * @example
 * ```typescript
 * const store = new GraphStore(':memory:');
 * await store.addNode('idle',     'process-state', { label: 'idle',     initial: true  });
 * await store.addNode('working',  'process-state', { label: 'working',  atomic: true   });
 * await store.addNode('done',     'process-state', { label: 'done'                     });
 * await store.addEdge('e1', 'idle',    'working', 'transition', {});
 * await store.addEdge('e2', 'working', 'done',    'transition', {});
 * await store.addEdge('e3', 'done',    'idle',    'transition', {});
 *
 * const promela = generatePromela(store, 'worker');
 * // => valid Promela for SPIN
 * ```
 */
export function generatePromela(store: GraphStore, processName: string = 'proc'): string {
  const sections: string[] = [];

  // Header comment
  sections.push(
    `/* Promela specification generated by generatePromela() from GraphStore\n` +
    ` * Generated: ${new Date().toISOString()}\n` +
    ` * SPIN Version 6+ compatible\n` +
    ` */\n`
  );

  // Shared variable declarations
  const sharedVars = extractSharedVars(store);
  if (sharedVars.length > 0) {
    sections.push(sharedVars.join('\n') + '\n');
  }

  // Extract states grouped by process
  let byProcess = extractProcessStates(store);

  // If no process-state nodes found with explicit process grouping,
  // treat all as belonging to the default processName
  if (byProcess.size === 0) {
    // Fallback: treat all nodes as process-states in one process
    const allStates: ProcessState[] = [];
    for (const node of store.nodes.values()) {
      const rawLabel = (node.properties.get('label') as string | undefined) ?? node.id;
      const label = sanitizeLabel(rawLabel);
      const initial = node.properties.get('initial') === true;
      const atomic = node.properties.get('atomic') === true;
      allStates.push({ nodeId: node.id, label, initial, atomic, process: processName });
    }
    byProcess.set(processName, allStates);
  }

  // If only one process group exists and its name is 'proc' (default), rename it
  if (byProcess.size === 1 && byProcess.has('proc') && processName !== 'proc') {
    const states = byProcess.get('proc')!;
    states.forEach(s => { s.process = processName; });
    byProcess.delete('proc');
    byProcess.set(processName, states);
  }

  // Build nodeId → Promela label map (global, across all processes)
  const nodeIdToLabel = new Map<string, string>();
  for (const states of byProcess.values()) {
    for (const state of states) {
      nodeIdToLabel.set(state.nodeId, state.label);
    }
  }

  // Extract all transitions
  const allTransitions = extractTransitions(store);

  // Generate one proctype per process
  for (const [proc, states] of byProcess.entries()) {
    const processNodeIds = new Set(states.map(s => s.nodeId));
    const proctype = generateProctype(proc, states, allTransitions, nodeIdToLabel, processNodeIds);
    sections.push(proctype + '\n');
  }

  // Generate LTL properties
  const ltlProps = generateLTLSafety(byProcess);
  if (ltlProps.length > 0) {
    sections.push('/* LTL safety properties (auto-generated) */');
    sections.push(ltlProps.join('\n') + '\n');
  }

  return sections.join('\n');
}
