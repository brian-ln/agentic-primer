// ugfm-claim5.ts
// UGFM Claim 5 demonstration: LLM-orchestrated deterministic graph tools
//
// Architecture: orchestrate(question, store) does ONLY semantic routing.
// It contains no graph reasoning — all computation is in deterministic functions.
// This is the key invariant: LLM selects the tool; graph returns the result.

import { GraphStore } from './graph';

export type AlgorithmName =
  | 'findSCCs'
  | 'hasCycle'
  | 'traverse'
  | 'getByType'
  | 'unknown';

export interface OrchestrationResult {
  question: string;
  algorithm: AlgorithmName;
  result: unknown;
  deterministic: true;  // Always true — the result is graph-determined
  routingPattern: string;  // Which regex matched (for transparency/audit)
}

export interface OrchestratorStats {
  totalCalls: number;
  algorithmDistribution: Record<AlgorithmName, number>;
}

// ROUTING TABLE: This is the ONLY logic in the orchestrator.
// An LLM would produce this mapping; here it is encoded as regex patterns.
// The patterns demonstrate what semantic routing looks like structurally.
const ROUTING_TABLE: Array<{ pattern: RegExp; algorithm: AlgorithmName; description: string }> = [
  { pattern: /deadlock|strongly.connected/i,             algorithm: 'findSCCs',   description: 'cycle/SCC detection' },
  { pattern: /reachable|connected|path|can.*reach|reach.*from/i, algorithm: 'traverse',  description: 'reachability traversal' },
  { pattern: /actor|worker|role|type|what.kind/i,         algorithm: 'getByType',  description: 'node type query' },
  { pattern: /cycle|circular/i,                           algorithm: 'hasCycle',   description: 'boolean cycle check' },
];

/**
 * Orchestrate: route a natural-language question to a deterministic graph algorithm.
 *
 * This function contains NO graph reasoning. It only:
 * 1. Pattern-matches the question to select an algorithm
 * 2. Calls the selected algorithm on the store
 * 3. Returns the result with metadata
 *
 * The result is deterministic because the graph algorithms are deterministic.
 * The routing is deterministic because the regex patterns are deterministic.
 */
export function orchestrate(question: string, store: GraphStore): OrchestrationResult {
  // Step 1: Semantic routing (LLM's role — here mocked as regex)
  const route = ROUTING_TABLE.find(r => r.pattern.test(question));
  const algorithm = route?.algorithm ?? 'unknown';
  const routingPattern = route?.description ?? 'no pattern matched';

  // Step 2: Deterministic algorithm execution (graph tool's role)
  let result: unknown;
  switch (algorithm) {
    case 'findSCCs':
      result = store.findSCCs();
      break;
    case 'hasCycle':
      result = store.hasCycle();
      break;
    case 'traverse': {
      // Use the first node in the store as traversal root.
      // A real LLM would extract entity names from the question to identify a specific
      // start node. Here we use first node as a deterministic proxy, since the
      // orchestrator's role is routing only — not graph reasoning.
      const nodes = [...store.nodes.values()];
      result = nodes.length > 0
        ? store.traverse(nodes[0].id).map((entry) => entry.node.id)
        : [];
      break;
    }
    case 'getByType': {
      // Extract type from question keywords.
      // A real LLM would perform named-entity recognition; here we use a regex heuristic.
      const typeMatch = question.match(/actor|worker|task|concept|knowledge/i);
      const nodeType = typeMatch ? capitalize(typeMatch[0]) : 'Actor';
      result = store.getByType(nodeType).map((n) => n.id);
      break;
    }
    default:
      result = null;
  }

  return {
    question,
    algorithm,
    result,
    deterministic: true,
    routingPattern,
  };
}

function capitalize(s: string): string {
  return s.charAt(0).toUpperCase() + s.slice(1).toLowerCase();
}

/**
 * Create a stats tracker for analyzing orchestrator behavior.
 * Demonstrates that the orchestrator's routing is auditable.
 */
export function createOrchestratorTracker() {
  const calls: OrchestrationResult[] = [];

  return {
    call(question: string, store: GraphStore): OrchestrationResult {
      const result = orchestrate(question, store);
      calls.push(result);
      return result;
    },
    stats(): OrchestratorStats {
      const dist: Record<AlgorithmName, number> = {
        findSCCs: 0, hasCycle: 0, traverse: 0, getByType: 0, unknown: 0,
      };
      for (const c of calls) dist[c.algorithm]++;
      return { totalCalls: calls.length, algorithmDistribution: dist };
    },
    history(): OrchestrationResult[] {
      return [...calls];
    },
  };
}

/**
 * orchestrateWithLLM: use Claude Haiku to classify the question into an algorithm,
 * then execute the deterministic graph algorithm.
 *
 * The LLM performs semantic routing; the graph tool does deterministic computation.
 * This is the live demonstration of Claim 5's architecture with a real LLM.
 *
 * Requires ANTHROPIC_API_KEY in environment. Throws if not set.
 */
export async function orchestrateWithLLM(
  question: string,
  store: GraphStore
): Promise<OrchestrationResult> {
  const apiKey = process.env.ANTHROPIC_API_KEY;
  if (!apiKey) throw new Error('ANTHROPIC_API_KEY not set');

  const systemPrompt =
    'You are a graph algorithm selector. Given a question about a graph system, ' +
    'select exactly one algorithm. Reply with ONLY the algorithm name, nothing else. ' +
    'Valid algorithms: findSCCs, hasCycle, traverse, getByType';

  const response = await fetch('https://api.anthropic.com/v1/messages', {
    method: 'POST',
    headers: {
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'content-type': 'application/json',
    },
    body: JSON.stringify({
      model: 'claude-haiku-4-5-20251001',
      max_tokens: 20,
      system: systemPrompt,
      messages: [{ role: 'user', content: question }],
    }),
  });

  if (!response.ok) {
    throw new Error(`Anthropic API error: ${response.status}`);
  }

  const data = await response.json() as { content: Array<{ text: string }> };
  const rawAlgorithm = data.content[0]?.text?.trim() ?? 'unknown';

  // Validate the LLM returned a known algorithm name
  const validAlgorithms: AlgorithmName[] = ['findSCCs', 'hasCycle', 'traverse', 'getByType'];
  const algorithm: AlgorithmName = validAlgorithms.includes(rawAlgorithm as AlgorithmName)
    ? rawAlgorithm as AlgorithmName
    : 'unknown';

  // Execute the deterministic graph algorithm (same logic as orchestrate() switch statement)
  let result: unknown;
  switch (algorithm) {
    case 'findSCCs':
      result = store.findSCCs();
      break;
    case 'hasCycle':
      result = store.hasCycle();
      break;
    case 'traverse': {
      // Use first node as traversal root.
      // Real LLM would extract entity names from question; here we use first node as proxy.
      const nodes = [...store.nodes.values()];
      result = nodes.length > 0
        ? store.traverse(nodes[0].id).map((entry) => entry.node.id)
        : [];
      break;
    }
    case 'getByType': {
      const typeMatch = question.match(/actor|worker|task|concept|knowledge/i);
      const nodeType = typeMatch ? capitalize(typeMatch[0]) : 'Actor';
      result = store.getByType(nodeType).map(n => n.id);
      break;
    }
    default:
      result = null;
  }

  return {
    question,
    algorithm,
    result,
    deterministic: true,  // Graph algorithm is deterministic; LLM routing is best-effort
    routingPattern: `llm-selected:${rawAlgorithm}`,
  };
}
