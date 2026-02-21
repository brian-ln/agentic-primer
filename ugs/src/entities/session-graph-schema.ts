/**
 * Session Graph Schema
 *
 * Extends the GraphStore node/edge model with session-specific types for
 * representing Claude Code JSONL session logs as a navigable graph.
 *
 * Fork and join are first-class primitives here — not accidents of file
 * system structure. A fork-point is any node with >1 outgoing 'threads-to'
 * edge; a join-point is any node with >1 incoming 'threads-to' edge.
 *
 * These arise naturally when two session files share history (externally
 * constructed forks), or can be injected explicitly by tooling.
 *
 * Design references:
 *   - graph.ts       — GraphStore Node/Edge base types
 *   - session.ts     — SessionLogEntry (primary input type)
 *   - ontology graph.ts — KnowledgeGraph pattern (container shape)
 *   - SESSION_REPLAY_RESEARCH.md — Claude Code JSONL format spec
 */

// ---------------------------------------------------------------------------
// Node types
// ---------------------------------------------------------------------------

/**
 * Session-specific node types.
 *
 * Extends the ontology NodeType ('evidence' | 'claim' | 'concept' | 'source')
 * with types that model the structure of a Claude Code conversation.
 *
 *   session     — A Claude Code session (maps from sessionId in JSONL)
 *   turn        — A conversation turn: one user message + the assistant
 *                 response(s) that follow, grouped by parentUuid chain
 *   fork-point  — A node where the conversation branched (>1 outgoing
 *                 threads-to edge). Does not appear in raw single-session
 *                 JSONL — emerges when multiple sessions share history.
 *   join-point  — A node where two branches converged (>1 incoming
 *                 threads-to edge). Same emergence condition as fork-point.
 *   branch      — A named sequence of turns between a fork and a join.
 */
export type SessionNodeType =
  | 'session'
  | 'turn'
  | 'fork-point'
  | 'join-point'
  | 'branch';

/** All valid SessionNodeType values as a readonly tuple. */
export const SESSION_NODE_TYPES = [
  'session',
  'turn',
  'fork-point',
  'join-point',
  'branch',
] as const;

// ---------------------------------------------------------------------------
// Edge types
// ---------------------------------------------------------------------------

/**
 * Session-specific edge types.
 *
 * Extends the ontology EdgeType with types that encode the temporal and
 * structural relationships between session nodes.
 *
 *   threads-to  — Sequential turn ordering (maps from parentUuid chains).
 *                 The primary structural edge: turn A → turn B means B's
 *                 parentUuid points to A's uuid in the JSONL.
 *   forks-to    — Session branched here. From a fork-point to the first
 *                 turn of each branch. NEW — first-class primitive.
 *   joins-from  — Branches converged here. From the last turn of each
 *                 branch to the join-point. NEW — first-class primitive.
 *   spawned     — Session spawned a sub-agent session (e.g. via Task tool).
 *                 From the parent session node to the child session node.
 *   contains    — Session contains a turn. From the session node to each
 *                 turn node. Provides session-scoped traversal.
 */
export type SessionEdgeType =
  | 'threads-to'
  | 'forks-to'
  | 'joins-from'
  | 'spawned'
  | 'contains';

/** All valid SessionEdgeType values as a readonly tuple. */
export const SESSION_EDGE_TYPES = [
  'threads-to',
  'forks-to',
  'joins-from',
  'spawned',
  'contains',
] as const;

// ---------------------------------------------------------------------------
// SessionNode
// ---------------------------------------------------------------------------

/**
 * A node in the session graph.
 *
 * All nodes have id, type, sessionId, and timestamp. Type-specific fields
 * are optional and populated only when the node's type matches.
 *
 * ID format conventions:
 *   session nodes:    session:{uuid}
 *   turn nodes:       turn:{uuid}        (uuid = JSONL event uuid)
 *   fork-point nodes: fork:{uuid}        (uuid = the JSONL event uuid
 *                                          of the turn that diverged)
 *   join-point nodes: join:{uuid}        (uuid = synthetic, assigned at
 *                                          detection time)
 *   branch nodes:     branch:{uuid}
 */
export interface SessionNode {
  /** Node ID. Format: session:{uuid} | turn:{uuid} | fork:{uuid} | join:{uuid} | branch:{uuid} */
  id: string;

  /** Entity type of this node. */
  type: SessionNodeType;

  /** The Claude Code session UUID this node belongs to. */
  sessionId: string;

  /** ISO 8601 timestamp — event timestamp from JSONL, or construction time for synthetic nodes. */
  timestamp: string;

  // --- Fields for 'turn' nodes ---

  /** Conversation role. Present when type === 'turn'. */
  role?: 'user' | 'assistant';

  /**
   * Truncated content of the turn's primary message (first 200 chars).
   * For user turns: the message text. For assistant turns: the first
   * text block content. Present when type === 'turn'.
   */
  content?: string;

  /**
   * Model ID that generated this response (e.g. 'claude-sonnet-4-5-20250929').
   * Present on assistant turns when type === 'turn'.
   */
  model?: string;

  /**
   * Names of tools invoked in this turn (deduplicated).
   * Present on assistant turns when type === 'turn' and tools were used.
   */
  tools?: string[];

  /**
   * Tool calls with inputs, in invocation order (may have duplicates for repeated tools).
   * Present on assistant turns when type === 'turn' and tools were used.
   */
  toolCalls?: Array<{ name: string; input: Record<string, unknown> }>;

  /**
   * Text content from the assistant response(s) in this turn.
   * Concatenated from all text blocks across the assistant chain.
   * Thinking blocks are encoded as "\x00thinking\x00<content>" for UI rendering.
   */
  assistantContent?: string;

  /**
   * Token usage for this turn's API call.
   * Present on assistant turns when type === 'turn'.
   */
  tokenUsage?: {
    input: number;
    output: number;
  };

  // --- Fields for 'fork-point' nodes ---

  /**
   * IDs of the branch nodes (or turn nodes) created at this fork.
   * Present when type === 'fork-point'.
   */
  branchIds?: string[];

  // --- Fields for 'session' nodes ---

  /**
   * Absolute path of the project directory for this session.
   * Present when type === 'session'.
   */
  projectPath?: string;

  /**
   * Human-readable session slug (e.g. 'glimmering-stargazing-wind').
   * Sourced from the JSONL envelope 'slug' field.
   * Present when type === 'session' and slug was observed.
   */
  slug?: string;

  /**
   * Truncated text of the first user message in the session (first 200 chars).
   * Present when type === 'session'.
   */
  firstPrompt?: string;
}

// ---------------------------------------------------------------------------
// SessionEdge
// ---------------------------------------------------------------------------

/**
 * A directed edge in the session graph.
 *
 * Direction is from → to. For 'threads-to' edges this encodes temporal
 * ordering: from = earlier turn, to = later turn.
 */
export interface SessionEdge {
  /** ID of the source node. */
  from: string;

  /** ID of the target node. */
  to: string;

  /** Relationship type. */
  type: SessionEdgeType;

  /**
   * The original parentUuid value from the JSONL event that produced this
   * edge. Preserved for traceability back to source data.
   * Present on 'threads-to' edges.
   */
  parentUuid?: string;

  /**
   * Which branch this edge belongs to, when the edge is part of a forked
   * subgraph. References a branch node ID (branch:{uuid}).
   * Present when type === 'threads-to' | 'forks-to' | 'joins-from'.
   */
  branchId?: string;
}

// ---------------------------------------------------------------------------
// SessionGraph container
// ---------------------------------------------------------------------------

/**
 * A complete session graph, suitable for serialisation to JSON.
 *
 * Produced by buildSessionGraph() from raw JSONL lines.
 */
export interface SessionGraph {
  /** All nodes in the graph. */
  nodes: SessionNode[];

  /** All directed edges in the graph. */
  edges: SessionEdge[];

  /** ISO 8601 timestamp of when this graph was constructed. */
  builtAt: string;

  /** Graph-level metadata. */
  meta: {
    /** All session UUIDs included in this graph. */
    sessionIds: string[];

    /** Source project directory paths. */
    projectPaths: string[];

    /** Number of fork-point nodes detected. */
    forkCount: number;

    /** Number of join-point nodes detected. */
    joinCount: number;

    /** Human-readable description of the graph's scope. */
    description?: string;
  };
}

// ---------------------------------------------------------------------------
// Internal types for the builder
// ---------------------------------------------------------------------------

/** Typed envelope shared by all Claude Code JSONL events. */
interface JsonlEnvelope {
  type: string;
  uuid: string;
  parentUuid?: string;
  timestamp?: string;
  sessionId?: string;
  isSidechain?: boolean;
  cwd?: string;
  slug?: string;
  requestId?: string;
  /**
   * True on daemon-injected rate-limit events ("You've hit your limit").
   * These are not real API responses and must not become turn nodes.
   */
  isApiErrorMessage?: boolean;
  message?: {
    role?: string;
    content?: unknown;
    /**
     * '<synthetic>' on daemon-injected synthetic messages.
     * These are not real API responses and must not become turn nodes.
     */
    model?: string;
    usage?: {
      input_tokens?: number;
      output_tokens?: number;
      cache_creation_input_tokens?: number;
      cache_read_input_tokens?: number;
    };
  };
}

/** Parsed turn: one user event uuid + associated assistant event uuids. */
interface ParsedTurn {
  uuid: string;          // The user event's uuid (canonical turn ID)
  parentUuid?: string;   // The user event's parentUuid (links to prior turn)
  timestamp: string;
  role: 'user';
  content: string;
  assistantUuids: string[];
  model?: string;
  tools: string[];
  toolCalls: Array<{ name: string; input: Record<string, unknown> }>;
  assistantContent?: string;
  tokenUsage?: { input: number; output: number };
}

// ---------------------------------------------------------------------------
// buildSessionGraph
// ---------------------------------------------------------------------------

/**
 * Maps Claude Code JSONL events into a SessionGraph.
 *
 * Algorithm:
 *   1. Parse all JSONL lines into typed events; skip malformed lines.
 *   2. Filter for type === 'user' and type === 'assistant' events.
 *      Sidechain events are included — the caller may filter if needed.
 *   3. Build a requestId → assistant events index to group response chunks.
 *   4. Group events into turns: each 'user' event is a turn root; 'assistant'
 *      events whose parentUuid traces back to that user event (via the
 *      requestId chain) are part of the same turn.
 *   5. Create a 'session' SessionNode.
 *   6. Create a 'turn' SessionNode for each parsed turn.
 *   7. Emit 'contains' edges from the session node to each turn node.
 *   8. Emit 'threads-to' edges following the parentUuid chain between turns.
 *   9. Detect fork-points: turn nodes with >1 outgoing 'threads-to' edge.
 *      Insert a 'fork-point' node and rewrite the outgoing edges as 'forks-to'.
 *  10. Detect join-points: turn nodes with >1 incoming 'threads-to' edge.
 *      Insert a 'join-point' node and rewrite the incoming edges as 'joins-from'.
 *
 * Notes:
 *   - Claude Code JSONL currently produces a tree (single parent per event),
 *     so fork-point and join-point nodes will not appear in a single session
 *     file. They emerge when sessions share history (externally constructed
 *     forks) or are explicitly injected by tooling.
 *   - Empty sessions and sessions with no user/assistant events produce a
 *     valid SessionGraph with only the session node.
 *   - Malformed JSONL lines are skipped with a console.warn.
 *
 * @param jsonlLines  - Raw JSONL lines from a Claude Code session file.
 * @param sessionId   - The session UUID (used to build node IDs and metadata).
 * @param projectPath - Absolute path of the project directory for this session.
 * @returns A fully constructed SessionGraph.
 */
export function buildSessionGraph(
  jsonlLines: string[],
  sessionId: string,
  projectPath: string
): SessionGraph {
  // --- Step 1: Parse events ---
  const events: JsonlEnvelope[] = [];

  for (let i = 0; i < jsonlLines.length; i++) {
    const line = jsonlLines[i].trim();
    if (!line) continue;

    try {
      const parsed = JSON.parse(line) as JsonlEnvelope;
      events.push(parsed);
    } catch {
      console.warn(`[session-graph] Skipping malformed JSONL line ${i + 1} in session ${sessionId}`);
    }
  }

  // --- Step 2: Split into user and assistant events ---
  // Bug fix: skip daemon-injected synthetic events before any node is created.
  // isApiErrorMessage === true  → rate-limit "You've hit your limit" messages
  // message?.model === '<synthetic>' → daemon-synthesised assistant events
  // Neither should become turn nodes in the graph.
  const isSyntheticEvent = (e: JsonlEnvelope): boolean =>
    e.isApiErrorMessage === true || e.message?.model === '<synthetic>';

  const userEvents = events.filter(e => e.type === 'user' && e.uuid && !isSyntheticEvent(e));
  const assistantEvents = events.filter(e => e.type === 'assistant' && e.uuid && !isSyntheticEvent(e));

  // --- Step 3: Build a parentUuid → assistant events index ---
  // Multiple assistant events share a requestId and form a chain. We index
  // by parentUuid so we can find which user event each assistant event belongs to.
  const assistantByParent = new Map<string, JsonlEnvelope[]>();

  for (const ae of assistantEvents) {
    const parent = ae.parentUuid;
    if (!parent) continue;
    if (!assistantByParent.has(parent)) {
      assistantByParent.set(parent, []);
    }
    assistantByParent.get(parent)!.push(ae);
  }

  // Collect slug from any event that has it (appears after first API call)
  let sessionSlug: string | undefined;
  for (const e of events) {
    if (e.slug) {
      sessionSlug = e.slug;
      break;
    }
  }

  // --- Step 4: Group into turns ---
  // For each user event, collect all assistant events reachable via the
  // parentUuid chain starting from that user event's uuid.
  function collectAssistantChain(
    rootUuid: string,
    visited: Set<string> = new Set()
  ): JsonlEnvelope[] {
    if (visited.has(rootUuid)) return [];
    visited.add(rootUuid);

    const directChildren = assistantByParent.get(rootUuid) ?? [];
    const result: JsonlEnvelope[] = [...directChildren];

    for (const child of directChildren) {
      result.push(...collectAssistantChain(child.uuid, visited));
    }

    return result;
  }

  const turns: ParsedTurn[] = [];
  // Track which user event UUIDs we've already converted to turns, so that a
  // JSONL file that contains the same event UUID more than once (possible after
  // an interrupted compaction + resume) does not produce duplicate turn nodes
  // or duplicate 'contains' / 'threads-to' edges.
  const seenUserUuids = new Set<string>();

  for (const ue of userEvents) {
    if (seenUserUuids.has(ue.uuid)) continue;
    seenUserUuids.add(ue.uuid);
    const assistantChain = collectAssistantChain(ue.uuid);

    // Extract text content from user event
    let userContent = '';
    if (ue.message?.content) {
      const content = ue.message.content;
      if (typeof content === 'string') {
        userContent = content.slice(0, 200);
      } else if (Array.isArray(content)) {
        // Tool result array — take first string-like element
        for (const block of content as unknown[]) {
          if (block && typeof block === 'object') {
            const b = block as Record<string, unknown>;
            if (b.type === 'tool_result' && typeof b.content === 'string') {
              userContent = (b.content as string).slice(0, 200);
              break;
            }
          }
        }
        if (!userContent) {
          userContent = '[tool result]';
        }
      }
    }

    // Aggregate model, tools, token usage, and assistant text from assistant chain
    let model: string | undefined;
    const toolNames = new Set<string>();
    const toolCallsList: Array<{ name: string; input: Record<string, unknown> }> = [];
    const assistantTextParts: string[] = [];
    let inputTokens = 0;
    let outputTokens = 0;

    for (const ae of assistantChain) {
      if (!model && ae.message?.model) {
        model = ae.message.model;
      }

      const usage = ae.message?.usage;
      if (usage) {
        inputTokens += (usage.input_tokens ?? 0) +
          (usage.cache_creation_input_tokens ?? 0) +
          (usage.cache_read_input_tokens ?? 0);
        outputTokens += usage.output_tokens ?? 0;
      }

      const contentBlocks = ae.message?.content;
      if (Array.isArray(contentBlocks)) {
        for (const block of contentBlocks as unknown[]) {
          if (block && typeof block === 'object') {
            const b = block as Record<string, unknown>;
            if (b.type === 'tool_use' && typeof b.name === 'string') {
              toolNames.add(b.name as string);
              const input = (b.input && typeof b.input === 'object')
                ? b.input as Record<string, unknown>
                : {};
              toolCallsList.push({ name: b.name as string, input });
            } else if (b.type === 'text' && typeof b.text === 'string' && b.text.trim()) {
              assistantTextParts.push(b.text.trim());
            } else if (b.type === 'thinking' && typeof b.thinking === 'string' && b.thinking.trim()) {
              // Prefix thinking blocks so UI can render them differently
              assistantTextParts.push('\x00thinking\x00' + b.thinking.trim());
            }
          }
        }
      }
    }

    // Concatenate assistant text — store full content (no truncation)
    const assistantContentRaw = assistantTextParts.join('\n\n');
    const assistantContent = assistantContentRaw || undefined;

    const tokenUsage = (inputTokens > 0 || outputTokens > 0)
      ? { input: inputTokens, output: outputTokens }
      : undefined;

    turns.push({
      uuid: ue.uuid,
      parentUuid: ue.parentUuid,
      timestamp: ue.timestamp ?? new Date().toISOString(),
      role: 'user',
      content: userContent,
      assistantUuids: assistantChain.map(ae => ae.uuid),
      model,
      tools: Array.from(toolNames),
      toolCalls: toolCallsList,
      assistantContent,
      tokenUsage,
    });
  }

  // Determine first prompt
  const firstTurn = turns[0];
  const firstPrompt = firstTurn ? firstTurn.content.slice(0, 200) : undefined;

  // --- Step 5 & 6: Build session node and turn nodes ---
  const nodes: SessionNode[] = [];
  const edges: SessionEdge[] = [];

  const sessionNodeId = `session:${sessionId}`;

  const sessionNode: SessionNode = {
    id: sessionNodeId,
    type: 'session',
    sessionId,
    timestamp: turns[0]?.timestamp ?? new Date().toISOString(),
    projectPath,
    slug: sessionSlug,
    firstPrompt,
  };
  nodes.push(sessionNode);

  // Map from JSONL uuid → turn node ID, for edge construction.
  // We register BOTH the user event uuid AND all assistant event uuids for each
  // turn. This is required because the parentUuid of a subsequent user event
  // points to an assistant event uuid (the last response in the chain), not
  // directly to another user event uuid.
  const uuidToTurnNodeId = new Map<string, string>();

  for (const turn of turns) {
    const turnNodeId = `turn:${turn.uuid}`;
    uuidToTurnNodeId.set(turn.uuid, turnNodeId);
    // Register all assistant uuids that belong to this turn so that the next
    // user event's parentUuid (which references an assistant uuid) can be
    // resolved back to this turn node.
    for (const assistantUuid of turn.assistantUuids) {
      uuidToTurnNodeId.set(assistantUuid, turnNodeId);
    }

    const turnNode: SessionNode = {
      id: turnNodeId,
      type: 'turn',
      sessionId,
      timestamp: turn.timestamp,
      role: turn.role,
      content: turn.content,
      ...(turn.model !== undefined && { model: turn.model }),
      ...(turn.tools.length > 0 && { tools: turn.tools }),
      ...(turn.toolCalls.length > 0 && { toolCalls: turn.toolCalls }),
      ...(turn.assistantContent !== undefined && { assistantContent: turn.assistantContent }),
      ...(turn.tokenUsage !== undefined && { tokenUsage: turn.tokenUsage }),
    };
    nodes.push(turnNode);

    // Step 7: 'contains' edge from session to turn
    edges.push({
      from: sessionNodeId,
      to: turnNodeId,
      type: 'contains',
    });
  }

  // --- Step 8: 'threads-to' edges from parentUuid chains ---
  // Bug fix: after an interrupted compaction + session resume a user event can
  // appear as the parentUuid of more than one child turn (resume-induced branch).
  // We emit one threads-to edge per unique (from, to) pair only — duplicate pairs
  // are silently dropped here. The fork-point detection in Step 9 then promotes
  // any source node with >1 distinct outgoing edges into a fork-point, which is
  // the correct representation of such a resume-induced branch.
  const seenThreadsToEdges = new Set<string>();

  for (const turn of turns) {
    if (turn.parentUuid) {
      const fromTurnNodeId = uuidToTurnNodeId.get(turn.parentUuid);
      const toTurnNodeId = uuidToTurnNodeId.get(turn.uuid);

      // Only emit if both ends are known turn nodes (skip edges to/from
      // non-turn events like system, progress, etc.)
      if (fromTurnNodeId && toTurnNodeId) {
        const edgeKey = `${fromTurnNodeId}→${toTurnNodeId}`;
        if (seenThreadsToEdges.has(edgeKey)) {
          // Duplicate edge — skip to keep the graph a valid DAG
          continue;
        }
        seenThreadsToEdges.add(edgeKey);
        edges.push({
          from: fromTurnNodeId,
          to: toTurnNodeId,
          type: 'threads-to',
          parentUuid: turn.parentUuid,
        });
      }
    }
  }

  // --- Steps 9 & 10: Detect fork-points and join-points ---
  // A source node with >1 outgoing 'threads-to' edges is a fork-point.  This
  // happens legitimately when an interrupted compaction + session resume causes
  // two distinct child turns to share the same parentUuid.  The fork-point node
  // is inserted here and the outgoing edges are rewritten to 'forks-to', so the
  // resulting graph is a valid DAG rather than a node with duplicate edges.

  // Build adjacency counts for 'threads-to' edges
  const outgoingCount = new Map<string, number>();
  const incomingCount = new Map<string, number>();

  for (const edge of edges) {
    if (edge.type !== 'threads-to') continue;

    outgoingCount.set(edge.from, (outgoingCount.get(edge.from) ?? 0) + 1);
    incomingCount.set(edge.to, (incomingCount.get(edge.to) ?? 0) + 1);
  }

  let forkCount = 0;
  let joinCount = 0;

  // Fork-points: nodes with >1 outgoing 'threads-to' edge
  for (const [nodeId, count] of outgoingCount) {
    if (count <= 1) continue;

    forkCount++;
    const forkNodeId = `fork:${nodeId.replace(/^turn:/, '')}`;

    // Collect the outgoing edges that will become 'forks-to' edges
    const outgoingEdges = edges.filter(
      e => e.type === 'threads-to' && e.from === nodeId
    );

    const forkNode: SessionNode = {
      id: forkNodeId,
      type: 'fork-point',
      sessionId,
      timestamp: new Date().toISOString(),
      branchIds: outgoingEdges.map(e => e.to),
    };
    nodes.push(forkNode);

    // Insert exactly ONE 'threads-to' edge from the original node to the fork node.
    // (One edge regardless of how many children — emitting one per child would
    // create duplicate edges for the same from→to pair.)
    edges.push({
      from: nodeId,
      to: forkNodeId,
      type: 'threads-to',
      parentUuid: outgoingEdges[0]?.parentUuid,
    });

    // Rewrite the outgoing 'threads-to' edges to 'forks-to' from the fork node
    for (const edge of outgoingEdges) {
      edge.type = 'forks-to';
      // Update the fork edge's source to be the fork node
      edge.from = forkNodeId;
    }
  }

  // Join-points: nodes with >1 incoming 'threads-to' or 'forks-to' edge
  // Recompute incoming counts after fork rewrites
  const incomingAfterFork = new Map<string, number>();

  for (const edge of edges) {
    if (edge.type !== 'threads-to' && edge.type !== 'forks-to') continue;
    incomingAfterFork.set(edge.to, (incomingAfterFork.get(edge.to) ?? 0) + 1);
  }

  for (const [nodeId, count] of incomingAfterFork) {
    if (count <= 1) continue;

    joinCount++;
    const joinNodeId = `join:${nodeId.replace(/^turn:/, '')}`;

    // Collect the incoming edges that will become 'joins-from' edges
    const incomingEdges = edges.filter(
      e => (e.type === 'threads-to' || e.type === 'forks-to') && e.to === nodeId
    );

    const joinNode: SessionNode = {
      id: joinNodeId,
      type: 'join-point',
      sessionId,
      timestamp: new Date().toISOString(),
    };
    nodes.push(joinNode);

    // Rewrite the incoming edges to 'joins-from' pointing to the join node
    for (const edge of incomingEdges) {
      edge.type = 'joins-from';
      edge.to = joinNodeId;
    }

    // Add a single 'threads-to' edge from the join node to the original target
    edges.push({
      from: joinNodeId,
      to: nodeId,
      type: 'threads-to',
    });
  }

  // --- Assemble and return ---
  return {
    nodes,
    edges,
    builtAt: new Date().toISOString(),
    meta: {
      sessionIds: [sessionId],
      projectPaths: [projectPath],
      forkCount,
      joinCount,
    },
  };
}

// ---------------------------------------------------------------------------
// Type guards
// ---------------------------------------------------------------------------

/**
 * Returns true if the value is a valid SessionNodeType.
 */
export function isSessionNodeType(value: unknown): value is SessionNodeType {
  return (
    typeof value === 'string' &&
    (SESSION_NODE_TYPES as readonly string[]).includes(value)
  );
}

/**
 * Returns true if the value is a valid SessionEdgeType.
 */
export function isSessionEdgeType(value: unknown): value is SessionEdgeType {
  return (
    typeof value === 'string' &&
    (SESSION_EDGE_TYPES as readonly string[]).includes(value)
  );
}
