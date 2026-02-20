/**
 * Session Graph Parser
 *
 * Reads Claude Code .jsonl files from disk and produces validated SessionGraph
 * instances. Handles file discovery, multi-session merging, and C4 constraint
 * validation.
 *
 * Design references:
 *   - session-graph-schema.ts  — types and buildSessionGraph()
 *   - SESSION_REPLAY_RESEARCH.md — JSONL format spec
 *   - CONSTRAINTS.md           — C1, C4 constraints
 */

import { readFileSync, readdirSync, statSync, existsSync } from 'fs';
import { join, basename } from 'path';
import { homedir } from 'os';
import { buildSessionGraph } from './session-graph-schema.js';
import type { SessionGraph, SessionNode, SessionEdge } from './session-graph-schema.js';

// ---------------------------------------------------------------------------
// Path encoding/decoding utilities
// ---------------------------------------------------------------------------

/**
 * Encode an absolute project path to the ~/.claude/projects/ directory name.
 * E.g. /Users/bln/play/foo → -Users-bln-play-foo
 */
function encodeProjectPath(projectPath: string): string {
  // Replace all '/' with '-', which naturally prepends '-' because absolute
  // paths start with '/'.
  return projectPath.replace(/\//g, '-');
}

/**
 * Extract the project path from JSONL content by reading the `cwd` field from
 * the first event that has one. This is the authoritative source because the
 * encoded directory name is lossy (hyphens in directory names are
 * indistinguishable from path separators after encoding).
 *
 * Falls back to empty string if no `cwd` field is found.
 */
function projectPathFromJsonlContent(lines: string[]): string {
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    try {
      const event = JSON.parse(trimmed) as Record<string, unknown>;
      if (typeof event.cwd === 'string' && event.cwd) {
        return event.cwd;
      }
    } catch {
      // Skip malformed lines
    }
  }
  return '';
}

/**
 * Derive the project path from a JSONL file path as a last-resort fallback.
 *
 * The encoding is: replace '/' with '-' in the absolute path.
 * The decoding replaces '-' with '/' which is lossy for paths with hyphens.
 * Prefer projectPathFromJsonlContent() instead.
 *
 * ~/.claude/projects/-Users-bln-play-foo/<uuid>.jsonl → /Users/bln/play/foo
 */
function projectPathFromFilePath(filePath: string): string {
  // filePath: /Users/bln/.claude/projects/-Users-bln-play-foo/<uuid>.jsonl
  const parts = filePath.split('/');
  // Find the 'projects' segment and take the next part as the encoded name.
  const projectsIdx = parts.lastIndexOf('projects');
  if (projectsIdx !== -1 && projectsIdx + 1 < parts.length) {
    const encoded = parts[projectsIdx + 1];
    // Lossy decode: replace all '-' with '/' to recover the path.
    // Works only for paths without literal hyphens in component names.
    return encoded.replace(/-/g, '/');
  }
  // Fallback: cannot decode, return empty string
  return '';
}

/**
 * Extract the session UUID from a JSONL file path.
 *
 * ~/.claude/projects/.../<uuid>.jsonl → <uuid>
 */
function sessionIdFromFilePath(filePath: string): string {
  const filename = basename(filePath);
  // Strip the .jsonl extension
  return filename.replace(/\.jsonl$/, '');
}

// ---------------------------------------------------------------------------
// parseSessionFile
// ---------------------------------------------------------------------------

/**
 * Parse a Claude Code .jsonl file from disk into a SessionGraph.
 *
 * This is the primary entry point for file-based parsing.
 *
 * @throws Error with a descriptive message on file not found or permission errors.
 */
export async function parseSessionFile(filePath: string): Promise<SessionGraph> {
  let content: string;

  try {
    content = readFileSync(filePath, 'utf-8');
  } catch (err: unknown) {
    if (err instanceof Error) {
      const nodeErr = err as NodeJS.ErrnoException;
      if (nodeErr.code === 'ENOENT') {
        throw new Error(`Session file not found: ${filePath}`);
      }
      if (nodeErr.code === 'EACCES') {
        throw new Error(`Permission denied reading session file: ${filePath}`);
      }
      throw new Error(`Failed to read session file ${filePath}: ${err.message}`);
    }
    throw new Error(`Failed to read session file ${filePath}: unknown error`);
  }

  const lines = content.split('\n');
  const sessionId = sessionIdFromFilePath(filePath);

  // Derive project path: prefer the authoritative `cwd` field from JSONL events
  // over the lossy file-path decode (which breaks for paths containing hyphens).
  const projectPathFromContent = projectPathFromJsonlContent(lines);
  const projectPath = projectPathFromContent || projectPathFromFilePath(filePath);

  const graph = buildSessionGraph(lines, sessionId, projectPath);

  // Ensure meta.projectPaths contains the decoded path (buildSessionGraph sets it
  // from the projectPath argument; this guards against an empty fallback).
  if (projectPath && !graph.meta.projectPaths.includes(projectPath)) {
    graph.meta.projectPaths.push(projectPath);
  }

  return graph;
}

// ---------------------------------------------------------------------------
// parseSessionFiles
// ---------------------------------------------------------------------------

/**
 * Parse multiple session files and merge into one graph.
 *
 * Sessions are linked by shared project path. Cross-session links are detected
 * by looking for parentUuid references that cross session boundaries, and a
 * `spawned` edge is emitted for each detected cross-session link.
 */
export async function parseSessionFiles(filePaths: string[]): Promise<SessionGraph> {
  if (filePaths.length === 0) {
    return {
      nodes: [],
      edges: [],
      builtAt: new Date().toISOString(),
      meta: {
        sessionIds: [],
        projectPaths: [],
        forkCount: 0,
        joinCount: 0,
      },
    };
  }

  if (filePaths.length === 1) {
    return parseSessionFile(filePaths[0]);
  }

  // Parse all files independently
  const graphs: SessionGraph[] = [];
  for (const fp of filePaths) {
    try {
      const g = await parseSessionFile(fp);
      graphs.push(g);
    } catch (err: unknown) {
      const msg = err instanceof Error ? err.message : String(err);
      console.warn(`[session-graph-parser] Skipping file ${fp}: ${msg}`);
    }
  }

  if (graphs.length === 0) {
    return {
      nodes: [],
      edges: [],
      builtAt: new Date().toISOString(),
      meta: {
        sessionIds: [],
        projectPaths: [],
        forkCount: 0,
        joinCount: 0,
      },
    };
  }

  // Merge: combine nodes and edges, deduplicate session nodes by sessionId
  const seenSessionIds = new Set<string>();
  const mergedNodes: SessionNode[] = [];
  const mergedEdges: SessionEdge[] = [];

  for (const graph of graphs) {
    for (const node of graph.nodes) {
      if (node.type === 'session') {
        if (seenSessionIds.has(node.sessionId)) {
          // Skip duplicate session nodes
          continue;
        }
        seenSessionIds.add(node.sessionId);
      }
      mergedNodes.push(node);
    }
    for (const edge of graph.edges) {
      mergedEdges.push(edge);
    }
  }

  // Collect all UUIDs from turn nodes across all sessions, keyed by uuid → sessionId
  // A turn node id has the form "turn:<uuid>"; extract the uuid part.
  const uuidToSessionId = new Map<string, string>();
  for (const node of mergedNodes) {
    if (node.type === 'turn' && node.id.startsWith('turn:')) {
      const uuid = node.id.slice('turn:'.length);
      uuidToSessionId.set(uuid, node.sessionId);
    }
  }

  // Detect cross-session links via parentUuid on threads-to edges.
  // A threads-to edge is cross-session when from.sessionId !== to.sessionId.
  const crossSessionEdges: SessionEdge[] = [];
  const seenSpawnedPairs = new Set<string>();

  for (const edge of mergedEdges) {
    if (edge.type !== 'threads-to') continue;
    if (!edge.parentUuid) continue;

    // Find the sessions of the from and to nodes
    const fromNode = mergedNodes.find(n => n.id === edge.from);
    const toNode = mergedNodes.find(n => n.id === edge.to);

    if (!fromNode || !toNode) continue;
    if (fromNode.sessionId === toNode.sessionId) continue;

    // Cross-session reference: emit a spawned edge from parent session → child session
    const parentSessionNodeId = `session:${fromNode.sessionId}`;
    const childSessionNodeId = `session:${toNode.sessionId}`;
    const pairKey = `${parentSessionNodeId}→${childSessionNodeId}`;

    if (!seenSpawnedPairs.has(pairKey)) {
      seenSpawnedPairs.add(pairKey);
      crossSessionEdges.push({
        from: parentSessionNodeId,
        to: childSessionNodeId,
        type: 'spawned',
      });
    }
  }

  mergedEdges.push(...crossSessionEdges);

  // Aggregate metadata
  const allSessionIds = Array.from(new Set(graphs.flatMap(g => g.meta.sessionIds)));
  const allProjectPaths = Array.from(new Set(graphs.flatMap(g => g.meta.projectPaths).filter(Boolean)));
  const totalForkCount = graphs.reduce((sum, g) => sum + g.meta.forkCount, 0);
  const totalJoinCount = graphs.reduce((sum, g) => sum + g.meta.joinCount, 0);

  return {
    nodes: mergedNodes,
    edges: mergedEdges,
    builtAt: new Date().toISOString(),
    meta: {
      sessionIds: allSessionIds,
      projectPaths: allProjectPaths,
      forkCount: totalForkCount,
      joinCount: totalJoinCount,
    },
  };
}

// ---------------------------------------------------------------------------
// discoverSessionFiles
// ---------------------------------------------------------------------------

/**
 * Discover all session files for a project directory.
 *
 * Maps projectPath → ~/.claude/projects/<encoded-path>/ and returns all .jsonl
 * files found there, sorted by mtime (most recent first).
 *
 * Excludes sessions-index.json and any subdirectories (subagent sessions).
 */
export async function discoverSessionFiles(projectPath: string): Promise<string[]> {
  const encoded = encodeProjectPath(projectPath);
  const claudeProjectsDir = join(homedir(), '.claude', 'projects', encoded);

  if (!existsSync(claudeProjectsDir)) {
    return [];
  }

  let entries: string[];
  try {
    entries = readdirSync(claudeProjectsDir);
  } catch (err: unknown) {
    const msg = err instanceof Error ? err.message : String(err);
    console.warn(`[session-graph-parser] Cannot read project directory ${claudeProjectsDir}: ${msg}`);
    return [];
  }

  const jsonlFiles: Array<{ path: string; mtime: number }> = [];

  for (const entry of entries) {
    // Skip sessions-index.json and non-.jsonl files
    if (!entry.endsWith('.jsonl')) continue;

    const fullPath = join(claudeProjectsDir, entry);

    let stat;
    try {
      stat = statSync(fullPath);
    } catch {
      continue;
    }

    // Skip directories (subagent sessions are in subdirectories)
    if (stat.isDirectory()) continue;

    jsonlFiles.push({ path: fullPath, mtime: stat.mtimeMs });
  }

  // Sort by mtime descending (most recent first)
  jsonlFiles.sort((a, b) => b.mtime - a.mtime);

  return jsonlFiles.map(f => f.path);
}

// ---------------------------------------------------------------------------
// validateSessionGraph
// ---------------------------------------------------------------------------

/**
 * Validate a SessionGraph against C4 constraints.
 *
 * Returns a list of violation strings. An empty array means the graph is valid.
 *
 * Checks:
 *   C4.1-style: every threads-to edge's `from` node exists in the nodes array
 *   C4.2-style: every threads-to edge's `to` node exists in the nodes array
 *   C4.3-style: no cycles in threads-to edges (sessions are DAGs, not cycles)
 *   C4.4-style: session nodes have at least a sessionId
 */
export function validateSessionGraph(graph: SessionGraph): string[] {
  const violations: string[] = [];
  const nodeIds = new Set(graph.nodes.map(n => n.id));

  // C4.1: every threads-to edge's `from` node exists
  for (const edge of graph.edges) {
    if (edge.type !== 'threads-to') continue;
    if (!nodeIds.has(edge.from)) {
      violations.push(
        `C4.1 violation: threads-to edge references non-existent 'from' node: ${edge.from} → ${edge.to}`
      );
    }
  }

  // C4.2: every threads-to edge's `to` node exists
  for (const edge of graph.edges) {
    if (edge.type !== 'threads-to') continue;
    if (!nodeIds.has(edge.to)) {
      violations.push(
        `C4.2 violation: threads-to edge references non-existent 'to' node: ${edge.from} → ${edge.to}`
      );
    }
  }

  // C4.3: no cycles in threads-to edges (DFS cycle detection)
  const threadsToAdj = new Map<string, string[]>();
  for (const edge of graph.edges) {
    if (edge.type !== 'threads-to') continue;
    if (!threadsToAdj.has(edge.from)) {
      threadsToAdj.set(edge.from, []);
    }
    threadsToAdj.get(edge.from)!.push(edge.to);
  }

  // DFS with three-color marking: white (unvisited), gray (in-stack), black (done)
  const WHITE = 0, GRAY = 1, BLACK = 2;
  const color = new Map<string, number>();

  function dfs(nodeId: string): boolean {
    color.set(nodeId, GRAY);
    for (const neighbor of (threadsToAdj.get(nodeId) ?? [])) {
      const neighborColor = color.get(neighbor) ?? WHITE;
      if (neighborColor === GRAY) {
        // Back edge — cycle detected
        return true;
      }
      if (neighborColor === WHITE) {
        if (dfs(neighbor)) return true;
      }
    }
    color.set(nodeId, BLACK);
    return false;
  }

  for (const nodeId of nodeIds) {
    if ((color.get(nodeId) ?? WHITE) === WHITE) {
      if (dfs(nodeId)) {
        violations.push(
          `C4.3 violation: cycle detected in threads-to edges reachable from node: ${nodeId}`
        );
        // Report once per starting node — break to avoid duplicate messages
        break;
      }
    }
  }

  // C4.4: session nodes have at least a sessionId (non-empty string)
  for (const node of graph.nodes) {
    if (node.type === 'session') {
      if (!node.sessionId || node.sessionId.trim() === '') {
        violations.push(
          `C4.4 violation: session node missing or empty sessionId: ${node.id}`
        );
      }
    }
  }

  return violations;
}
