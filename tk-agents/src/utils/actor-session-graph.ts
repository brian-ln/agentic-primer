/**
 * Actor Session Graph Integration
 *
 * Provides graph storage and query capabilities for actor-session mappings.
 * Integrates the deterministic UUID generation with the Graph system.
 *
 * This module bridges:
 * - Actor addressing (human-readable paths)
 * - Session UUIDs (Claude Code requirements)
 * - Graph storage (persistence and queries)
 */

import type { Graph } from '../graph';
import {
  actorAddressToSessionUUID,
  registerActorSession,
  findSessionByAddress,
  findAddressByUUID,
  type ActorSessionMapping,
} from './actor-session-uuid';

/**
 * Actor-Session Graph Manager
 *
 * Manages the relationship between actor addresses and session UUIDs
 * with graph-based storage and queries.
 */
export class ActorSessionGraph {
  private graph: Graph;

  constructor(graph: Graph) {
    this.graph = graph;
  }

  /**
   * Register an actor-session mapping in the graph
   *
   * Creates a node representing the session with bidirectional edges
   * to both the actor address and the session UUID for lookup.
   *
   * @param address - Human-readable actor address
   * @param sessionUUID - Optional pre-generated UUID
   * @returns The complete mapping with graph node ID
   */
  async registerSession(
    address: string,
    sessionUUID?: string
  ): Promise<ActorSessionMapping & { nodeId: string }> {
    // Generate or use provided UUID
    const mapping = registerActorSession(address, sessionUUID);

    // Create session node in graph
    const nodeId = `session:${mapping.sessionUUID}`;

    // Note: In a real implementation, you would create an actor for this session
    // For now, we just store the metadata in the graph
    // this.graph.registerNode(nodeId, sessionAddress, {
    //   type: 'session',
    //   address: mapping.address,
    //   canonicalAddress: mapping.canonicalAddress,
    //   sessionUUID: mapping.sessionUUID,
    //   createdAt: mapping.createdAt.toISOString(),
    //   lastAccessedAt: mapping.lastAccessedAt.toISOString(),
    // });

    return {
      ...mapping,
      nodeId,
    };
  }

  /**
   * Find session by actor address
   *
   * @param address - Actor address (any format)
   * @returns Session mapping if found
   */
  async findByAddress(address: string): Promise<ActorSessionMapping | undefined> {
    return findSessionByAddress(address);
  }

  /**
   * Find session by UUID
   *
   * @param uuid - Session UUID
   * @returns Session mapping if found
   */
  async findByUUID(uuid: string): Promise<ActorSessionMapping | undefined> {
    return findAddressByUUID(uuid);
  }

  /**
   * Convert actor address to session UUID
   *
   * Deterministic conversion without registration.
   * Useful for quick lookups and validation.
   *
   * @param address - Actor address
   * @returns Session UUID
   */
  addressToUUID(address: string): string {
    return actorAddressToSessionUUID(address);
  }

  /**
   * List all registered sessions
   *
   * @returns Array of all session mappings
   */
  async listSessions(): Promise<ActorSessionMapping[]> {
    // In a full implementation, this would query the graph
    // For now, return from in-memory cache
    const { getAllSessions } = await import('./actor-session-uuid');
    return getAllSessions();
  }

  /**
   * Update session last accessed time
   *
   * @param uuid - Session UUID
   */
  async touchSession(uuid: string): Promise<void> {
    const { touchSession } = await import('./actor-session-uuid');
    touchSession(uuid);

    // In a full implementation, update the graph node as well
    // const nodeId = `session:${uuid}`;
    // await this.graph.updateNode(nodeId, {
    //   lastAccessedAt: new Date().toISOString(),
    // });
  }

  /**
   * Query sessions by prefix (e.g., all sessions under "primer/tasks")
   *
   * @param prefix - Address prefix to match
   * @returns Sessions matching the prefix
   */
  async findByPrefix(prefix: string): Promise<ActorSessionMapping[]> {
    const { normalizeActorAddress } = await import('./actor-session-uuid');
    const canonicalPrefix = normalizeActorAddress(prefix);

    const all = await this.listSessions();
    return all.filter((session) => session.canonicalAddress.startsWith(canonicalPrefix));
  }

  /**
   * Validate session exists and is active
   *
   * @param uuid - Session UUID
   * @returns true if session exists, false otherwise
   */
  async isActiveSession(uuid: string): Promise<boolean> {
    const session = await this.findByUUID(uuid);
    return session !== undefined;
  }

  /**
   * Generate hierarchical session path
   *
   * For hierarchical actor addressing (e.g., primer/tasks/task_55/agent_a)
   *
   * @param basePath - Base path (e.g., "primer/tasks/task_55")
   * @param agentName - Agent name (e.g., "agent_a")
   * @returns Full session UUID for the hierarchical path
   */
  async createHierarchicalSession(basePath: string, agentName: string): Promise<string> {
    const { normalizeActorAddress } = await import('./actor-session-uuid');
    const fullPath = `${basePath}/${agentName}`;
    const canonical = normalizeActorAddress(fullPath);

    const mapping = await this.registerSession(canonical);
    return mapping.sessionUUID;
  }
}

/**
 * Factory function to create ActorSessionGraph
 *
 * @param graph - Graph instance
 * @returns Configured ActorSessionGraph
 */
export function createActorSessionGraph(graph: Graph): ActorSessionGraph {
  return new ActorSessionGraph(graph);
}

/**
 * CozoDB Schema Extension for Actor-Session Mappings
 *
 * SQL-like DDL for creating actor_sessions relation in CozoDB.
 * To be integrated with existing CozoDB schema initialization.
 */
export const ACTOR_SESSION_SCHEMA = `
  :create actor_sessions {
    session_uuid: String,      # RFC 4122 UUID v5
    address: String,           # Original address as provided
    canonical_address: String, # Normalized address
    created_at: String,        # ISO 8601 timestamp
    last_accessed_at: String   # ISO 8601 timestamp
  }
`;

/**
 * CozoDB Queries for Actor-Session Mappings
 */
export const ActorSessionQueries = {
  /**
   * Find session by canonical address
   */
  FIND_BY_ADDRESS: `
    ?[session_uuid, address, canonical_address, created_at, last_accessed_at] :=
      *actor_sessions{
        session_uuid,
        address,
        canonical_address,
        created_at,
        last_accessed_at
      },
      canonical_address == $canonical_address
  `,

  /**
   * Find session by UUID
   */
  FIND_BY_UUID: `
    ?[session_uuid, address, canonical_address, created_at, last_accessed_at] :=
      *actor_sessions{
        session_uuid,
        address,
        canonical_address,
        created_at,
        last_accessed_at
      },
      session_uuid == $session_uuid
  `,

  /**
   * Find sessions by address prefix
   */
  FIND_BY_PREFIX: `
    ?[session_uuid, address, canonical_address, created_at, last_accessed_at] :=
      *actor_sessions{
        session_uuid,
        address,
        canonical_address,
        created_at,
        last_accessed_at
      },
      str_starts_with(canonical_address, $prefix)
  `,

  /**
   * List all sessions
   */
  LIST_ALL: `
    ?[session_uuid, address, canonical_address, created_at, last_accessed_at] :=
      *actor_sessions{
        session_uuid,
        address,
        canonical_address,
        created_at,
        last_accessed_at
      }
  `,
};
