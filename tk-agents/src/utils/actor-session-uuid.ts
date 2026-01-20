/**
 * Actor Session UUID Generation
 *
 * Maps human-readable actor addresses to RFC 4122 UUIDs for Claude Code session management.
 * Uses UUID v5 (deterministic, SHA-1 based) to ensure same address always produces same UUID.
 *
 * Supports multiple address formats:
 * - Hierarchical paths: "primer/tasks/task_55/agent_a"
 * - Dot notation: "primer.tasks.task_55"
 * - Docker-style: "happy-elephant-42"
 * - Mixed formats: "primer.tasks/task_55"
 */

import { v5 as uuidv5 } from 'uuid';

/**
 * Project namespace UUID (fixed for all primer sessions)
 * Generated once using UUID v4, then hardcoded for determinism
 */
export const PRIMER_NAMESPACE = '550e8400-e29b-41d4-a716-446655440000';

/**
 * Alternative namespace for testing/development
 * Use this in tests to avoid polluting production namespace
 */
export const TEST_NAMESPACE = '6ba7b810-9dad-11d1-80b4-00c04fd430c8';

/**
 * Convert an actor address to a deterministic UUID
 *
 * @param address - Human-readable actor address (any format)
 * @param namespace - UUID namespace (defaults to PRIMER_NAMESPACE)
 * @returns RFC 4122 UUID v5
 *
 * @example
 * ```typescript
 * // All produce deterministic UUIDs
 * actorAddressToSessionUUID("primer/tasks/task_55")
 * actorAddressToSessionUUID("primer.tasks.task_55")
 * actorAddressToSessionUUID("happy-elephant-42")
 * ```
 */
export function actorAddressToSessionUUID(
  address: string,
  namespace: string = PRIMER_NAMESPACE
): string {
  if (!address || address.trim() === '') {
    throw new Error('Actor address cannot be empty');
  }

  // Normalize the address to canonical form
  const canonical = normalizeActorAddress(address);

  // Generate deterministic UUID v5
  return uuidv5(canonical, namespace);
}

/**
 * Normalize different address formats to canonical form
 *
 * Rules:
 * - Convert dots to slashes: "a.b.c" → "a/b/c"
 * - Remove leading/trailing slashes
 * - Collapse multiple slashes: "a//b" → "a/b"
 * - Convert to lowercase for case-insensitive matching
 * - Preserve hyphens in Docker-style names
 *
 * @param address - Raw actor address in any format
 * @returns Canonical form (lowercase, slash-separated)
 *
 * @example
 * ```typescript
 * normalizeActorAddress("Primer.Tasks.Task_55")  // → "primer/tasks/task_55"
 * normalizeActorAddress("primer//tasks/task_55") // → "primer/tasks/task_55"
 * normalizeActorAddress("/primer/tasks/")        // → "primer/tasks"
 * normalizeActorAddress("Happy-Elephant-42")     // → "happy-elephant-42"
 * ```
 */
export function normalizeActorAddress(address: string): string {
  return address
    .trim()
    .toLowerCase()
    .replace(/\./g, '/') // Dots to slashes
    .replace(/\/+/g, '/') // Collapse multiple slashes
    .replace(/^\/|\/$/g, ''); // Remove leading/trailing slashes
}

/**
 * Validate that a string is a valid RFC 4122 UUID
 *
 * @param uuid - String to validate
 * @returns true if valid UUID, false otherwise
 */
export function isValidUUID(uuid: string): boolean {
  const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
  return uuidRegex.test(uuid);
}

/**
 * Actor-Session mapping type
 */
export interface ActorSessionMapping {
  address: string;
  canonicalAddress: string;
  sessionUUID: string;
  createdAt: Date;
  lastAccessedAt: Date;
}

/**
 * In-memory cache for actor-session mappings
 * Used for reverse lookups (UUID → address)
 *
 * In production, this should be backed by CozoDB graph storage
 */
const sessionCache = new Map<string, ActorSessionMapping>();

/**
 * Register an actor-session mapping
 *
 * @param address - Human-readable actor address
 * @param sessionUUID - UUID (optional, will be generated if not provided)
 * @returns The mapping with both address and UUID
 *
 * @example
 * ```typescript
 * const mapping = registerActorSession("primer/tasks/task_55");
 * // { address: "primer/tasks/task_55", sessionUUID: "abc123...", ... }
 * ```
 */
export function registerActorSession(
  address: string,
  sessionUUID?: string
): ActorSessionMapping {
  const canonical = normalizeActorAddress(address);
  const uuid = sessionUUID || actorAddressToSessionUUID(address);

  if (!isValidUUID(uuid)) {
    throw new Error(`Invalid UUID: ${uuid}`);
  }

  const now = new Date();
  const mapping: ActorSessionMapping = {
    address,
    canonicalAddress: canonical,
    sessionUUID: uuid,
    createdAt: now,
    lastAccessedAt: now,
  };

  sessionCache.set(uuid, mapping);
  return mapping;
}

/**
 * Find an actor session by address
 *
 * @param address - Human-readable actor address
 * @returns The mapping if found, undefined otherwise
 */
export function findSessionByAddress(address: string): ActorSessionMapping | undefined {
  const uuid = actorAddressToSessionUUID(address);
  return sessionCache.get(uuid);
}

/**
 * Find an actor address by session UUID
 *
 * @param uuid - Session UUID
 * @returns The mapping if found, undefined otherwise
 */
export function findAddressByUUID(uuid: string): ActorSessionMapping | undefined {
  return sessionCache.get(uuid);
}

/**
 * Update last accessed time for a session
 *
 * @param uuid - Session UUID
 */
export function touchSession(uuid: string): void {
  const mapping = sessionCache.get(uuid);
  if (mapping) {
    mapping.lastAccessedAt = new Date();
  }
}

/**
 * Clear all session mappings (useful for testing)
 */
export function clearSessionCache(): void {
  sessionCache.clear();
}

/**
 * Get all registered sessions
 *
 * @returns Array of all session mappings
 */
export function getAllSessions(): ActorSessionMapping[] {
  return Array.from(sessionCache.values());
}

/**
 * Generate a human-readable session ID from an actor address
 * Useful for debugging and logs
 *
 * @param address - Actor address
 * @returns Shortened UUID + address hint
 *
 * @example
 * ```typescript
 * generateReadableSessionId("primer/tasks/task_55")
 * // → "abc123-task_55"
 * ```
 */
export function generateReadableSessionId(address: string): string {
  const uuid = actorAddressToSessionUUID(address);
  const shortUuid = uuid.slice(0, 8);
  const addressPart = address.split('/').pop() || address.split('.').pop() || address;
  return `${shortUuid}-${addressPart}`;
}
