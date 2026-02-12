#!/usr/bin/env bun
/**
 * Protocol Adapter Layer
 *
 * Converts between SEAG internal types and @agentic-primer/protocols types.
 * This adapter provides bidirectional conversion while maintaining backward
 * compatibility with existing SEAG code.
 *
 * @module protocol
 */

import type { Address as ProtocolAddress } from '@agentic-primer/protocols';
import { addressSchema } from '@agentic-primer/protocols/validators';
import type { Address as SeagAddress } from '@agentic-primer/actors';
import { address as seagAddress, parseAddress } from '@agentic-primer/actors';

/**
 * Convert SEAG Address string to Protocol Address object.
 *
 * SEAG uses string addresses in format: @(id) or @(namespace/path)
 * Protocol uses structured Address object with id, namespace, scope.
 *
 * ## Examples
 *
 * ```typescript
 * // Flat ID (legacy)
 * toProtocolAddress('@(actor-123)')
 * // => { id: 'actor-123', scope: 'node' }
 *
 * // Hierarchical path
 * toProtocolAddress('@(domain/inference)')
 * // => { id: 'inference', namespace: 'domain', scope: 'node' }
 *
 * // Deep path
 * toProtocolAddress('@(workflows/build/tasks/compile)')
 * // => { id: 'compile', namespace: 'workflows/build/tasks', scope: 'node' }
 * ```
 *
 * @param seagAddr - SEAG address string (@(id) format)
 * @returns Protocol Address object
 */
export function toProtocolAddress(seagAddr: SeagAddress): ProtocolAddress {
  const id = parseAddress(seagAddr);

  // Check if this is a hierarchical path (contains '/')
  if (id.includes('/')) {
    const segments = id.split('/').filter(s => s.length > 0);
    const leafId = segments[segments.length - 1];
    const namespace = segments.slice(0, -1).join('/');

    return {
      id: leafId,
      namespace: namespace || null,
      scope: 'node',
    };
  }

  // Flat ID (no namespace)
  return {
    id,
    scope: 'node',
  };
}

/**
 * Convert Protocol Address object to SEAG Address string.
 *
 * Protocol uses structured Address object, SEAG uses @(id) strings.
 * Reconstructs hierarchical path from namespace + id.
 *
 * ## Examples
 *
 * ```typescript
 * // Flat ID
 * fromProtocolAddress({ id: 'actor-123', scope: 'node' })
 * // => '@(actor-123)'
 *
 * // With namespace
 * fromProtocolAddress({ id: 'inference', namespace: 'domain', scope: 'node' })
 * // => '@(domain/inference)'
 *
 * // Deep namespace
 * fromProtocolAddress({ id: 'compile', namespace: 'workflows/build/tasks', scope: 'node' })
 * // => '@(workflows/build/tasks/compile)'
 * ```
 *
 * @param protocolAddr - Protocol Address object
 * @returns SEAG address string (@(id) format)
 */
export function fromProtocolAddress(protocolAddr: ProtocolAddress): SeagAddress {
  // Construct full path from namespace + id
  if (protocolAddr.namespace) {
    const fullPath = `${protocolAddr.namespace}/${protocolAddr.id}`;
    return seagAddress(fullPath);
  }

  // No namespace, just return ID
  return seagAddress(protocolAddr.id);
}

/**
 * Serialize Protocol Address to string representation.
 *
 * For storage, display, or transmission. Different from fromProtocolAddress
 * which converts to SEAG's @(id) format - this creates a canonical string
 * suitable for protocol-aware systems.
 *
 * ## Format
 *
 * - With namespace: `namespace/id`
 * - Without namespace: `id`
 * - With version: `namespace/id@version`
 *
 * ## Examples
 *
 * ```typescript
 * addressToString({ id: 'actor-123', scope: 'node' })
 * // => 'actor-123'
 *
 * addressToString({ id: 'inference', namespace: 'domain', scope: 'node' })
 * // => 'domain/inference'
 *
 * addressToString({ id: 'inference', namespace: 'domain', scope: 'node', version: 'v1' })
 * // => 'domain/inference@v1'
 * ```
 *
 * @param addr - Protocol Address object
 * @returns String representation (namespace/id[@version])
 */
export function addressToString(addr: ProtocolAddress): string {
  let result = addr.namespace
    ? `${addr.namespace}/${addr.id}`
    : addr.id;

  if (addr.version) {
    result += `@${addr.version}`;
  }

  return result;
}

/**
 * Parse string to Protocol Address.
 *
 * Inverse of addressToString. Parses canonical protocol address strings
 * into structured Address objects.
 *
 * ## Format Support
 *
 * - Flat ID: `actor-123`
 * - Namespaced: `domain/inference`
 * - Versioned: `domain/inference@v1`
 * - Deep paths: `workflows/build/tasks/compile`
 *
 * @param str - Address string (namespace/id[@version])
 * @returns Protocol Address object
 * @throws Error if string format is invalid
 */
export function parseProtocolAddress(str: string): ProtocolAddress {
  // Check for version suffix (@v1, @v2, etc.)
  let version: string | null = null;
  let pathPart = str;

  const versionMatch = str.match(/^(.+)@([^@/]+)$/);
  if (versionMatch) {
    pathPart = versionMatch[1];
    version = versionMatch[2];
  }

  // Split into segments
  const segments = pathPart.split('/').filter(s => s.length > 0);

  if (segments.length === 0) {
    throw new Error(`Invalid address string: empty path`);
  }

  // Last segment is ID, rest is namespace
  const id = segments[segments.length - 1];
  const namespace = segments.length > 1
    ? segments.slice(0, -1).join('/')
    : null;

  const addr: ProtocolAddress = {
    id,
    namespace,
    scope: 'node',
  };

  if (version) {
    addr.version = version;
  }

  return addr;
}

/**
 * Validate Protocol Address using Zod schema.
 *
 * Uses the official @agentic-primer/protocols validator to ensure
 * address conforms to protocol specification.
 *
 * @param addr - Address to validate
 * @returns True if valid, false otherwise
 */
export function validateProtocolAddress(addr: ProtocolAddress): boolean {
  try {
    addressSchema.parse(addr);
    return true;
  } catch {
    return false;
  }
}

/**
 * Create Protocol Address from parts.
 *
 * Convenience builder for creating Protocol Address objects with
 * validation.
 *
 * @param id - Actor/node ID (required)
 * @param namespace - Optional namespace path
 * @param version - Optional version string
 * @returns Protocol Address object
 * @throws Error if validation fails
 */
export function createProtocolAddress(
  id: string,
  namespace?: string | null,
  version?: string | null
): ProtocolAddress {
  // Additional SEAG validation beyond protocol schema
  if (!id || id.length === 0) {
    throw new Error('Address ID cannot be empty');
  }

  const addr: ProtocolAddress = {
    id,
    namespace: namespace || null,
    scope: 'node',
  };

  if (version) {
    addr.version = version;
  }

  // Validate using protocol schema
  addressSchema.parse(addr);
  return addr;
}

// Re-export protocol types for convenience
export type { ProtocolAddress };
export { addressSchema };
