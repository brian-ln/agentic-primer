/**
 * Claim check pattern for large message payloads.
 *
 * Messages exceeding CLAIM_CHECK_THRESHOLD (100KB) are stored externally
 * and a reference is passed in the message envelope instead of the full payload.
 *
 * Pattern: Claim Check (Enterprise Integration Patterns)
 * - Small messages: sent directly (no overhead)
 * - Large messages: stored externally, reference passed
 * - Receiving actor gets full payload transparently
 *
 * References:
 * - https://www.enterpriseintegrationpatterns.com/patterns/messaging/StoreInLibrary.html
 */

import type { JSONSchema } from './introspection.ts';

/**
 * Claim check threshold: 100KB
 * Payloads larger than this are stored externally
 */
export const CLAIM_CHECK_THRESHOLD = 100 * 1024; // 100KB

/**
 * Claim check reference passed in message envelope
 */
export interface ClaimCheckReference {
  claimCheckId: string;      // Storage key
  storageType: 'kv';         // Storage backend (only KV in MVP)
  contentType: 'json';       // Payload serialization
  size: number;              // Byte size of serialized payload
}

/**
 * Check if payload should use claim check pattern
 */
export function shouldUseClaimCheck(payload: unknown): boolean {
  try {
    const serialized = JSON.stringify(payload);
    return serialized.length > CLAIM_CHECK_THRESHOLD;
  } catch {
    // Non-serializable payload - can't use claim check
    return false;
  }
}

/**
 * Type guard for claim check reference
 */
export function isClaimCheckReference(value: unknown): value is ClaimCheckReference {
  if (!value || typeof value !== 'object') return false;
  const ref = value as any;
  return (
    typeof ref.claimCheckId === 'string' &&
    ref.storageType === 'kv' &&
    ref.contentType === 'json' &&
    typeof ref.size === 'number'
  );
}
