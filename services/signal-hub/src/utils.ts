/**
 * Signal Hub Utilities
 */

import type { CanonicalAddress, SharedMessage, TokenBucket } from './types';

/**
 * Generate a canonical address from a path
 */
export function toCanonicalAddress(path: string): CanonicalAddress {
  return `@(${path})` as CanonicalAddress;
}

/**
 * Extract path from canonical address
 */
export function fromCanonicalAddress(addr: CanonicalAddress): string {
  const match = addr.match(/^@\((.+)\)$/);
  return match ? match[1] : addr;
}

/**
 * Generate a unique session ID
 */
export function generateSessionId(): string {
  return `session-${crypto.randomUUID()}`;
}

/**
 * Generate a renewal token for actor registration
 */
export function generateRenewalToken(): string {
  return `renewal-${crypto.randomUUID()}`;
}

/**
 * Create a SharedMessage
 */
export function createMessage(
  type: string,
  payload: unknown,
  from: CanonicalAddress,
  to: CanonicalAddress,
  pattern: 'tell' | 'ask' = 'tell',
  metadata: Record<string, unknown> = {}
): SharedMessage {
  return {
    id: crypto.randomUUID(),
    from,
    to,
    type,
    payload,
    pattern,
    correlationId: null,
    timestamp: Date.now(),
    metadata,
    ttl: null,
    signature: null,
  };
}

/**
 * Create a reply message (preserves correlationId from original)
 */
export function createReply(
  type: string,
  payload: unknown,
  originalMessage: SharedMessage,
  from: CanonicalAddress,
  metadata: Record<string, unknown> = {}
): SharedMessage {
  return {
    id: crypto.randomUUID(),
    from,
    to: originalMessage.from,
    type,
    payload,
    pattern: 'tell',
    correlationId: originalMessage.id,
    timestamp: Date.now(),
    metadata,
    ttl: null,
    signature: null,
  };
}

/**
 * Create an error message
 */
export function createErrorMessage(
  code: string,
  message: string,
  originalMessage: SharedMessage,
  from: CanonicalAddress,
  details?: Record<string, unknown>
): SharedMessage {
  return createReply(
    'hub:error',
    {
      code,
      message,
      details: details ?? {},
    },
    originalMessage,
    from
  );
}

/**
 * Match actor address against glob pattern
 * Simple implementation - supports * wildcard
 */
export function matchPattern(pattern: string, address: CanonicalAddress): boolean {
  // Extract path from address
  const addressPath = fromCanonicalAddress(address);

  // Extract path from pattern if it has @() wrapper
  const patternPath = pattern.startsWith('@(') && pattern.endsWith(')')
    ? fromCanonicalAddress(pattern as CanonicalAddress)
    : pattern;

  // Convert glob pattern to regex
  const regexPattern = patternPath
    .replace(/[.+?^${}()|[\]\\]/g, '\\$&') // Escape special chars
    .replace(/\*/g, '.*'); // Convert * to .*

  const regex = new RegExp(`^${regexPattern}$`);
  return regex.test(addressPath);
}

/**
 * Token bucket rate limiting
 */
export function createTokenBucket(capacity: number, refillRate: number): TokenBucket {
  return {
    tokens: capacity,
    capacity,
    refillRate,
    lastRefill: Date.now(),
  };
}

export function consumeToken(bucket: TokenBucket): boolean {
  // Refill tokens based on elapsed time
  const now = Date.now();
  const elapsed = (now - bucket.lastRefill) / 1000; // seconds
  const tokensToAdd = elapsed * bucket.refillRate;

  bucket.tokens = Math.min(bucket.capacity, bucket.tokens + tokensToAdd);
  bucket.lastRefill = now;

  // Try to consume a token
  if (bucket.tokens >= 1) {
    bucket.tokens -= 1;
    return true;
  }

  return false;
}

/**
 * Parse JWT payload without verification (for debugging)
 */
export function parseJWTPayload(token: string): unknown {
  try {
    const [, payloadB64] = token.split('.');
    if (!payloadB64) return null;

    const payload = atob(payloadB64.replace(/-/g, '+').replace(/_/g, '/'));
    return JSON.parse(payload);
  } catch {
    return null;
  }
}

/**
 * Check if a timestamp has expired
 */
export function isExpired(expiresAt: number): boolean {
  return Date.now() >= expiresAt;
}

/**
 * Validate SharedMessage structure
 */
export function validateSharedMessage(msg: unknown): msg is SharedMessage {
  if (typeof msg !== 'object' || msg === null) return false;

  const m = msg as Record<string, unknown>;

  return (
    typeof m.id === 'string' &&
    typeof m.from === 'string' &&
    m.from.startsWith('@(') &&
    typeof m.to === 'string' &&
    m.to.startsWith('@(') &&
    typeof m.type === 'string' &&
    (m.pattern === 'tell' || m.pattern === 'ask') &&
    typeof m.timestamp === 'number' &&
    (m.correlationId === null || typeof m.correlationId === 'string') &&
    (m.ttl === null || typeof m.ttl === 'number') &&
    (m.signature === null || typeof m.signature === 'string')
  );
}
