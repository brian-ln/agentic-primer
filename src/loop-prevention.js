/**
 * Loop Prevention Mechanisms
 *
 * Four-layer defense against infinite event loops:
 * 1. Depth counter (configurable limit, default 50)
 * 2. Event fingerprinting (LRU cache, 1000 entries)
 * 3. Ancestry chain tracking (parent_id)
 * 4. Circuit breakers (per-function quotas)
 */

export const DEFAULT_MAX_DEPTH = 50;
export const DEFAULT_FINGERPRINT_CACHE_SIZE = 1000;
export const DEFAULT_CIRCUIT_BREAKER_THRESHOLD = 100;

/**
 * Generate fingerprint for event
 */
export function generateFingerprint(event) {
  const { type, source, payload } = event;
  const key = JSON.stringify({ type, source, payload });

  // Simple hash function
  let hash = 0;
  for (let i = 0; i < key.length; i++) {
    const char = key.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32bit integer
  }

  return hash.toString(36);
}

/**
 * LRU Cache for fingerprints
 */
export class FingerprintCache {
  constructor(maxSize = DEFAULT_FINGERPRINT_CACHE_SIZE) {
    this.maxSize = maxSize;
    this.cache = new Map();
  }

  has(fingerprint) {
    return this.cache.has(fingerprint);
  }

  add(fingerprint) {
    // Remove oldest if at capacity
    if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    this.cache.set(fingerprint, Date.now());
  }

  clear() {
    this.cache.clear();
  }

  size() {
    return this.cache.size;
  }
}

/**
 * Circuit Breaker for per-function quotas
 */
export class CircuitBreaker {
  constructor(threshold = DEFAULT_CIRCUIT_BREAKER_THRESHOLD) {
    this.threshold = threshold;
    this.counts = new Map(); // functionId -> count
    this.windows = new Map(); // functionId -> timestamp
    this.windowDuration = 60000; // 1 minute
  }

  check(functionId) {
    const now = Date.now();
    const windowStart = this.windows.get(functionId);

    // Reset window if expired
    if (windowStart && (now - windowStart) > this.windowDuration) {
      this.counts.set(functionId, 0);
      this.windows.set(functionId, now);
    }

    const count = this.counts.get(functionId) || 0;

    if (count >= this.threshold) {
      return { allowed: false, reason: "Circuit breaker tripped" };
    }

    return { allowed: true };
  }

  increment(functionId) {
    const count = this.counts.get(functionId) || 0;
    this.counts.set(functionId, count + 1);

    if (!this.windows.has(functionId)) {
      this.windows.set(functionId, Date.now());
    }
  }

  reset(functionId) {
    this.counts.delete(functionId);
    this.windows.delete(functionId);
  }
}

/**
 * Check if event would create a loop
 */
export function checkForLoop(event, fingerprintCache, maxDepth = DEFAULT_MAX_DEPTH) {
  // Check depth limit
  if (event.depth >= maxDepth) {
    return {
      isLoop: true,
      reason: `Depth limit exceeded (${event.depth} >= ${maxDepth})`
    };
  }

  // Check fingerprint
  const fingerprint = generateFingerprint(event);
  if (fingerprintCache.has(fingerprint)) {
    return {
      isLoop: true,
      reason: `Duplicate event detected (fingerprint: ${fingerprint})`
    };
  }

  return { isLoop: false };
}
