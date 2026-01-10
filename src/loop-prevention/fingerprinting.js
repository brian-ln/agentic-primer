/**
 * Event Fingerprinting - Mechanism 2
 *
 * Detects duplicate events using content-based fingerprints.
 * LRU cache with configurable size (default: 1000)
 */

export const DEFAULT_CACHE_SIZE = 1000;

/**
 * Generate fingerprint for event
 */
export function generateFingerprint(event) {
  // Create fingerprint from key fields
  const { type, source, data } = event;
  const key = JSON.stringify({ type, source, data });

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
  constructor(maxSize = DEFAULT_CACHE_SIZE) {
    this.maxSize = maxSize;
    this.cache = new Map();
    this.hits = 0;
    this.misses = 0;
  }

  /**
   * Check if fingerprint exists in cache
   */
  has(fingerprint) {
    const exists = this.cache.has(fingerprint);

    if (exists) {
      this.hits++;
      // Move to end (LRU)
      const value = this.cache.get(fingerprint);
      this.cache.delete(fingerprint);
      this.cache.set(fingerprint, value);
    } else {
      this.misses++;
    }

    return exists;
  }

  /**
   * Add fingerprint to cache
   */
  add(fingerprint) {
    // Remove oldest if at capacity
    if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    this.cache.set(fingerprint, {
      timestamp: Date.now(),
      count: (this.cache.get(fingerprint)?.count || 0) + 1
    });
  }

  /**
   * Clear cache
   */
  clear() {
    this.cache.clear();
    this.hits = 0;
    this.misses = 0;
  }

  /**
   * Get cache statistics
   */
  getStats() {
    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      hits: this.hits,
      misses: this.misses,
      hitRate: this.hits / (this.hits + this.misses) || 0
    };
  }
}

/**
 * Fingerprint-based loop detector
 */
export class FingerprintDetector {
  constructor(cacheSize = DEFAULT_CACHE_SIZE) {
    this.cache = new FingerprintCache(cacheSize);
  }

  /**
   * Check if event is a duplicate
   */
  check(event) {
    const fingerprint = generateFingerprint(event);

    if (this.cache.has(fingerprint)) {
      return {
        allowed: false,
        reason: `Duplicate event detected (fingerprint: ${fingerprint})`,
        mechanism: 'fingerprinting',
        fingerprint
      };
    }

    // Add to cache
    this.cache.add(fingerprint);

    return { allowed: true, fingerprint };
  }

  /**
   * Clear cache
   */
  clear() {
    this.cache.clear();
  }

  /**
   * Get statistics
   */
  getStats() {
    return this.cache.getStats();
  }
}
