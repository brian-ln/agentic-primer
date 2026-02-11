/**
 * Path Cache - LRU cache for actor path resolution
 *
 * Reduces routing overhead by caching resolved actor references.
 * Generic type parameter for portability - stores any reference type.
 *
 * Ported from simplify/src/messaging/path-cache.ts
 */

interface CacheEntry<T> {
  value: T;
  expiry: number;
  created: number;
}

export interface PathCacheConfig {
  maxSize?: number;
  ttl?: number;
  enableMetrics?: boolean;
}

export interface CacheMetrics {
  hits: number;
  misses: number;
  hitRate: number;
  size: number;
  maxSize: number;
  evictions: number;
  expirations: number;
  invalidations: number;
}

export class PathCache<T = unknown> {
  private cache: Map<string, CacheEntry<T>>;
  private maxSize: number;
  private ttl: number;
  private enableMetrics: boolean;

  private hits: number = 0;
  private misses: number = 0;
  private evictions: number = 0;
  private expirations: number = 0;
  private invalidations: number = 0;

  constructor(config: PathCacheConfig = {}) {
    this.cache = new Map();
    this.maxSize = config.maxSize ?? 1000;
    this.ttl = config.ttl ?? 60000;
    this.enableMetrics = config.enableMetrics ?? true;
  }

  get(path: string): T | null {
    const entry = this.cache.get(path);

    if (!entry) {
      if (this.enableMetrics) this.misses++;
      return null;
    }

    const now = Date.now();
    if (now > entry.expiry) {
      this.cache.delete(path);
      if (this.enableMetrics) {
        this.misses++;
        this.expirations++;
      }
      return null;
    }

    // LRU: move to end
    this.cache.delete(path);
    this.cache.set(path, entry);

    if (this.enableMetrics) this.hits++;
    return entry.value;
  }

  set(path: string, value: T): void {
    const now = Date.now();

    if (this.cache.has(path)) {
      this.cache.delete(path);
    }

    if (this.cache.size >= this.maxSize) {
      const oldestKey = this.cache.keys().next().value;
      if (oldestKey !== undefined) {
        this.cache.delete(oldestKey);
      }
      if (this.enableMetrics) this.evictions++;
    }

    this.cache.set(path, {
      value,
      expiry: now + this.ttl,
      created: now,
    });
  }

  invalidate(path: string): boolean {
    const deleted = this.cache.delete(path);
    if (deleted && this.enableMetrics) {
      this.invalidations++;
    }
    return deleted;
  }

  invalidatePrefix(prefix: string): number {
    let count = 0;
    const normalizedPrefix = prefix.endsWith('/') ? prefix : prefix + '/';
    for (const key of this.cache.keys()) {
      if (key === prefix || key.startsWith(normalizedPrefix)) {
        this.cache.delete(key);
        count++;
      }
    }
    if (this.enableMetrics) {
      this.invalidations += count;
    }
    return count;
  }

  clear(): void {
    const size = this.cache.size;
    this.cache.clear();
    if (this.enableMetrics) {
      this.invalidations += size;
    }
  }

  getSize(): number {
    return this.cache.size;
  }

  getMetrics(): CacheMetrics {
    const totalAccesses = this.hits + this.misses;
    const hitRate = totalAccesses > 0
      ? Math.round((this.hits / totalAccesses) * 10000) / 100
      : 0;

    return {
      hits: this.hits,
      misses: this.misses,
      hitRate,
      size: this.cache.size,
      maxSize: this.maxSize,
      evictions: this.evictions,
      expirations: this.expirations,
      invalidations: this.invalidations,
    };
  }

  resetMetrics(): void {
    this.hits = 0;
    this.misses = 0;
    this.evictions = 0;
    this.expirations = 0;
    this.invalidations = 0;
  }

  prune(): number {
    const now = Date.now();
    let pruned = 0;
    for (const [key, entry] of this.cache.entries()) {
      if (now > entry.expiry) {
        this.cache.delete(key);
        pruned++;
      }
    }
    if (this.enableMetrics) {
      this.expirations += pruned;
    }
    return pruned;
  }
}
