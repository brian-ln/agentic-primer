#!/usr/bin/env bun
/**
 * Query Cache with Learning
 *
 * Implements plan caching with exact and semantic similarity matching,
 * plus query statistics for adaptive optimization (Halo paper insights).
 *
 * Key capabilities:
 * - Exact match caching (query hash → plan)
 * - Semantic similarity search (find "close enough" queries)
 * - Query statistics tracking (latency, hit rate, success rate)
 * - Adaptive optimization (improve plans based on execution history)
 */

import type {
  QueryDefinition,
  QueryPlan,
  PlanCacheKey,
  QueryStatistics,
  ExecutionContext,
  ExecutionStats,
  IndexEffectiveness,
  IndexHint,
} from './types.ts';
import { createHash } from 'crypto';

/**
 * Query cache with exact and semantic matching
 */
export class QueryCache {
  // Exact match cache (query hash → plan)
  private exactCache = new Map<string, CachedPlan>();

  // Query statistics (signature → stats)
  private statistics = new Map<string, QueryStatistics>();

  // Cache size limits
  private maxCacheSize: number;
  private maxStatsSize: number;

  constructor(options?: {
    maxCacheSize?: number;
    maxStatsSize?: number;
  }) {
    this.maxCacheSize = options?.maxCacheSize || 1000;
    this.maxStatsSize = options?.maxStatsSize || 10000;
  }

  /**
   * Get cached plan or return null
   */
  get(
    query: QueryDefinition,
    context: ExecutionContext
  ): QueryPlan | null {
    const key = this.buildCacheKey(query, context);
    const cached = this.exactCache.get(key.workflowHash);

    if (!cached) {
      return null;
    }

    // Update access stats
    cached.accessCount++;
    cached.lastAccessedAt = Date.now();

    return cached.plan;
  }

  /**
   * Put plan in cache
   */
  put(
    query: QueryDefinition,
    plan: QueryPlan,
    context: ExecutionContext
  ): void {
    const key = this.buildCacheKey(query, context);

    // Evict if cache is full
    if (this.exactCache.size >= this.maxCacheSize) {
      this.evictLRU();
    }

    this.exactCache.set(key.workflowHash, {
      plan,
      key,
      createdAt: Date.now(),
      lastAccessedAt: Date.now(),
      accessCount: 1,
    });
  }

  /**
   * Find similar cached plans (semantic similarity)
   *
   * This would ideally use embedding similarity, but for now
   * we use structural similarity (pattern/filter matching).
   */
  findSimilar(
    query: QueryDefinition,
    threshold: number = 0.85
  ): CachedPlan | null {
    let bestMatch: CachedPlan | null = null;
    let bestScore = 0;

    for (const cached of this.exactCache.values()) {
      const score = this.calculateSimilarity(query, cached.plan.original);

      if (score >= threshold && score > bestScore) {
        bestMatch = cached;
        bestScore = score;
      }
    }

    if (bestMatch) {
      bestMatch.accessCount++;
      bestMatch.lastAccessedAt = Date.now();
    }

    return bestMatch;
  }

  /**
   * Record execution statistics (including per-step selectivity)
   */
  recordExecution(
    plan: QueryPlan,
    stats: ExecutionStats
  ): void {
    const signature = this.normalizeSignature(plan);

    // Get or create statistics
    let queryStats = this.statistics.get(signature);

    if (!queryStats) {
      queryStats = {
        signature,
        executionCount: 0,
        avgDurationMs: 0,
        durationVariance: 0,
        avgResultCount: 0,
        successRate: 0,
        cacheHitRate: 0,
        lastExecutedAt: 0,
        latencyPercentiles: { p50: 0, p90: 0, p99: 0 },
      };
    }

    // Update statistics using moving average
    const n = queryStats.executionCount;
    const newN = n + 1;

    // Update average duration
    queryStats.avgDurationMs =
      (queryStats.avgDurationMs * n + stats.durationMs) / newN;

    // Update variance (simplified)
    const delta = stats.durationMs - queryStats.avgDurationMs;
    queryStats.durationVariance =
      (queryStats.durationVariance * n + delta * delta) / newN;

    // Update average result count
    queryStats.avgResultCount =
      (queryStats.avgResultCount * n + stats.resultsReturned) / newN;

    // Update success rate
    const successValue = stats.resultsReturned > 0 ? 1 : 0;
    queryStats.successRate =
      (queryStats.successRate * n + successValue) / newN;

    // Update cache hit rate
    const totalHits = stats.cacheHits + stats.cacheMisses;
    const hitRate = totalHits > 0 ? stats.cacheHits / totalHits : 0;
    queryStats.cacheHitRate =
      (queryStats.cacheHitRate * n + hitRate) / newN;

    // Update execution count and timestamp
    queryStats.executionCount = newN;
    queryStats.lastExecutedAt = Date.now();

    // Update percentiles (simplified - would use histogram in production)
    this.updatePercentiles(queryStats, stats.durationMs);

    // Update index effectiveness tracking
    this.updateIndexEffectiveness(plan, stats, queryStats);

    // Store updated stats
    this.statistics.set(signature, queryStats);

    // Evict old stats if needed
    if (this.statistics.size > this.maxStatsSize) {
      this.evictOldestStats();
    }
  }

  /**
   * Get statistics for a plan
   */
  getStatistics(plan: QueryPlan): QueryStatistics | null {
    const signature = this.normalizeSignature(plan);
    return this.statistics.get(signature) || null;
  }

  /**
   * Get all statistics (for analysis and join optimization)
   */
  getAllStatistics(): QueryStatistics[] {
    return Array.from(this.statistics.values());
  }

  /**
   * Get per-step statistics from execution stats
   * Used by join optimizer to learn selectivity
   */
  getStepStatistics(stats: ExecutionStats): Map<string, { resultCount: number; stepId: string }> {
    const stepStats = new Map<string, { resultCount: number; stepId: string }>();

    for (const [stepId, stat] of stats.stepStats.entries()) {
      if (stat.success) {
        stepStats.set(stepId, {
          resultCount: stat.resultCount,
          stepId: stat.stepId
        });
      }
    }

    return stepStats;
  }

  /**
   * Clear cache
   */
  clear(): void {
    this.exactCache.clear();
  }

  /**
   * Clear statistics
   */
  clearStatistics(): void {
    this.statistics.clear();
  }

  /**
   * Get cache statistics
   */
  getCacheStats(): {
    size: number;
    maxSize: number;
    hitRate: number;
    avgAccessCount: number;
  } {
    const plans = Array.from(this.exactCache.values());
    const totalAccesses = plans.reduce((sum, p) => sum + p.accessCount, 0);
    const avgAccessCount = plans.length > 0 ? totalAccesses / plans.length : 0;

    // Calculate hit rate from query statistics
    const allStats = Array.from(this.statistics.values());
    const avgHitRate =
      allStats.length > 0
        ? allStats.reduce((sum, s) => sum + s.cacheHitRate, 0) / allStats.length
        : 0;

    return {
      size: this.exactCache.size,
      maxSize: this.maxCacheSize,
      hitRate: avgHitRate,
      avgAccessCount,
    };
  }

  // Private helpers

  /**
   * Build cache key from query and context
   */
  private buildCacheKey(
    query: QueryDefinition,
    context: ExecutionContext
  ): PlanCacheKey {
    // Hash query structure
    const workflowHash = this.hashQuery(query);

    // Fingerprint actor state (warm actors, cached data)
    const actorStateFingerprint = this.fingerprintActorState(context);

    // Fingerprint resource context
    const resourceContext = this.fingerprintResources(context);

    return {
      workflowHash,
      actorStateFingerprint,
      resourceContext,
    };
  }

  /**
   * Hash query definition
   */
  private hashQuery(query: QueryDefinition): string {
    const normalized = {
      patterns: query.patterns,
      filters: query.filters,
      traversals: query.traversals,
      aggregations: query.aggregations,
      actions: query.actions,
      returns: query.returns,
    };

    return createHash('sha256')
      .update(JSON.stringify(normalized))
      .digest('hex')
      .slice(0, 16);
  }

  /**
   * Fingerprint actor state
   */
  private fingerprintActorState(context: ExecutionContext): string {
    const warmActors = Array.from(context.warmActors).sort();
    const cacheKeys = Array.from(context.computationCache.keys()).sort();

    return createHash('sha256')
      .update(JSON.stringify({ warmActors, cacheKeys }))
      .digest('hex')
      .slice(0, 16);
  }

  /**
   * Fingerprint resource context
   */
  private fingerprintResources(context: ExecutionContext): string {
    return createHash('sha256')
      .update(JSON.stringify(context.resources))
      .digest('hex')
      .slice(0, 8);
  }

  /**
   * Calculate structural similarity between queries
   */
  private calculateSimilarity(
    q1: QueryDefinition,
    q2: QueryDefinition
  ): number {
    let score = 0;
    let totalWeight = 0;

    // Pattern similarity (weight: 0.4)
    const patternSim = this.comparePatterns(q1.patterns, q2.patterns);
    score += patternSim * 0.4;
    totalWeight += 0.4;

    // Filter similarity (weight: 0.2)
    const filterSim = this.compareFilters(
      q1.filters || [],
      q2.filters || []
    );
    score += filterSim * 0.2;
    totalWeight += 0.2;

    // Traversal similarity (weight: 0.2)
    const traversalSim = this.compareTraversals(
      q1.traversals || [],
      q2.traversals || []
    );
    score += traversalSim * 0.2;
    totalWeight += 0.2;

    // Action similarity (weight: 0.2)
    const actionSim = this.compareActions(
      q1.actions || [],
      q2.actions || []
    );
    score += actionSim * 0.2;
    totalWeight += 0.2;

    return totalWeight > 0 ? score / totalWeight : 0;
  }

  /**
   * Compare pattern arrays
   */
  private comparePatterns(p1: any[], p2: any[]): number {
    if (p1.length === 0 && p2.length === 0) return 1;
    if (p1.length === 0 || p2.length === 0) return 0;

    // Simple Jaccard similarity on labels
    const labels1 = new Set(p1.flatMap((p) => p.labels || []));
    const labels2 = new Set(p2.flatMap((p) => p.labels || []));

    const intersection = new Set(
      [...labels1].filter((x) => labels2.has(x))
    );
    const union = new Set([...labels1, ...labels2]);

    return union.size > 0 ? intersection.size / union.size : 0;
  }

  /**
   * Compare filter arrays
   */
  private compareFilters(f1: any[], f2: any[]): number {
    if (f1.length === 0 && f2.length === 0) return 1;
    if (f1.length === 0 || f2.length === 0) return 0;
    return f1.length === f2.length ? 0.8 : 0.5;
  }

  /**
   * Compare traversal arrays
   */
  private compareTraversals(t1: any[], t2: any[]): number {
    if (t1.length === 0 && t2.length === 0) return 1;
    if (t1.length === 0 || t2.length === 0) return 0;
    return t1.length === t2.length ? 0.8 : 0.5;
  }

  /**
   * Compare action arrays
   */
  private compareActions(a1: any[], a2: any[]): number {
    if (a1.length === 0 && a2.length === 0) return 1;
    if (a1.length === 0 || a2.length === 0) return 0;
    return a1.length === a2.length ? 0.8 : 0.5;
  }

  /**
   * Normalize plan signature for statistics
   */
  private normalizeSignature(plan: QueryPlan): string {
    // Use plan ID as signature (already normalized)
    return plan.id;
  }

  /**
   * Update latency percentiles (simplified)
   */
  private updatePercentiles(
    stats: QueryStatistics,
    newLatency: number
  ): void {
    // Simplified percentile update using exponential moving average
    const alpha = 0.1; // Smoothing factor

    if (stats.executionCount === 1) {
      stats.latencyPercentiles.p50 = newLatency;
      stats.latencyPercentiles.p90 = newLatency;
      stats.latencyPercentiles.p99 = newLatency;
    } else {
      // Update with bias toward higher latencies for p90/p99
      stats.latencyPercentiles.p50 =
        stats.latencyPercentiles.p50 * (1 - alpha) + newLatency * alpha;

      if (newLatency > stats.latencyPercentiles.p90) {
        stats.latencyPercentiles.p90 =
          stats.latencyPercentiles.p90 * (1 - alpha * 2) +
          newLatency * alpha * 2;
      }

      if (newLatency > stats.latencyPercentiles.p99) {
        stats.latencyPercentiles.p99 =
          stats.latencyPercentiles.p99 * (1 - alpha * 3) +
          newLatency * alpha * 3;
      }
    }
  }

  /**
   * Update index effectiveness statistics
   */
  private updateIndexEffectiveness(
    plan: QueryPlan,
    execStats: ExecutionStats,
    queryStats: QueryStatistics
  ): void {
    // Extract index hints from plan metadata
    const indexHints = plan.metadata.indexHints || [];
    if (indexHints.length === 0) return;

    // Initialize index effectiveness map if needed
    if (!queryStats.indexEffectiveness) {
      queryStats.indexEffectiveness = new Map();
    }

    // Calculate baseline (estimated cost without indexes)
    const estimatedDuration = plan.metadata.estimatedCost.makespan;
    const actualDuration = execStats.durationMs;

    // Calculate improvement (positive = faster than expected)
    const improvement =
      estimatedDuration > 0
        ? Math.max(0, (estimatedDuration - actualDuration) / estimatedDuration)
        : 0;

    // Update effectiveness for each index hint
    for (const hint of indexHints) {
      const indexName = hint.index;

      // Get or create effectiveness entry
      let effectiveness = queryStats.indexEffectiveness.get(indexName);

      if (!effectiveness) {
        effectiveness = {
          indexName,
          useCount: 0,
          avgImprovement: 0,
          successRate: 0,
          avgResultCount: 0,
          lastUsedAt: 0,
        };
      }

      // Update with moving average
      const n = effectiveness.useCount;
      const newN = n + 1;

      effectiveness.avgImprovement =
        (effectiveness.avgImprovement * n + improvement) / newN;

      const successValue = execStats.resultsReturned > 0 ? 1 : 0;
      effectiveness.successRate =
        (effectiveness.successRate * n + successValue) / newN;

      effectiveness.avgResultCount =
        (effectiveness.avgResultCount * n + execStats.resultsReturned) / newN;

      effectiveness.useCount = newN;
      effectiveness.lastUsedAt = Date.now();

      // Store updated effectiveness
      queryStats.indexEffectiveness.set(indexName, effectiveness);
    }
  }

  /**
   * Evict least recently used plan
   */
  private evictLRU(): void {
    let lruKey: string | null = null;
    let lruTime = Infinity;

    for (const [key, cached] of this.exactCache.entries()) {
      if (cached.lastAccessedAt < lruTime) {
        lruTime = cached.lastAccessedAt;
        lruKey = key;
      }
    }

    if (lruKey) {
      this.exactCache.delete(lruKey);
    }
  }

  /**
   * Evict oldest statistics
   */
  private evictOldestStats(): void {
    let oldestKey: string | null = null;
    let oldestTime = Infinity;

    for (const [key, stats] of this.statistics.entries()) {
      if (stats.lastExecutedAt < oldestTime) {
        oldestTime = stats.lastExecutedAt;
        oldestKey = key;
      }
    }

    if (oldestKey) {
      this.statistics.delete(oldestKey);
    }
  }
}

/**
 * Cached plan entry
 */
interface CachedPlan {
  plan: QueryPlan;
  key: PlanCacheKey;
  createdAt: number;
  lastAccessedAt: number;
  accessCount: number;
}
