#!/usr/bin/env bun
/**
 * Join Optimizer
 *
 * Implements cost-based join optimization for multi-pattern queries.
 * Reorders pattern matching steps to minimize intermediate result sizes
 * using selectivity estimates from query statistics.
 *
 * Key principles from Halo paper:
 * - Start with most selective patterns (smallest result sets)
 * - Use historical statistics to estimate selectivity
 * - Consider dependencies when reordering
 * - Balance selectivity with warm actor advantages
 *
 * Cost Model:
 * - Selectivity: probability a pattern matches (0-1)
 * - Cardinality: estimated result count
 * - Join cost: left_cardinality * right_cardinality * join_selectivity
 */

import type {
  QueryDefinition,
  PatternSpec,
  PlanStep,
  ExecutionContext,
  QueryStatistics,
} from '../types.ts';
import type { Address } from '@agentic-primer/actors';

/**
 * Statistics tracker for pattern selectivity
 */
export interface SelectivityStats {
  /** Pattern signature (normalized) */
  signature: string;

  /** Average result count */
  avgResultCount: number;

  /** Result count variance */
  resultVariance: number;

  /** Execution count */
  executionCount: number;

  /** Selectivity estimate (0-1) */
  selectivity: number;

  /** Last updated timestamp */
  lastUpdated: number;
}

/**
 * Join order candidate
 */
interface JoinCandidate {
  /** Pattern steps in proposed order */
  steps: PlanStep[];

  /** Estimated total cost */
  estimatedCost: number;

  /** Estimated intermediate result sizes */
  intermediateCardinalities: number[];
}

/**
 * Join optimizer for multi-pattern queries
 */
export class JoinOptimizer {
  // Pattern selectivity statistics (signature → stats)
  private selectivityStats = new Map<string, SelectivityStats>();

  // Default selectivity for unknown patterns
  private readonly defaultSelectivity = 0.1;

  // Default cardinality for unknown patterns
  private readonly defaultCardinality = 100;

  constructor(
    private options?: {
      defaultSelectivity?: number;
      defaultCardinality?: number;
      enableDynamicProgramming?: boolean;
    }
  ) {
    if (options?.defaultSelectivity !== undefined) {
      this.defaultSelectivity = options.defaultSelectivity;
    }
    if (options?.defaultCardinality !== undefined) {
      this.defaultCardinality = options.defaultCardinality;
    }
  }

  /**
   * Optimize join order for pattern steps
   *
   * This is the main entry point called by the compiler.
   */
  optimizeJoinOrder(
    steps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep[] {
    // Filter to only pattern/query steps (these are candidates for reordering)
    const querySteps = steps.filter((s) => s.type === 'query');
    const otherSteps = steps.filter((s) => s.type !== 'query');

    // If only one pattern, no optimization needed
    if (querySteps.length <= 1) {
      return steps;
    }

    // Find optimal join order using cost-based optimization
    const optimized = this.findOptimalJoinOrder(querySteps, context);

    // Merge back with other steps (preserving dependencies)
    return this.mergeSteps(optimized, otherSteps);
  }

  /**
   * Find optimal join order using greedy or DP approach
   */
  private findOptimalJoinOrder(
    steps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep[] {
    // Use dynamic programming for small join sets, greedy for large
    const useDynamicProgramming =
      this.options?.enableDynamicProgramming !== false && steps.length <= 8;

    if (useDynamicProgramming) {
      return this.dynamicProgrammingJoinOrder(steps, context);
    } else {
      return this.greedyJoinOrder(steps, context);
    }
  }

  /**
   * Greedy join ordering: always pick most selective remaining pattern
   *
   * Time complexity: O(n^2) where n is number of patterns
   * This is fast and works well in practice.
   */
  private greedyJoinOrder(
    steps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep[] {
    const remaining = [...steps];
    const ordered: PlanStep[] = [];
    let currentCardinality = 1; // Start with empty set

    while (remaining.length > 0) {
      // Find most selective step considering current state
      let bestIndex = 0;
      let bestCost = Infinity;

      for (let i = 0; i < remaining.length; i++) {
        const step = remaining[i];
        const selectivity = this.getSelectivity(step);
        const cardinality = this.getCardinality(step);

        // Cost = current_size * new_size * join_selectivity
        // For first step, cost is just the cardinality
        const cost =
          ordered.length === 0
            ? cardinality
            : currentCardinality * cardinality * selectivity;

        // Adjust for warm actors (reduce cost by 50%)
        const warmBonus =
          context?.warmActors.has(step.actor) ? 0.5 : 1.0;

        const adjustedCost = cost * warmBonus;

        if (adjustedCost < bestCost) {
          bestCost = adjustedCost;
          bestIndex = i;
        }
      }

      // Add best step to ordered list
      const bestStep = remaining.splice(bestIndex, 1)[0];
      ordered.push(bestStep);

      // Update current cardinality for next iteration
      currentCardinality = Math.max(
        1,
        currentCardinality *
          this.getCardinality(bestStep) *
          this.getSelectivity(bestStep)
      );
    }

    return ordered;
  }

  /**
   * Dynamic programming join ordering (optimal for small join sets)
   *
   * Time complexity: O(n * 2^n) where n is number of patterns
   * Finds globally optimal join order but slower for large n.
   */
  private dynamicProgrammingJoinOrder(
    steps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep[] {
    const n = steps.length;

    // DP table: memo[subset] = { cost, order }
    const memo = new Map<number, { cost: number; order: number[] }>();

    // Base case: single patterns
    // Cost is the cardinality adjusted for warm actors
    for (let i = 0; i < n; i++) {
      const subset = 1 << i; // Bitmask for single pattern
      const cardinality = this.getCardinality(steps[i]);

      // Apply warm actor bonus
      const warmBonus = context?.warmActors.has(steps[i].actor) ? 0.5 : 1.0;
      const adjustedCost = cardinality * warmBonus;

      memo.set(subset, {
        cost: adjustedCost,
        order: [i],
      });
    }

    // Build up: try all subsets of size 2, 3, ..., n
    for (let size = 2; size <= n; size++) {
      // Enumerate all subsets of given size
      for (let subset = 0; subset < 1 << n; subset++) {
        if (this.popcount(subset) !== size) continue;

        let bestCost = Infinity;
        let bestOrder: number[] = [];

        // Try all ways to split subset into two parts
        // (Find best join order by trying all possible last steps)
        for (let i = 0; i < n; i++) {
          if ((subset & (1 << i)) === 0) continue; // i not in subset

          const remaining = subset ^ (1 << i); // Remove i from subset
          const prevResult = memo.get(remaining);
          if (!prevResult) continue;

          // Cost of joining prevResult with step[i]
          // prevResult.cost is the accumulated cost so far
          // Calculate the cardinality after joining with step[i]
          const prevCardinality = this.getResultCardinality(prevResult.order, steps);
          const newStepCard = this.getCardinality(steps[i]);
          const newStepSel = this.getSelectivity(steps[i]);

          // Warm actor bonus
          const warmBonus = context?.warmActors.has(steps[i].actor) ? 0.5 : 1.0;

          // New intermediate cardinality after join
          const newCardinality = prevCardinality * newStepCard * newStepSel * warmBonus;

          // Total cost = previous cost + new intermediate size
          const totalCost = prevResult.cost + newCardinality;

          if (totalCost < bestCost) {
            bestCost = totalCost;
            bestOrder = [...prevResult.order, i];
          }
        }

        memo.set(subset, { cost: bestCost, order: bestOrder });
      }
    }

    // Extract final order
    const fullSubset = (1 << n) - 1;
    const result = memo.get(fullSubset);
    if (!result) {
      return steps; // Fallback to original order
    }

    // Map indices back to steps
    return result.order.map((i) => steps[i]);
  }

  /**
   * Calculate the result cardinality after joining steps in given order
   */
  private getResultCardinality(order: number[], steps: PlanStep[]): number {
    let cardinality = 1;

    for (const idx of order) {
      const step = steps[idx];
      const stepCard = this.getCardinality(step);
      const stepSel = this.getSelectivity(step);
      cardinality = cardinality * stepCard * stepSel;
    }

    return Math.max(1, cardinality);
  }

  /**
   * Count set bits in bitmask (population count)
   */
  private popcount(n: number): number {
    let count = 0;
    while (n) {
      count += n & 1;
      n >>= 1;
    }
    return count;
  }

  /**
   * Merge optimized query steps with other steps
   */
  private mergeSteps(
    optimizedQuerySteps: PlanStep[],
    otherSteps: PlanStep[]
  ): PlanStep[] {
    // For now, just concatenate (query steps first, then others)
    // More sophisticated: interleave based on dependencies
    return [...optimizedQuerySteps, ...otherSteps];
  }

  /**
   * Get selectivity estimate for a step
   */
  private getSelectivity(step: PlanStep): number {
    const signature = step.signature;
    const stats = this.selectivityStats.get(signature);

    if (!stats) {
      return this.defaultSelectivity;
    }

    return stats.selectivity;
  }

  /**
   * Get cardinality estimate for a step
   */
  private getCardinality(step: PlanStep): number {
    const signature = step.signature;
    const stats = this.selectivityStats.get(signature);

    if (!stats) {
      return this.defaultCardinality;
    }

    return Math.max(1, stats.avgResultCount);
  }

  /**
   * Update selectivity statistics from execution results
   */
  updateStatistics(
    stepSignature: string,
    resultCount: number,
    totalPossible: number = 1000
  ): void {
    let stats = this.selectivityStats.get(stepSignature);

    if (!stats) {
      stats = {
        signature: stepSignature,
        avgResultCount: resultCount,
        resultVariance: 0,
        executionCount: 1,
        selectivity: resultCount / totalPossible,
        lastUpdated: Date.now(),
      };
    } else {
      // Update using exponential moving average
      const alpha = 0.1; // Smoothing factor
      const n = stats.executionCount;
      const newN = n + 1;

      // Update average result count
      const prevAvg = stats.avgResultCount;
      stats.avgResultCount = prevAvg * (1 - alpha) + resultCount * alpha;

      // Update variance
      const delta = resultCount - stats.avgResultCount;
      stats.resultVariance =
        stats.resultVariance * (1 - alpha) + delta * delta * alpha;

      // Update selectivity
      stats.selectivity =
        stats.selectivity * (1 - alpha) +
        (resultCount / totalPossible) * alpha;

      stats.executionCount = newN;
      stats.lastUpdated = Date.now();
    }

    this.selectivityStats.set(stepSignature, stats);
  }

  /**
   * Import statistics from query cache
   */
  importFromQueryStatistics(queryStats: QueryStatistics[]): void {
    for (const qs of queryStats) {
      // Convert query statistics to selectivity stats
      const stats: SelectivityStats = {
        signature: qs.signature,
        avgResultCount: qs.avgResultCount,
        resultVariance: qs.durationVariance, // Use duration variance as proxy
        executionCount: qs.executionCount,
        selectivity: Math.min(1.0, qs.avgResultCount / 1000), // Assume 1000 total
        lastUpdated: qs.lastExecutedAt,
      };

      this.selectivityStats.set(stats.signature, stats);
    }
  }

  /**
   * Get all selectivity statistics (for analysis)
   */
  getStatistics(): SelectivityStats[] {
    return Array.from(this.selectivityStats.values());
  }

  /**
   * Clear all statistics
   */
  clearStatistics(): void {
    this.selectivityStats.clear();
  }

  /**
   * Export statistics for persistence
   */
  exportStatistics(): Record<string, SelectivityStats> {
    const result: Record<string, SelectivityStats> = {};
    for (const [key, stats] of this.selectivityStats.entries()) {
      result[key] = stats;
    }
    return result;
  }

  /**
   * Import statistics from persistence
   */
  importStatistics(stats: Record<string, SelectivityStats>): void {
    for (const [key, value] of Object.entries(stats)) {
      this.selectivityStats.set(key, value);
    }
  }

  /**
   * Generate join order explanation (for debugging/logging)
   */
  explainJoinOrder(steps: PlanStep[]): string {
    const lines: string[] = ['Join Order Analysis:'];

    for (let i = 0; i < steps.length; i++) {
      const step = steps[i];
      const selectivity = this.getSelectivity(step);
      const cardinality = this.getCardinality(step);

      lines.push(
        `  ${i + 1}. ${step.id} (actor: ${step.actor})`,
        `     - Selectivity: ${(selectivity * 100).toFixed(2)}%`,
        `     - Est. Results: ${Math.round(cardinality)}`,
        `     - Signature: ${step.signature}`
      );
    }

    return lines.join('\n');
  }
}
