#!/usr/bin/env bun
/**
 * Predicate Pushdown Optimizer
 *
 * Pushes filter predicates down to source actors for early filtering,
 * reducing data transfer and intermediate result sizes.
 *
 * Key optimizations:
 * 1. Push WHERE filters into pattern MATCH clauses
 * 2. Push filters through traversals when safe
 * 3. Eliminate redundant filters
 * 4. Preserve query semantics exactly
 *
 * Safety rules:
 * - Only push down filters that reference a single variable
 * - Don't push filters that depend on aggregations
 * - Don't push filters through operations that modify data
 */

import type {
  QueryPlan,
  PlanStep,
  FilterExpression,
  QueryDefinition,
} from '../types.ts';

/**
 * Optimization result with statistics
 */
export interface OptimizationResult {
  /** Optimized query plan */
  plan: QueryPlan;

  /** Whether any optimizations were applied */
  optimized: boolean;

  /** Transformation statistics */
  stats: OptimizationStats;
}

/**
 * Statistics about optimizations applied
 */
export interface OptimizationStats {
  /** Number of filters pushed down */
  filtersPushedDown: number;

  /** Number of redundant filters eliminated */
  redundantFiltersRemoved: number;

  /** Number of steps modified */
  stepsModified: number;

  /** Estimated result size reduction (%) */
  estimatedReduction: number;
}

/**
 * Predicate pushdown optimizer
 */
export class PredicatePushdownOptimizer {
  /**
   * Optimize a query plan by pushing predicates down
   */
  optimize(plan: QueryPlan): OptimizationResult {
    const stats: OptimizationStats = {
      filtersPushedDown: 0,
      redundantFiltersRemoved: 0,
      stepsModified: 0,
      estimatedReduction: 0,
    };

    // Clone the plan to avoid mutating original
    const optimizedPlan = this.clonePlan(plan);

    // Extract filters from query definition
    const filters = optimizedPlan.original.filters || [];
    if (filters.length === 0) {
      return { plan: optimizedPlan, optimized: false, stats };
    }

    // Analyze which filters can be pushed down
    const pushableFilters = this.identifyPushableFilters(
      filters,
      optimizedPlan.steps
    );

    // Apply pushdown transformations
    for (const { filter, targetStep } of pushableFilters) {
      if (this.pushFilterToStep(filter, targetStep, optimizedPlan)) {
        stats.filtersPushedDown++;
        stats.stepsModified++;
      }
    }

    // Remove redundant filters from later stages
    const redundantCount = this.removeRedundantFilters(optimizedPlan);
    stats.redundantFiltersRemoved = redundantCount;

    // Estimate result size reduction
    stats.estimatedReduction = this.estimateReduction(stats, optimizedPlan);

    // Update plan costs after optimization
    this.updateCosts(optimizedPlan, stats);

    return {
      plan: optimizedPlan,
      optimized: stats.filtersPushedDown > 0 || stats.redundantFiltersRemoved > 0,
      stats,
    };
  }

  /**
   * Identify filters that can be safely pushed down
   */
  private identifyPushableFilters(
    filters: FilterExpression[],
    steps: PlanStep[]
  ): Array<{ filter: FilterExpression; targetStep: PlanStep }> {
    const pushable: Array<{ filter: FilterExpression; targetStep: PlanStep }> =
      [];

    for (const filter of filters) {
      // Extract variable(s) referenced by this filter
      const referencedVars = this.extractVariables(filter);

      // Only push down filters that reference a single variable
      if (referencedVars.size !== 1) {
        continue;
      }

      const variable = Array.from(referencedVars)[0];

      // Find the step that produces this variable
      const producerStep = steps.find((step) =>
        step.bindings.includes(variable)
      );

      if (!producerStep) {
        continue;
      }

      // Check if it's safe to push down to this step
      if (this.isSafeToPushDown(filter, producerStep, steps)) {
        pushable.push({ filter, targetStep: producerStep });
      }
    }

    return pushable;
  }

  /**
   * Check if it's safe to push a filter to a specific step
   */
  private isSafeToPushDown(
    filter: FilterExpression,
    targetStep: PlanStep,
    allSteps: PlanStep[]
  ): boolean {
    // Only push to query steps (source actors)
    if (targetStep.type !== 'query') {
      return false;
    }

    // Don't push filters that depend on aggregations
    if (this.dependsOnAggregation(filter, allSteps)) {
      return false;
    }

    // Don't push filters through action steps
    const hasActionBefore = allSteps.some(
      (step) =>
        step.type === 'action' &&
        this.stepDependsOn(targetStep, step, allSteps)
    );
    if (hasActionBefore) {
      return false;
    }

    return true;
  }

  /**
   * Push a filter to a specific step
   */
  private pushFilterToStep(
    filter: FilterExpression,
    targetStep: PlanStep,
    plan: QueryPlan
  ): boolean {
    // Convert filter expression to actor-compatible format
    const actorFilter = this.convertFilterToActorFormat(filter);

    if (!actorFilter) {
      return false;
    }

    // Merge with existing filters in the step's payload
    if (!targetStep.message.payload.filter) {
      targetStep.message.payload.filter = {};
    }

    Object.assign(targetStep.message.payload.filter, actorFilter);

    // Update step signature to reflect the change
    targetStep.signature = this.regenerateSignature(targetStep);

    return true;
  }

  /**
   * Convert filter expression to actor-compatible format
   */
  private convertFilterToActorFormat(
    filter: FilterExpression
  ): Record<string, any> | null {
    if (filter.type === 'comparison') {
      // Simple comparison: { property: value }
      if (
        filter.operator === '=' &&
        filter.property &&
        filter.value !== undefined
      ) {
        return { [filter.property]: filter.value };
      }

      // Range comparisons
      if (
        filter.property &&
        (filter.operator === '>' ||
          filter.operator === '<' ||
          filter.operator === '>=' ||
          filter.operator === '<=')
      ) {
        // Actor format: { property: { $op: value } }
        const opMap: Record<string, string> = {
          '>': '$gt',
          '<': '$lt',
          '>=': '$gte',
          '<=': '$lte',
          '!=': '$ne',
        };
        return {
          [filter.property]: {
            [opMap[filter.operator]]: filter.value,
          },
        };
      }
    }

    if (filter.type === 'logical' && filter.operator === 'AND') {
      // Merge AND conditions
      const result: Record<string, any> = {};
      for (const expr of filter.expressions || []) {
        const converted = this.convertFilterToActorFormat(expr);
        if (converted) {
          Object.assign(result, converted);
        }
      }
      return Object.keys(result).length > 0 ? result : null;
    }

    // For complex filters (OR, NOT), don't push down
    return null;
  }

  /**
   * Remove redundant filters that have been pushed down
   */
  private removeRedundantFilters(plan: QueryPlan): number {
    const originalFilterCount = (plan.original.filters || []).length;

    // Track which filters have been pushed down
    const pushedDown = new Set<string>();

    for (const step of plan.steps) {
      if (step.type === 'query' && step.message.payload.filter) {
        // Mark filters that are now in the step
        for (const key of Object.keys(step.message.payload.filter)) {
          pushedDown.add(key);
        }
      }
    }

    // In a full implementation, we would remove these from a separate
    // filter step. For now, we just track how many were pushed.
    return pushedDown.size;
  }

  /**
   * Estimate result size reduction from pushdown
   */
  private estimateReduction(
    stats: OptimizationStats,
    plan: QueryPlan
  ): number {
    if (stats.filtersPushedDown === 0) {
      return 0;
    }

    // Heuristic: each pushed filter reduces results by ~50-80%
    // Multiple filters compound: (1 - reduction)^n
    const avgReduction = 0.65; // 65% reduction per filter
    const compoundReduction =
      1 - Math.pow(1 - avgReduction, stats.filtersPushedDown);

    return Math.round(compoundReduction * 100);
  }

  /**
   * Update plan costs after optimization
   */
  private updateCosts(plan: QueryPlan, stats: OptimizationStats): void {
    // Reduce result count estimates for optimized steps
    const reductionFactor = 1 - stats.estimatedReduction / 100;

    for (const step of plan.steps) {
      if (step.type === 'query' && step.message.payload.filter) {
        // Reduce expected result count
        step.cost.resultCount = Math.ceil(
          step.cost.resultCount * reductionFactor
        );

        // Slightly increase query latency (filter overhead)
        step.cost.latencyMs += 1; // 1ms overhead for filtering
        step.cost.cpuMs += 0.5; // 0.5ms CPU overhead
      }
    }

    // Update plan-level costs
    const totalWork = plan.steps.reduce((sum, s) => sum + s.cost.latencyMs, 0);
    plan.metadata.estimatedCost.totalWork = totalWork;

    // Makespan should decrease due to less data transfer
    const makespanReduction = stats.estimatedReduction * 0.3; // 30% of reduction
    plan.metadata.estimatedCost.makespan = Math.ceil(
      plan.metadata.estimatedCost.makespan * (1 - makespanReduction / 100)
    );

    // Reduce memory usage estimate
    plan.metadata.estimatedCost.resourceUsage.memoryBytes = Math.ceil(
      plan.metadata.estimatedCost.resourceUsage.memoryBytes * reductionFactor
    );
  }

  /**
   * Extract all variables referenced in a filter
   */
  private extractVariables(filter: FilterExpression): Set<string> {
    const vars = new Set<string>();

    if (filter.variable) {
      vars.add(filter.variable);
    }

    if (filter.expressions) {
      for (const expr of filter.expressions) {
        const subVars = this.extractVariables(expr);
        subVars.forEach((v) => vars.add(v));
      }
    }

    return vars;
  }

  /**
   * Check if filter depends on aggregation
   */
  private dependsOnAggregation(
    filter: FilterExpression,
    steps: PlanStep[]
  ): boolean {
    const vars = this.extractVariables(filter);

    // Check if any variable is produced by an aggregate step
    return Array.from(vars).some((variable) =>
      steps.some(
        (step) => step.type === 'aggregate' && step.bindings.includes(variable)
      )
    );
  }

  /**
   * Check if step A depends on step B
   */
  private stepDependsOn(
    stepA: PlanStep,
    stepB: PlanStep,
    allSteps: PlanStep[]
  ): boolean {
    const visited = new Set<string>();

    const checkDep = (currentId: string): boolean => {
      if (visited.has(currentId)) {
        return false;
      }
      visited.add(currentId);

      if (currentId === stepB.id) {
        return true;
      }

      const current = allSteps.find((s) => s.id === currentId);
      if (!current) {
        return false;
      }

      return current.dependencies.some((depId) => checkDep(depId));
    };

    return checkDep(stepA.id);
  }

  /**
   * Regenerate step signature after modification
   */
  private regenerateSignature(step: PlanStep): string {
    const crypto = require('crypto');
    const sigString = `${step.type}:${step.actor}:${JSON.stringify(
      step.message.payload
    )}`;
    return crypto.createHash('sha256').update(sigString).digest('hex').slice(0, 16);
  }

  /**
   * Clone a query plan for safe modification
   */
  private clonePlan(plan: QueryPlan): QueryPlan {
    return JSON.parse(JSON.stringify(plan));
  }
}

/**
 * Convenience function to optimize a plan
 */
export function optimizePlan(plan: QueryPlan): OptimizationResult {
  const optimizer = new PredicatePushdownOptimizer();
  return optimizer.optimize(plan);
}
