#!/usr/bin/env bun
/**
 * Query Explainer
 *
 * Main entry point for EXPLAIN functionality. Orchestrates plan formatting,
 * visualization, and analysis.
 */

import type { QueryPlan } from '../types.ts';
import type {
  ExplainResult,
  ExplainOptions,
  CostBreakdown,
  CacheAnalysis,
  OptimizationNote,
} from './types.ts';
import { PlanFormatter } from './plan-formatter.ts';
import { PlanVisualizer } from './plan-visualizer.ts';

/**
 * Query explainer - generates EXPLAIN output
 */
export class QueryExplainer {
  private formatter: PlanFormatter;
  private visualizer: PlanVisualizer;

  constructor() {
    this.formatter = new PlanFormatter();
    this.visualizer = new PlanVisualizer();
  }

  /**
   * Explain a query plan
   */
  explain(plan: QueryPlan, options: ExplainOptions = {}): ExplainResult {
    // Default options
    const opts: Required<ExplainOptions> = {
      verbose: options.verbose ?? false,
      costs: options.costs ?? true,
      cache: options.cache ?? true,
      optimize: options.optimize ?? true,
      format: options.format ?? 'both',
    };

    // Generate formatted text
    const text = this.formatter.format(plan, opts);

    // Generate tree visualization
    const tree = this.visualizer.visualize(plan, opts.costs);

    // Generate cost breakdown
    const costBreakdown = this.generateCostBreakdown(plan);

    // Generate cache analysis
    const cacheAnalysis = this.generateCacheAnalysis(plan);

    // Generate optimization notes
    const optimizations = this.generateOptimizationNotes(plan);

    return {
      plan,
      text,
      tree,
      optimizations,
      costBreakdown,
      cacheAnalysis,
    };
  }

  /**
   * Explain with flow diagram
   */
  explainFlow(plan: QueryPlan): string {
    return this.visualizer.visualizeFlow(plan);
  }

  /**
   * Generate cost breakdown
   */
  private generateCostBreakdown(plan: QueryPlan): CostBreakdown {
    const totalWork = plan.metadata.estimatedCost.totalWork;
    const criticalPath = plan.metadata.estimatedCost.makespan;
    const parallelismBenefit = totalWork - criticalPath;

    const stepCosts = plan.steps.map((step) => ({
      stepId: step.id,
      type: step.type,
      latency: step.cost.latencyMs,
      cpuMs: step.cost.cpuMs,
      resultCount: step.cost.resultCount,
      percentage: (step.cost.latencyMs / totalWork) * 100,
    }));

    // Sort by latency descending
    stepCosts.sort((a, b) => b.latency - a.latency);

    return {
      totalLatency: totalWork,
      criticalPath,
      parallelismBenefit,
      stepCosts,
      resources: plan.metadata.estimatedCost.resourceUsage,
    };
  }

  /**
   * Generate cache analysis
   */
  private generateCacheAnalysis(plan: QueryPlan): CacheAnalysis {
    const totalSteps = plan.steps.length;
    const avgHitProb =
      plan.steps.reduce((sum, step) => sum + step.cost.cacheHitProb, 0) /
      totalSteps;
    const expectedHits = plan.steps.reduce(
      (sum, step) => sum + step.cost.cacheHitProb,
      0
    );
    const expectedMisses = totalSteps - expectedHits;

    const stepCacheProbs = plan.steps.map((step) => {
      const impact =
        step.cost.latencyMs > 20
          ? 'high'
          : step.cost.latencyMs > 5
            ? 'medium'
            : 'low';
      return {
        stepId: step.id,
        hitProb: step.cost.cacheHitProb,
        impact: impact as 'high' | 'medium' | 'low',
      };
    });

    // Generate recommendations
    const recommendations: string[] = [];
    for (const step of plan.steps) {
      if (step.cost.cacheHitProb < 0.3 && step.cost.latencyMs > 10) {
        recommendations.push(
          `Warm up ${step.actor} before executing (expected ${step.cost.latencyMs.toFixed(2)}ms latency)`
        );
      }
    }

    if (avgHitProb < 0.5) {
      recommendations.push(
        'Overall low cache hit rate. Consider warming up actors or executing similar queries first.'
      );
    }

    return {
      overallHitProb: avgHitProb,
      expectedHits,
      expectedMisses,
      stepCacheProbs,
      recommendations,
    };
  }

  /**
   * Generate optimization notes
   */
  private generateOptimizationNotes(plan: QueryPlan): OptimizationNote[] {
    const notes: OptimizationNote[] = [];

    // Check for long critical path
    if (plan.metadata.criticalPathSteps > 5) {
      notes.push({
        type: 'warning',
        message: `Long critical path (${plan.metadata.criticalPathSteps} steps). Consider reducing dependencies or restructuring query.`,
      });
    }

    // Check for low parallelism utilization
    if (plan.metadata.parallelizable) {
      const benefit =
        plan.metadata.estimatedCost.totalWork -
        plan.metadata.estimatedCost.makespan;
      const utilization = benefit / plan.metadata.estimatedCost.totalWork;
      if (utilization > 0.5) {
        notes.push({
          type: 'info',
          message: `Good parallelism utilization (${(utilization * 100).toFixed(1)}%). Query will benefit from concurrent execution.`,
        });
      } else if (utilization < 0.2) {
        notes.push({
          type: 'warning',
          message: `Low parallelism utilization (${(utilization * 100).toFixed(1)}%). Most steps run sequentially.`,
        });
      }
    } else {
      notes.push({
        type: 'info',
        message: 'Query is fully sequential (no parallelism opportunities).',
      });
    }

    // Check for expensive steps
    const expensiveSteps = plan.steps.filter(
      (s) => s.cost.latencyMs > 50
    );
    if (expensiveSteps.length > 0) {
      notes.push({
        type: 'warning',
        message: `${expensiveSteps.length} expensive step(s) detected (>50ms). These dominate execution time.`,
      });
      for (const step of expensiveSteps) {
        notes.push({
          type: 'tip',
          message: `Consider optimizing or caching results (${step.cost.latencyMs.toFixed(2)}ms)`,
          stepId: step.id,
        });
      }
    }

    // Check for cold actors
    const coldSteps = plan.steps.filter(
      (s) => s.cost.cacheHitProb < 0.3
    );
    if (coldSteps.length > 0) {
      notes.push({
        type: 'tip',
        message: `${coldSteps.length} step(s) with cold actors. Warm-up could reduce latency by ~50%.`,
      });
    }

    // Check for many small steps
    if (plan.steps.length > 10) {
      const avgLatency =
        plan.metadata.estimatedCost.totalWork / plan.steps.length;
      if (avgLatency < 5) {
        notes.push({
          type: 'tip',
          message: `Many small steps (${plan.steps.length} steps, ${avgLatency.toFixed(2)}ms avg). Consider batching to reduce message overhead.`,
        });
      }
    }

    // Resource usage warnings
    const memoryMB =
      plan.metadata.estimatedCost.resourceUsage.memoryBytes / (1024 * 1024);
    if (memoryMB > 100) {
      notes.push({
        type: 'warning',
        message: `High memory usage (${memoryMB.toFixed(1)}MB). Consider streaming or pagination.`,
      });
    }

    // Message count efficiency
    if (plan.metadata.estimatedCost.resourceUsage.messageCount > 50) {
      notes.push({
        type: 'warning',
        message: `High message count (${plan.metadata.estimatedCost.resourceUsage.messageCount}). Message overhead may be significant.`,
      });
    }

    return notes;
  }

  /**
   * Generate compact summary (one-liner)
   */
  summary(plan: QueryPlan): string {
    const steps = plan.steps.length;
    const makespan = plan.metadata.estimatedCost.makespan.toFixed(1);
    const totalWork = plan.metadata.estimatedCost.totalWork.toFixed(1);
    const parallel = plan.metadata.parallelizable ? 'parallel' : 'sequential';

    return `${steps} steps, ${makespan}ms critical path (${totalWork}ms total), ${parallel}`;
  }
}
