#!/usr/bin/env bun
/**
 * Plan Formatter
 *
 * Formats query plans into human-readable text with cost analysis
 * and optimization recommendations.
 */

import type { QueryPlan, PlanStep, StepCost } from '../types.ts';
import type {
  ExplainResult,
  CostBreakdown,
  CacheAnalysis,
  OptimizationNote,
  ExplainOptions,
} from './types.ts';

/**
 * Format query plan into human-readable explanation
 */
export class PlanFormatter {
  /**
   * Format plan with options
   */
  format(plan: QueryPlan, options: ExplainOptions = {}): string {
    const sections: string[] = [];

    // Header
    sections.push(this.formatHeader(plan));
    sections.push('');

    // Overview
    sections.push(this.formatOverview(plan));
    sections.push('');

    // Steps
    sections.push(this.formatSteps(plan, options.verbose));
    sections.push('');

    // Costs (if enabled)
    if (options.costs !== false) {
      sections.push(this.formatCosts(plan));
      sections.push('');
    }

    // Cache analysis (if enabled)
    if (options.cache) {
      sections.push(this.formatCacheAnalysis(plan));
      sections.push('');
    }

    // Optimizations (if enabled)
    if (options.optimize) {
      const notes = this.generateOptimizationNotes(plan);
      if (notes.length > 0) {
        sections.push(this.formatOptimizations(notes));
        sections.push('');
      }
    }

    return sections.join('\n');
  }

  /**
   * Format header
   */
  private formatHeader(plan: QueryPlan): string {
    return `QUERY PLAN: ${plan.id}\n${'='.repeat(60)}`;
  }

  /**
   * Format overview
   */
  private formatOverview(plan: QueryPlan): string {
    const lines: string[] = ['OVERVIEW'];

    lines.push(`  Steps: ${plan.steps.length}`);
    lines.push(`  Variables: ${plan.variables.join(', ')}`);
    lines.push(
      `  Critical Path: ${plan.metadata.criticalPathSteps} steps (${plan.metadata.estimatedCost.makespan.toFixed(2)}ms)`
    );
    lines.push(
      `  Total Work: ${plan.metadata.estimatedCost.totalWork.toFixed(2)}ms`
    );
    lines.push(
      `  Parallelizable: ${plan.metadata.parallelizable ? 'Yes' : 'No'}`
    );

    // Calculate parallelism benefit
    if (plan.metadata.parallelizable) {
      const benefit =
        plan.metadata.estimatedCost.totalWork -
        plan.metadata.estimatedCost.makespan;
      if (benefit > 0) {
        lines.push(
          `  Parallelism Benefit: ${benefit.toFixed(2)}ms saved (${((benefit / plan.metadata.estimatedCost.totalWork) * 100).toFixed(1)}%)`
        );
      }
    }

    return lines.join('\n');
  }

  /**
   * Format steps
   */
  private formatSteps(plan: QueryPlan, verbose = false): string {
    const lines: string[] = ['EXECUTION STEPS'];

    for (let i = 0; i < plan.steps.length; i++) {
      const step = plan.steps[i];
      lines.push('');
      lines.push(
        `  ${i + 1}. [${step.id}] ${step.type.toUpperCase()} → ${step.actor}`
      );

      // Dependencies
      if (step.dependencies.length > 0) {
        lines.push(`     Dependencies: ${step.dependencies.join(', ')}`);
      } else {
        lines.push(`     Dependencies: None (can start immediately)`);
      }

      // Bindings
      if (step.bindings.length > 0) {
        lines.push(`     Produces: ${step.bindings.join(', ')}`);
      }

      // Message details (verbose)
      if (verbose) {
        lines.push(`     Message: ${step.message.pattern} ${step.message.type}`);
        if (step.message.payload && Object.keys(step.message.payload).length > 0) {
          lines.push(
            `     Payload: ${JSON.stringify(step.message.payload, null, 2).split('\n').join('\n              ')}`
          );
        }
      }

      // Cost estimates
      lines.push(
        `     Cost: ${step.cost.latencyMs.toFixed(2)}ms (CPU: ${step.cost.cpuMs.toFixed(2)}ms)`
      );
      lines.push(
        `     Expected Results: ${step.cost.resultCount} (cache hit prob: ${(step.cost.cacheHitProb * 100).toFixed(1)}%)`
      );

      // Parallelizable flag
      lines.push(
        `     Parallel: ${step.parallelizable ? 'Yes' : 'No (sequential)'}`
      );

      // Signature (verbose)
      if (verbose) {
        lines.push(`     Signature: ${step.signature}`);
      }
    }

    return lines.join('\n');
  }

  /**
   * Format cost breakdown
   */
  private formatCosts(plan: QueryPlan): string {
    const lines: string[] = ['COST BREAKDOWN'];

    const totalWork = plan.metadata.estimatedCost.totalWork;

    // Per-step costs
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

    lines.push('');
    lines.push('  By Step (sorted by cost):');
    for (const cost of stepCosts) {
      const bar = this.createBar(cost.percentage, 30);
      lines.push(
        `    ${cost.stepId.padEnd(12)} ${bar} ${cost.latency.toFixed(2)}ms (${cost.percentage.toFixed(1)}%)`
      );
    }

    // Resource usage
    lines.push('');
    lines.push('  Resource Usage:');
    lines.push(
      `    Memory: ${this.formatBytes(plan.metadata.estimatedCost.resourceUsage.memoryBytes)}`
    );
    lines.push(
      `    I/O Operations: ${plan.metadata.estimatedCost.resourceUsage.ioOps}`
    );
    lines.push(
      `    Messages: ${plan.metadata.estimatedCost.resourceUsage.messageCount}`
    );

    return lines.join('\n');
  }

  /**
   * Format cache analysis
   */
  private formatCacheAnalysis(plan: QueryPlan): string {
    const lines: string[] = ['CACHE ANALYSIS'];

    // Calculate overall cache statistics
    const totalSteps = plan.steps.length;
    const avgHitProb =
      plan.steps.reduce((sum, step) => sum + step.cost.cacheHitProb, 0) /
      totalSteps;
    const expectedHits = plan.steps.reduce(
      (sum, step) => sum + step.cost.cacheHitProb,
      0
    );
    const expectedMisses = totalSteps - expectedHits;

    lines.push('');
    lines.push(
      `  Overall Cache Hit Probability: ${(avgHitProb * 100).toFixed(1)}%`
    );
    lines.push(
      `  Expected Hits: ${expectedHits.toFixed(1)} / Misses: ${expectedMisses.toFixed(1)}`
    );

    // Per-step cache probabilities
    lines.push('');
    lines.push('  Per-Step Cache Probabilities:');
    for (const step of plan.steps) {
      const impact =
        step.cost.latencyMs > 20
          ? 'high'
          : step.cost.latencyMs > 5
            ? 'medium'
            : 'low';
      const bar = this.createBar(step.cost.cacheHitProb * 100, 20);
      lines.push(
        `    ${step.id.padEnd(12)} ${bar} ${(step.cost.cacheHitProb * 100).toFixed(1)}% (impact: ${impact})`
      );
    }

    // Recommendations
    const recommendations: string[] = [];
    for (const step of plan.steps) {
      if (step.cost.cacheHitProb < 0.3 && step.cost.latencyMs > 10) {
        recommendations.push(
          `Warm up ${step.actor} before query (expected ${step.cost.latencyMs.toFixed(2)}ms latency)`
        );
      }
    }

    if (recommendations.length > 0) {
      lines.push('');
      lines.push('  Recommendations:');
      for (const rec of recommendations) {
        lines.push(`    - ${rec}`);
      }
    }

    return lines.join('\n');
  }

  /**
   * Format optimization notes
   */
  private formatOptimizations(notes: OptimizationNote[]): string {
    const lines: string[] = ['OPTIMIZATION NOTES'];

    const byType = {
      info: notes.filter((n) => n.type === 'info'),
      warning: notes.filter((n) => n.type === 'warning'),
      tip: notes.filter((n) => n.type === 'tip'),
    };

    for (const [type, typeNotes] of Object.entries(byType)) {
      if (typeNotes.length > 0) {
        lines.push('');
        lines.push(`  ${type.toUpperCase()}:`);
        for (const note of typeNotes) {
          const prefix = type === 'warning' ? '⚠' : type === 'tip' ? '💡' : 'ℹ';
          lines.push(`    ${prefix} ${note.message}`);
          if (note.stepId) {
            lines.push(`      (affects ${note.stepId})`);
          }
        }
      }
    }

    return lines.join('\n');
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
        message: `Long critical path (${plan.metadata.criticalPathSteps} steps). Consider reducing dependencies.`,
      });
    }

    // Check for low parallelism utilization
    if (plan.metadata.parallelizable) {
      const benefit =
        plan.metadata.estimatedCost.totalWork -
        plan.metadata.estimatedCost.makespan;
      const utilization = benefit / plan.metadata.estimatedCost.totalWork;
      if (utilization < 0.3) {
        notes.push({
          type: 'tip',
          message: `Low parallelism utilization (${(utilization * 100).toFixed(1)}%). Most steps run sequentially.`,
        });
      }
    }

    // Check for expensive steps
    for (const step of plan.steps) {
      if (step.cost.latencyMs > 50) {
        notes.push({
          type: 'warning',
          message: `High latency step: ${step.cost.latencyMs.toFixed(2)}ms`,
          stepId: step.id,
        });
      }

      // Check for cold actors
      if (step.cost.cacheHitProb < 0.2) {
        notes.push({
          type: 'tip',
          message: `Cold actor (${(step.cost.cacheHitProb * 100).toFixed(1)}% cache hit). Consider warming up.`,
          stepId: step.id,
        });
      }
    }

    // Check for many small steps
    if (plan.steps.length > 10) {
      const avgLatency =
        plan.metadata.estimatedCost.totalWork / plan.steps.length;
      if (avgLatency < 5) {
        notes.push({
          type: 'tip',
          message: `Many small steps (${plan.steps.length} steps, ${avgLatency.toFixed(2)}ms avg). Consider batching.`,
        });
      }
    }

    // Highlight parallelism opportunities
    const parallelSteps = plan.steps.filter((s) => s.parallelizable);
    if (parallelSteps.length > 1) {
      notes.push({
        type: 'info',
        message: `${parallelSteps.length} steps can run in parallel.`,
      });
    }

    return notes;
  }

  /**
   * Create progress bar
   */
  private createBar(percentage: number, width: number): string {
    const filled = Math.round((percentage / 100) * width);
    const empty = width - filled;
    return '█'.repeat(filled) + '░'.repeat(empty);
  }

  /**
   * Format bytes
   */
  private formatBytes(bytes: number): string {
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
    return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
  }
}
