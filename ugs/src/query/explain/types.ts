#!/usr/bin/env bun
/**
 * EXPLAIN Types
 *
 * Type definitions for query plan explanation and visualization.
 */

import type { QueryPlan, PlanStep } from '../types.ts';

/**
 * Explain result - contains plan and formatted output
 */
export interface ExplainResult {
  /** Original query plan */
  plan: QueryPlan;

  /** Human-readable text explanation */
  text: string;

  /** ASCII tree visualization */
  tree: string;

  /** Optimization notes */
  optimizations: OptimizationNote[];

  /** Cost breakdown */
  costBreakdown: CostBreakdown;

  /** Cache analysis */
  cacheAnalysis: CacheAnalysis;
}

/**
 * Optimization note
 */
export interface OptimizationNote {
  type: 'info' | 'warning' | 'tip';
  message: string;
  stepId?: string;
}

/**
 * Cost breakdown
 */
export interface CostBreakdown {
  /** Total estimated latency (ms) */
  totalLatency: number;

  /** Critical path latency (ms) */
  criticalPath: number;

  /** Potential parallelism benefit (ms saved) */
  parallelismBenefit: number;

  /** Per-step costs */
  stepCosts: Array<{
    stepId: string;
    type: string;
    latency: number;
    cpuMs: number;
    resultCount: number;
    percentage: number;
  }>;

  /** Resource usage */
  resources: {
    memoryBytes: number;
    ioOps: number;
    messageCount: number;
  };
}

/**
 * Cache analysis
 */
export interface CacheAnalysis {
  /** Overall cache hit probability */
  overallHitProb: number;

  /** Expected cache hits */
  expectedHits: number;

  /** Expected cache misses */
  expectedMisses: number;

  /** Per-step cache probabilities */
  stepCacheProbs: Array<{
    stepId: string;
    hitProb: number;
    impact: 'high' | 'medium' | 'low';
  }>;

  /** Cache recommendations */
  recommendations: string[];
}

/**
 * Tree node for ASCII visualization
 */
export interface TreeNode {
  /** Step this node represents */
  step: PlanStep;

  /** Child nodes (dependencies) */
  children: TreeNode[];

  /** Display label */
  label: string;

  /** Display details */
  details: string[];
}

/**
 * Explain options
 */
export interface ExplainOptions {
  /** Show verbose details */
  verbose?: boolean;

  /** Show costs */
  costs?: boolean;

  /** Show cache analysis */
  cache?: boolean;

  /** Show optimization tips */
  optimize?: boolean;

  /** Output format */
  format?: 'text' | 'json' | 'both';
}
