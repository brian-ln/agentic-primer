#!/usr/bin/env bun
/**
 * Query Optimizer Module
 *
 * Exports all optimization components for query plans.
 */

export { JoinOptimizer } from './join-optimizer.ts';
export { PredicatePushdownOptimizer, optimizePlan } from './predicate-pushdown.ts';
export { getIndexSelector } from './index-selector.ts';

export type {
  OptimizationResult,
  OptimizationStats,
} from './predicate-pushdown.ts';
