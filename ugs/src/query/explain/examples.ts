#!/usr/bin/env bun
/**
 * EXPLAIN Examples
 *
 * Demonstrates how to use EXPLAIN functionality for query analysis
 * and optimization.
 */

import { query, pattern, send, filter } from '../index.ts';
import { logic } from '../pattern.ts';
import type { ExecutionContext } from '../types.ts';
import { address } from '@agentic-primer/actors';

/**
 * Example 1: Basic EXPLAIN
 *
 * Show execution plan for a simple query
 */
export async function basicExplain() {
  console.log('=== Example 1: Basic EXPLAIN ===\n');

  const result = await query()
    .match(pattern('task').label('Task').where({ status: 'open' }))
    .return(['task'])
    .explain();

  console.log(result.text);
  console.log('\n');
  console.log(result.tree);
}

/**
 * Example 2: EXPLAIN with costs and cache analysis
 *
 * Detailed cost breakdown and cache hit predictions
 */
export async function explainWithCosts() {
  console.log('=== Example 2: EXPLAIN with Costs ===\n');

  const result = await query()
    .match(pattern('root').label('Task').where({ id: 'build' }))
    .traverse({
      from: 'root',
      relationship: 'requires',
      direction: 'outbound',
      depth: { max: 5 },
      as: 'dependencies',
    })
    .return(['root', 'dependencies'])
    .explain({ verbose: true, costs: true, cache: true });

  console.log(result.text);
  console.log('\n--- Cost Breakdown ---');
  console.log(`Total Latency: ${result.costBreakdown.totalLatency.toFixed(2)}ms`);
  console.log(`Critical Path: ${result.costBreakdown.criticalPath.toFixed(2)}ms`);
  console.log(
    `Parallelism Benefit: ${result.costBreakdown.parallelismBenefit.toFixed(2)}ms`
  );

  console.log('\n--- Cache Analysis ---');
  console.log(
    `Overall Hit Probability: ${(result.cacheAnalysis.overallHitProb * 100).toFixed(1)}%`
  );
  console.log(`Expected Hits: ${result.cacheAnalysis.expectedHits.toFixed(1)}`);
  console.log(`Expected Misses: ${result.cacheAnalysis.expectedMisses.toFixed(1)}`);
}

/**
 * Example 3: EXPLAIN with warm execution context
 *
 * Show how warm actors improve performance
 */
export async function explainWithWarmContext() {
  console.log('=== Example 3: EXPLAIN with Warm Context ===\n');

  // Cold context (first execution)
  console.log('--- Cold Execution ---');
  const coldResult = await query()
    .match(pattern('task').label('Task'))
    .return(['task'])
    .explain();

  console.log(`Cold Cache Hit Prob: ${(coldResult.cacheAnalysis.overallHitProb * 100).toFixed(1)}%`);
  console.log(`Cold Total Latency: ${coldResult.costBreakdown.totalLatency.toFixed(2)}ms`);

  // Warm context (actors already initialized)
  console.log('\n--- Warm Execution ---');
  const warmContext: ExecutionContext = {
    warmActors: new Set([address('domain/tasks'), address('domain/relationships')]),
    computationCache: new Map(),
    resources: {
      maxConcurrency: 4,
      availableMemory: 1024 * 1024 * 100,
    },
    startTime: Date.now(),
  };

  const warmResult = await query()
    .match(pattern('task').label('Task'))
    .return(['task'])
    .explain({}, warmContext);

  console.log(`Warm Cache Hit Prob: ${(warmResult.cacheAnalysis.overallHitProb * 100).toFixed(1)}%`);
  console.log(`Warm Total Latency: ${warmResult.costBreakdown.totalLatency.toFixed(2)}ms`);

  const improvement =
    ((coldResult.costBreakdown.totalLatency -
      warmResult.costBreakdown.totalLatency) /
      coldResult.costBreakdown.totalLatency) *
    100;
  console.log(`\nImprovement: ${improvement.toFixed(1)}% faster with warm actors`);
}

/**
 * Example 4: EXPLAIN with optimization recommendations
 *
 * Get suggestions for improving query performance
 */
export async function explainWithOptimizations() {
  console.log('=== Example 4: Optimization Recommendations ===\n');

  const result = await query()
    .match(
      pattern('task').label('Task').where({ status: 'open' }),
      pattern('user').label('User').where({ id: 'alice' })
    )
    .where(
      logic.and(
        filter('task', 'assignee').eq('alice'),
        filter('task', 'priority').gte('medium')
      )
    )
    .forEach(send('task').tell('assign'))
    .explain({ optimize: true });

  console.log('--- Optimization Notes ---');
  for (const note of result.optimizations) {
    const icon = note.type === 'warning' ? '⚠️' : note.type === 'tip' ? '💡' : 'ℹ️';
    console.log(`${icon} [${note.type.toUpperCase()}] ${note.message}`);
    if (note.stepId) {
      console.log(`   (affects: ${note.stepId})`);
    }
  }

  console.log('\n--- Cache Recommendations ---');
  for (const rec of result.cacheAnalysis.recommendations) {
    console.log(`  • ${rec}`);
  }
}

/**
 * Example 5: Compare different query strategies
 *
 * Use EXPLAIN to compare execution plans
 */
export async function compareQueryStrategies() {
  console.log('=== Example 5: Compare Query Strategies ===\n');

  // Strategy 1: Sequential (filter after fetch)
  console.log('--- Strategy 1: Filter After Fetch ---');
  const strategy1 = await query()
    .match(pattern('task').label('Task'))
    .where(filter('task', 'status').eq('open'))
    .return(['task'])
    .explain();

  console.log(`Steps: ${strategy1.plan.steps.length}`);
  console.log(`Total Latency: ${strategy1.costBreakdown.totalLatency.toFixed(2)}ms`);

  // Strategy 2: Direct filter in pattern (more efficient)
  console.log('\n--- Strategy 2: Filter in Pattern ---');
  const strategy2 = await query()
    .match(pattern('task').label('Task').where({ status: 'open' }))
    .return(['task'])
    .explain();

  console.log(`Steps: ${strategy2.plan.steps.length}`);
  console.log(`Total Latency: ${strategy2.costBreakdown.totalLatency.toFixed(2)}ms`);

  const diff =
    strategy1.costBreakdown.totalLatency - strategy2.costBreakdown.totalLatency;
  console.log(
    `\nStrategy 2 is ${diff.toFixed(2)}ms faster (${((diff / strategy1.costBreakdown.totalLatency) * 100).toFixed(1)}% improvement)`
  );
}

/**
 * Example 6: Execution flow visualization
 *
 * Show stage-by-stage execution with parallelism
 */
export async function visualizeExecutionFlow() {
  console.log('=== Example 6: Execution Flow ===\n');

  const result = await query()
    .match(
      pattern('task1').label('Task').where({ id: '1' }),
      pattern('task2').label('Task').where({ id: '2' })
    )
    .traverse({
      from: 'task1',
      relationship: 'requires',
      direction: 'outbound',
      as: 'deps1',
    })
    .traverse({
      from: 'task2',
      relationship: 'requires',
      direction: 'outbound',
      as: 'deps2',
    })
    .return(['task1', 'task2', 'deps1', 'deps2'])
    .explain();

  console.log(result.tree);
}

/**
 * Example 7: Complex workflow EXPLAIN
 *
 * Analyze a real workflow query
 */
export async function explainComplexWorkflow() {
  console.log('=== Example 7: Complex Workflow Analysis ===\n');

  const result = await query()
    .match(
      pattern('test').label('Task').where({ id: 'test' }),
      pattern('build').label('Task').where({ id: 'build' }),
      pattern('deploy').label('Task').where({ id: 'deploy' })
    )
    .traverse({
      from: 'build',
      relationship: 'requires',
      direction: 'outbound',
      depth: { max: 3 },
      as: 'dependencies',
    })
    .when(
      pattern('test').where({
        lifecycle: 'completed',
        result: { passed: true },
      })
    )
    .then(send('deploy').tell('start'))
    .explain({ verbose: true, costs: true, optimize: true });

  console.log(result.text);
  console.log('\n');
  console.log(result.tree);
}

/**
 * Run all examples
 */
export async function runAllExamples() {
  const examples = [
    { name: 'Basic EXPLAIN', fn: basicExplain },
    { name: 'Costs & Cache', fn: explainWithCosts },
    { name: 'Warm Context', fn: explainWithWarmContext },
    { name: 'Optimizations', fn: explainWithOptimizations },
    { name: 'Compare Strategies', fn: compareQueryStrategies },
    { name: 'Execution Flow', fn: visualizeExecutionFlow },
    { name: 'Complex Workflow', fn: explainComplexWorkflow },
  ];

  for (const example of examples) {
    try {
      await example.fn();
      console.log('\n' + '='.repeat(80) + '\n');
    } catch (error) {
      console.error(`Error in ${example.name}:`, error);
    }
  }
}

// Run if executed directly
if (import.meta.main) {
  await runAllExamples();
}
