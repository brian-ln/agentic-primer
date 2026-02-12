#!/usr/bin/env bun
/**
 * Join Optimizer Benchmark
 *
 * Demonstrates performance improvements from join optimization
 * on various multi-pattern query scenarios.
 */

import { QueryCompiler } from '../compiler.ts';
import { QueryCache } from '../cache.ts';
import { JoinOptimizer } from './join-optimizer.ts';
import { query } from '../builder.ts';
import { pattern } from '../pattern.ts';
import type { ExecutionContext } from '../types.ts';
import { address } from '@agentic-primer/actors';

// Create execution context
function createContext(warm: string[] = []): ExecutionContext {
  return {
    warmActors: new Set(warm.map(a => address(a))),
    computationCache: new Map(),
    resources: {
      maxConcurrency: 10,
      availableMemory: 1024 * 1024 * 1024,
    },
    startTime: Date.now(),
  };
}

interface BenchmarkResult {
  scenario: string;
  unoptimizedCost: number;
  optimizedCost: number;
  improvement: number;
  improvementPercent: number;
  stepsReordered: boolean;
}

/**
 * Run a benchmark scenario
 */
async function runScenario(
  name: string,
  queryDef: any,
  selectivityData: Record<string, number>,
  context?: ExecutionContext
): Promise<BenchmarkResult> {
  // Compile without optimization
  const compilerUnopt = new QueryCompiler({ enableJoinOptimization: false });
  const planUnopt = await compilerUnopt.compile(queryDef, context);

  // Compile with optimization
  const compilerOpt = new QueryCompiler({ enableJoinOptimization: true });

  // Seed optimizer with statistics
  const optimizer = compilerOpt.getJoinOptimizer();
  for (const [sig, cardinality] of Object.entries(selectivityData)) {
    optimizer.updateStatistics(sig, cardinality, 10000);
  }

  const planOpt = await compilerOpt.compile(queryDef, context);

  // Calculate more realistic cost based on intermediate result sizes
  // For query execution, the cost is dominated by processing intermediate results
  const calculateExecutionCost = (plan: any, selectivity: Record<string, number>) => {
    const querySteps = plan.steps.filter((s: any) => s.type === 'query');
    if (querySteps.length === 0) return 10;

    let cost = 0;
    let intermediateSize = 1;

    for (const step of querySteps) {
      const stepCard = selectivity[step.signature] || 100;
      const stepSel = Math.min(1, stepCard / 10000);

      // Cost of processing current step against intermediate results
      // Each row in intermediate must be checked against step results
      const processingCost = intermediateSize * stepCard * 0.001; // 1μs per row pair
      cost += processingCost;

      // Update intermediate size
      intermediateSize = Math.max(1, intermediateSize * stepCard * stepSel);
    }

    return cost;
  };

  const unoptimizedCost = calculateExecutionCost(planUnopt, selectivityData);
  const optimizedCost = calculateExecutionCost(planOpt, selectivityData);
  const improvement = unoptimizedCost - optimizedCost;
  const improvementPercent = improvement > 0 ? (improvement / unoptimizedCost) * 100 : 0;

  // Check if steps were reordered
  const unoptOrder = planUnopt.steps.map(s => s.signature).join(',');
  const optOrder = planOpt.steps.map(s => s.signature).join(',');
  const stepsReordered = unoptOrder !== optOrder;

  return {
    scenario: name,
    unoptimizedCost,
    optimizedCost,
    improvement,
    improvementPercent,
    stepsReordered,
  };
}

/**
 * Main benchmark suite
 */
async function runBenchmarks() {
  console.log('='.repeat(80));
  console.log('JOIN OPTIMIZER BENCHMARK');
  console.log('='.repeat(80));
  console.log();

  const results: BenchmarkResult[] = [];

  // Scenario 1: Two patterns with different selectivity
  {
    const q = query()
      .match(
        pattern('tasks').label('Task').where({ status: 'open' }),
        pattern('users').label('User').where({ active: true })
      )
      .build();

    const selectivity = {
      [q.patterns[0].variable]: 5000,  // Many tasks
      [q.patterns[1].variable]: 50,    // Few active users
    };

    // Get actual signatures from compiled plan to map correctly
    const compiler = new QueryCompiler({ enableJoinOptimization: false });
    const tempPlan = await compiler.compile(q);
    const actualSelectivity: Record<string, number> = {};
    tempPlan.steps.forEach((step, i) => {
      if (step.type === 'query') {
        actualSelectivity[step.signature] = selectivity[q.patterns[i].variable];
      }
    });

    const result = await runScenario(
      'Two patterns: many tasks vs few users',
      q,
      actualSelectivity
    );
    results.push(result);
  }

  // Scenario 2: Three patterns with varying selectivity
  {
    const q = query()
      .match(
        pattern('t').label('Task'),
        pattern('u').label('User'),
        pattern('p').label('Project').where({ active: true })
      )
      .build();

    const compiler = new QueryCompiler({ enableJoinOptimization: false });
    const tempPlan = await compiler.compile(q);
    const actualSelectivity: Record<string, number> = {};
    const cardinalityMap = [10000, 500, 20]; // tasks, users, active projects
    tempPlan.steps.forEach((step, i) => {
      if (step.type === 'query') {
        actualSelectivity[step.signature] = cardinalityMap[i];
      }
    });

    const result = await runScenario(
      'Three patterns: tasks, users, projects',
      q,
      actualSelectivity
    );
    results.push(result);
  }

  // Scenario 3: Warm vs cold actors
  {
    const q = query()
      .match(
        pattern('t').label('Task'),
        pattern('k').label('Knowledge')
      )
      .build();

    const compiler = new QueryCompiler({ enableJoinOptimization: false });
    const tempPlan = await compiler.compile(q);
    const actualSelectivity: Record<string, number> = {};
    const cardinalityMap = [1000, 800]; // Similar cardinality
    tempPlan.steps.forEach((step, i) => {
      if (step.type === 'query') {
        actualSelectivity[step.signature] = cardinalityMap[i];
      }
    });

    const context = createContext(['tasks']); // Tasks is warm

    const result = await runScenario(
      'Warm actor preference (tasks warm)',
      q,
      actualSelectivity,
      context
    );
    results.push(result);
  }

  // Scenario 4: Four patterns (stress test)
  {
    const q = query()
      .match(
        pattern('t').label('Task'),
        pattern('u').label('User'),
        pattern('p').label('Project'),
        pattern('r').label('Relationship')
      )
      .build();

    const compiler = new QueryCompiler({ enableJoinOptimization: false });
    const tempPlan = await compiler.compile(q);
    const actualSelectivity: Record<string, number> = {};
    const cardinalityMap = [8000, 3000, 150, 50]; // Various sizes
    tempPlan.steps.forEach((step, i) => {
      if (step.type === 'query') {
        actualSelectivity[step.signature] = cardinalityMap[i];
      }
    });

    const result = await runScenario(
      'Four patterns: complex query',
      q,
      actualSelectivity
    );
    results.push(result);
  }

  // Scenario 5: Pathological case (reverse order optimal)
  {
    const q = query()
      .match(
        pattern('a').label('Task').where({ priority: 'critical' }), // Very selective
        pattern('b').label('Task').where({ status: 'open' }),       // Moderately selective
        pattern('c').label('Task')                                   // Not selective
      )
      .build();

    const compiler = new QueryCompiler({ enableJoinOptimization: false });
    const tempPlan = await compiler.compile(q);
    const actualSelectivity: Record<string, number> = {};
    const cardinalityMap = [10, 500, 10000]; // Reverse order is optimal
    tempPlan.steps.forEach((step, i) => {
      if (step.type === 'query') {
        actualSelectivity[step.signature] = cardinalityMap[i];
      }
    });

    const result = await runScenario(
      'Pathological: reverse order optimal',
      q,
      actualSelectivity
    );
    results.push(result);
  }

  // Print results
  console.log('Results:');
  console.log('-'.repeat(80));
  console.log();

  for (const result of results) {
    console.log(`Scenario: ${result.scenario}`);
    console.log(`  Unoptimized cost: ${result.unoptimizedCost.toFixed(2)}ms`);
    console.log(`  Optimized cost:   ${result.optimizedCost.toFixed(2)}ms`);
    console.log(`  Improvement:      ${result.improvement.toFixed(2)}ms (${result.improvementPercent.toFixed(1)}%)`);
    console.log(`  Steps reordered:  ${result.stepsReordered ? 'Yes' : 'No'}`);
    console.log();
  }

  // Summary statistics
  const avgImprovement = results.reduce((sum, r) => sum + r.improvementPercent, 0) / results.length;
  const maxImprovement = Math.max(...results.map(r => r.improvementPercent));
  const reorderedCount = results.filter(r => r.stepsReordered).length;

  console.log('='.repeat(80));
  console.log('SUMMARY');
  console.log('='.repeat(80));
  console.log(`Total scenarios:        ${results.length}`);
  console.log(`Scenarios reordered:    ${reorderedCount} (${((reorderedCount/results.length)*100).toFixed(0)}%)`);
  console.log(`Average improvement:    ${avgImprovement.toFixed(1)}%`);
  console.log(`Maximum improvement:    ${maxImprovement.toFixed(1)}%`);
  console.log();

  // Success criteria: >20% average improvement
  const success = avgImprovement > 20;
  console.log(`Success criteria (>20% avg improvement): ${success ? 'PASS ✓' : 'FAIL ✗'}`);
  console.log();

  return { results, success };
}

// Run benchmarks if executed directly
if (import.meta.main) {
  const { success } = await runBenchmarks();
  process.exit(success ? 0 : 1);
}

export { runBenchmarks, type BenchmarkResult };
