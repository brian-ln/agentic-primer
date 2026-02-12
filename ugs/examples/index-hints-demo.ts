#!/usr/bin/env bun
/**
 * Index Hints Demo
 *
 * Demonstrates the full capabilities of the index hints system:
 * 1. Manual index hints
 * 2. Automatic index selection
 * 3. Index effectiveness tracking
 * 4. Historical learning
 */

import { query } from '../src/query/builder.ts';
import { pattern } from '../src/query/pattern.ts';
import { QueryCompiler } from '../src/query/compiler.ts';
import { QueryCache } from '../src/query/cache.ts';
import { getIndexSelector } from '../src/query/optimizer/index-selector.ts';
import type { ExecutionContext, ExecutionStats } from '../src/query/types.ts';
import { address } from '../src/messaging/message.ts';

// Helper to create execution context
function createContext(): ExecutionContext {
  return {
    warmActors: new Set([address('domain/tasks'), address('domain/users')]),
    computationCache: new Map(),
    resources: {
      maxConcurrency: 10,
      availableMemory: 1024 * 1024 * 1024,
    },
    startTime: Date.now(),
  };
}

// Simulate query execution
function simulateExecution(planId: string, withIndex: boolean): ExecutionStats {
  const baseLatency = 50;
  const improvement = withIndex ? 0.6 : 1.0; // 40% improvement with index

  return {
    durationMs: baseLatency * improvement,
    stepsExecuted: 1,
    messagesSent: 1,
    cacheHits: 0,
    cacheMisses: 1,
    resultsReturned: withIndex ? 25 : 100, // Better filtering with index
    stepStats: new Map(),
  };
}

async function demo() {
  console.log('🚀 Index Hints Demo\n');
  console.log('='.repeat(60));

  const compiler = new QueryCompiler();
  const cache = new QueryCache();
  const context = createContext();

  // ================================================================
  // 1. Manual Index Hints
  // ================================================================
  console.log('\n1️⃣  MANUAL INDEX HINTS\n');

  const manualQuery = query()
    .match(pattern('task').label('Task').where({ status: 'open', priority: 'high' }))
    .useIndex('task', 'status', 'Critical path optimization')
    .useIndex('task', 'priority', 'High-priority queries are frequent')
    .return(['task'])
    .build();

  console.log('Query:');
  console.log('  MATCH (task:Task { status: "open", priority: "high" })');
  console.log('  USE INDEX task.status -- Critical path optimization');
  console.log('  USE INDEX task.priority -- High-priority queries are frequent');
  console.log('  RETURN task\n');

  const manualPlan = await compiler.compile(manualQuery, context);

  console.log('Plan Metadata:');
  console.log(`  Index Hints: ${manualPlan.metadata.indexHints?.length || 0}`);
  for (const hint of manualPlan.metadata.indexHints || []) {
    console.log(`    - ${hint.variable}.${hint.index} (${hint.source}, confidence: ${hint.confidence})`);
    console.log(`      Reason: ${hint.reason}`);
  }
  console.log(`  Estimated Makespan: ${manualPlan.metadata.estimatedCost.makespan.toFixed(1)}ms`);

  // ================================================================
  // 2. Automatic Index Selection
  // ================================================================
  console.log('\n2️⃣  AUTOMATIC INDEX SELECTION\n');

  const autoQuery = query()
    .match(
      pattern('user').label('User').where({
        email: 'alice@example.com',
        active: true,
      })
    )
    .return(['user'])
    .build();

  console.log('Query:');
  console.log('  MATCH (user:User { email: "alice@example.com", active: true })');
  console.log('  RETURN user\n');

  const autoPlan = await compiler.compile(autoQuery, context);

  console.log('Automatically Selected Indexes:');
  const sortedHints = [...(autoPlan.metadata.indexHints || [])].sort(
    (a, b) => (b.confidence || 0) - (a.confidence || 0)
  );
  for (const hint of sortedHints) {
    const stars = '★'.repeat(Math.round((hint.confidence || 0) * 5));
    console.log(`  ${stars} ${hint.variable}.${hint.index}`);
    console.log(`    Confidence: ${((hint.confidence || 0) * 100).toFixed(0)}%`);
    console.log(`    Reason: ${hint.reason}`);
  }

  // ================================================================
  // 3. Strategy Analysis
  // ================================================================
  console.log('\n3️⃣  STRATEGY ANALYSIS\n');

  const selector = getIndexSelector();
  const complexQuery = query()
    .match(
      pattern('task').label('Task').where({
        id: 'task-123',
        status: 'open',
        assignee: 'alice',
      })
    )
    .build();

  const hints = selector.selectIndexes(complexQuery);

  console.log('Query Pattern:');
  console.log('  MATCH (task:Task { id: "task-123", status: "open", assignee: "alice" })\n');

  console.log('Strategy Recommendations:');
  const byStrategy = new Map<string, typeof hints>();

  for (const hint of hints) {
    // Infer strategy from confidence and reason
    let strategy = 'unknown';
    if (hint.reason?.includes('High cardinality')) strategy = 'cardinality';
    else if (hint.reason?.includes('Equality filter')) strategy = 'pattern';
    else if (hint.reason?.includes('Composite')) strategy = 'composite';
    else if (hint.reason?.includes('Historical')) strategy = 'historical';

    if (!byStrategy.has(strategy)) {
      byStrategy.set(strategy, []);
    }
    byStrategy.get(strategy)!.push(hint);
  }

  for (const [strategy, strategyHints] of byStrategy) {
    console.log(`\n  ${strategy.toUpperCase()}:`);
    for (const hint of strategyHints) {
      console.log(`    - ${hint.index} (${((hint.confidence || 0) * 100).toFixed(0)}% confidence)`);
    }
  }

  // ================================================================
  // 4. Index Effectiveness Tracking
  // ================================================================
  console.log('\n4️⃣  INDEX EFFECTIVENESS TRACKING\n');

  const trackedQuery = query()
    .match(pattern('task').label('Task').where({ status: 'open' }))
    .useIndex('task', 'status')
    .build();

  const trackedPlan = await compiler.compile(trackedQuery, context);

  console.log('Simulating 10 query executions...\n');

  for (let i = 1; i <= 10; i++) {
    const stats = simulateExecution(trackedPlan.id, true);
    cache.recordExecution(trackedPlan, stats);

    if (i % 3 === 0) {
      const queryStats = cache.getStatistics(trackedPlan);
      const statusEff = queryStats?.indexEffectiveness?.get('status');

      console.log(`After ${i} executions:`);
      console.log(`  Use Count: ${statusEff?.useCount || 0}`);
      console.log(`  Avg Improvement: ${((statusEff?.avgImprovement || 0) * 100).toFixed(1)}%`);
      console.log(`  Success Rate: ${((statusEff?.successRate || 0) * 100).toFixed(1)}%`);
      console.log(`  Avg Results: ${statusEff?.avgResultCount?.toFixed(0) || 0}`);
      console.log();
    }
  }

  // ================================================================
  // 5. Historical Learning
  // ================================================================
  console.log('\n5️⃣  HISTORICAL LEARNING\n');

  const allStats = cache.getAllStatistics();
  const withEffectiveness = allStats.filter((s) => s.indexEffectiveness && s.indexEffectiveness.size > 0);

  console.log(`Tracked ${allStats.length} query signatures`);
  console.log(`${withEffectiveness.length} have index effectiveness data\n`);

  if (withEffectiveness.length > 0) {
    const stats = withEffectiveness[0];
    console.log('Index Effectiveness Summary:');

    for (const [indexName, eff] of Array.from(stats.indexEffectiveness?.entries() || [])) {
      console.log(`\n  📊 ${indexName}:`);
      console.log(`     Uses: ${eff.useCount}`);
      console.log(`     Improvement: ${(eff.avgImprovement * 100).toFixed(1)}%`);
      console.log(`     Success Rate: ${(eff.successRate * 100).toFixed(1)}%`);

      // Would be recommended?
      const wouldRecommend =
        eff.useCount >= 3 && eff.avgImprovement > 0.1 && eff.successRate > 0.8;
      console.log(`     Recommended: ${wouldRecommend ? '✅ Yes' : '❌ No'}`);
    }
  }

  // ================================================================
  // 6. Cost Comparison
  // ================================================================
  console.log('\n6️⃣  COST COMPARISON\n');

  const queryWithoutHint = query()
    .match(pattern('task').label('Task').where({ id: 'task-123' }))
    .build();

  const queryWithHint = query()
    .match(pattern('task').label('Task').where({ id: 'task-123' }))
    .useIndex('task', 'id')
    .build();

  const planWithout = await compiler.compile(queryWithoutHint, context);
  const planWith = await compiler.compile(queryWithHint, context);

  const costWithout = planWithout.metadata.estimatedCost.makespan;
  const costWith = planWith.metadata.estimatedCost.makespan;
  const improvement = ((costWithout - costWith) / costWithout) * 100;

  console.log('Query: MATCH (task:Task { id: "task-123" })\n');
  console.log(`Without Index Hint: ${costWithout.toFixed(1)}ms`);
  console.log(`With Index Hint:    ${costWith.toFixed(1)}ms`);
  console.log(`Improvement:        ${improvement.toFixed(1)}%`);

  // ================================================================
  // Summary
  // ================================================================
  console.log('\n' + '='.repeat(60));
  console.log('\n✅ Demo Complete!\n');
  console.log('Key Takeaways:');
  console.log('  1. Manual hints provide explicit control');
  console.log('  2. Automatic selection covers common patterns');
  console.log('  3. Multiple strategies provide comprehensive coverage');
  console.log('  4. Effectiveness tracking enables learning');
  console.log('  5. Historical data improves future queries');
  console.log('  6. Cost estimates guide optimization\n');
}

// Run demo
demo().catch(console.error);
