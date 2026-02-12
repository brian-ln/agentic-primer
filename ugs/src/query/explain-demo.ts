#!/usr/bin/env bun
/**
 * EXPLAIN Demo
 *
 * Quick demonstration of EXPLAIN functionality with real-world queries.
 * Shows how to use EXPLAIN for performance analysis and optimization.
 */

import { query, pattern, send, filter } from './index.ts';
import { logic } from './pattern.ts';
import type { ExecutionContext } from './types.ts';
import { address } from '@agentic-primer/actors';

console.log('╔═══════════════════════════════════════════════════════════════╗');
console.log('║           Query EXPLAIN Demonstration                        ║');
console.log('╚═══════════════════════════════════════════════════════════════╝\n');

// Example 1: Simple query
console.log('┌─────────────────────────────────────────────────────────────┐');
console.log('│ Example 1: Simple Query - Find Open Tasks                  │');
console.log('└─────────────────────────────────────────────────────────────┘\n');

const simple = await query()
  .match(pattern('task').label('Task').where({ status: 'open' }))
  .return(['task'])
  .explain();

console.log(simple.text);
console.log('\n');

// Example 2: Query with traversal
console.log('┌─────────────────────────────────────────────────────────────┐');
console.log('│ Example 2: Dependency Tree Traversal                       │');
console.log('└─────────────────────────────────────────────────────────────┘\n');

const traversal = await query()
  .match(pattern('root').label('Task').where({ id: 'build' }))
  .traverse({
    from: 'root',
    relationship: 'requires',
    direction: 'outbound',
    depth: { max: 5 },
    as: 'dependencies',
  })
  .return(['root', 'dependencies'])
  .explain();

console.log(traversal.tree);
console.log('\n📊 Cost Analysis:');
console.log(`   Total Latency: ${traversal.costBreakdown.totalLatency.toFixed(2)}ms`);
console.log(`   Critical Path: ${traversal.costBreakdown.criticalPath.toFixed(2)}ms`);
console.log(
  `   Cache Hit Rate: ${(traversal.cacheAnalysis.overallHitProb * 100).toFixed(1)}%\n`
);

// Example 3: Complex workflow
console.log('┌─────────────────────────────────────────────────────────────┐');
console.log('│ Example 3: Workflow Query with Actions                     │');
console.log('└─────────────────────────────────────────────────────────────┘\n');

const workflow = await query()
  .match(
    pattern('test').label('Task').where({ id: 'test' }),
    pattern('deploy').label('Task').where({ id: 'deploy' })
  )
  .when(
    pattern('test').where({
      lifecycle: 'completed',
      result: { passed: true },
    })
  )
  .then(send('deploy').tell('start'))
  .explain({ optimize: true });

console.log('🔍 Optimization Notes:');
for (const note of workflow.optimizations) {
  const icon = note.type === 'warning' ? '⚠️' : note.type === 'tip' ? '💡' : 'ℹ️';
  console.log(`   ${icon} ${note.message}`);
}
console.log('\n');

// Example 4: Comparison - Cold vs Warm
console.log('┌─────────────────────────────────────────────────────────────┐');
console.log('│ Example 4: Performance Impact of Warm Actors               │');
console.log('└─────────────────────────────────────────────────────────────┘\n');

const coldQuery = query()
  .match(pattern('task').label('Task'))
  .traverse({
    from: 'task',
    relationship: 'requires',
    direction: 'outbound',
    as: 'deps',
  })
  .return(['task', 'deps']);

const coldResult = await coldQuery.explain();

const warmContext: ExecutionContext = {
  warmActors: new Set([address('domain/tasks'), address('domain/relationships')]),
  computationCache: new Map(),
  resources: {
    maxConcurrency: 4,
    availableMemory: 1024 * 1024 * 100,
  },
  startTime: Date.now(),
};

const warmResult = await coldQuery.explain({}, warmContext);

console.log('❄️  Cold Execution (first run):');
console.log(`   Latency: ${coldResult.costBreakdown.totalLatency.toFixed(2)}ms`);
console.log(
  `   Cache Hit Rate: ${(coldResult.cacheAnalysis.overallHitProb * 100).toFixed(1)}%`
);
console.log('\n🔥 Warm Execution (actors initialized):');
console.log(`   Latency: ${warmResult.costBreakdown.totalLatency.toFixed(2)}ms`);
console.log(
  `   Cache Hit Rate: ${(warmResult.cacheAnalysis.overallHitProb * 100).toFixed(1)}%`
);

const improvement =
  ((coldResult.costBreakdown.totalLatency -
    warmResult.costBreakdown.totalLatency) /
    coldResult.costBreakdown.totalLatency) *
  100;
console.log(`\n✨ Improvement: ${improvement.toFixed(1)}% faster with warm actors\n`);

// Example 5: Parallel execution
console.log('┌─────────────────────────────────────────────────────────────┐');
console.log('│ Example 5: Parallel Execution Analysis                     │');
console.log('└─────────────────────────────────────────────────────────────┘\n');

const parallel = await query()
  .match(
    pattern('task1').label('Task').where({ id: '1' }),
    pattern('task2').label('Task').where({ id: '2' }),
    pattern('task3').label('Task').where({ id: '3' })
  )
  .return(['task1', 'task2', 'task3'])
  .explain();

console.log(`📦 Query has ${parallel.plan.steps.length} steps`);
console.log(`⚡ Parallelizable: ${parallel.plan.metadata.parallelizable ? 'Yes' : 'No'}`);
console.log(`🔗 Critical Path: ${parallel.plan.metadata.criticalPathSteps} steps`);
console.log(`⏱️  Sequential Time: ${parallel.costBreakdown.totalLatency.toFixed(2)}ms`);
console.log(
  `⚡ Parallel Time: ${parallel.costBreakdown.criticalPath.toFixed(2)}ms`
);
console.log(
  `💾 Savings: ${parallel.costBreakdown.parallelismBenefit.toFixed(2)}ms (${((parallel.costBreakdown.parallelismBenefit / parallel.costBreakdown.totalLatency) * 100).toFixed(1)}%)\n`
);

// Summary
console.log('┌─────────────────────────────────────────────────────────────┐');
console.log('│ Summary                                                     │');
console.log('└─────────────────────────────────────────────────────────────┘\n');

console.log('✅ EXPLAIN provides:');
console.log('   • Detailed execution plans with cost estimates');
console.log('   • Visual dependency trees (ASCII art)');
console.log('   • Cache hit predictions');
console.log('   • Parallelism analysis');
console.log('   • Optimization recommendations');
console.log('   • Performance comparison (cold vs warm)');
console.log('\n💡 Use EXPLAIN to:');
console.log('   • Identify query bottlenecks before execution');
console.log('   • Optimize query structure');
console.log('   • Understand cache behavior');
console.log('   • Plan actor warm-up strategies');
console.log('   • Debug complex workflows');
console.log('\n📚 See docs/EXPLAIN.md for complete documentation\n');

console.log('╔═══════════════════════════════════════════════════════════════╗');
console.log('║                    Demo Complete                             ║');
console.log('╚═══════════════════════════════════════════════════════════════╝\n');
