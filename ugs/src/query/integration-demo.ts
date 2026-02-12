#!/usr/bin/env bun
/**
 * Query Layer Integration Demo
 *
 * Demonstrates query-driven workflow orchestration:
 * 1. Find ready tasks (no blockers)
 * 2. Auto-start them via QueryExecutor → WorkflowOrchestrator
 *
 * This shows how declarative queries enable automatic workflow execution
 * without manual task polling.
 */

import { QueryExecutor } from '../messaging/actors/query-executor.ts';
import { query, pattern, send } from './index.ts';
import { address } from '@agentic-primer/actors';
import type { MessageRouter } from '@agentic-primer/actors';
import type GraphStore from '../graph.ts';
import type { ProgramManager } from '../entities/program.ts';

/**
 * Demo: Query-driven workflow orchestration
 *
 * Scenario: Build pipeline with dependencies
 * - compile (no deps)
 * - link (requires compile)
 * - test (requires link)
 * - deploy (requires test)
 *
 * The query layer detects when tasks become ready and auto-starts them.
 */
export async function demoQueryDrivenWorkflow() {
  console.log('=== Query-Driven Workflow Demo ===\n');

  // Setup (would use real store in production)
  const mockStore = createMockStore();
  const mockProgramManager = {} as ProgramManager;
  const router = new MessageRouter(mockStore, mockProgramManager);

  // Create QueryExecutor
  const queryExecutor = new QueryExecutor('query-executor', router);

  // Define query: Find tasks with no open blockers
  const readyTasksQuery = query()
    .match(
      pattern('task')
        .label('Task')
        .where({ status: 'open' })
        .notExists(
          pattern('blocker')
            .label('Task')
            .where({ status: 'open' })
            .relatedTo('task', { type: 'requires', direction: 'inbound' })
        )
    )
    .forEach(send('task').tell('start'))
    .build();

  console.log('Query definition:');
  console.log('  MATCH (task:Task { status: "open" })');
  console.log('  WHERE NOT EXISTS (');
  console.log('    (blocker:Task { status: "open" })-[:requires]->(task)');
  console.log('  )');
  console.log('  DO send(task).tell("start")\n');

  // Execute query
  console.log('Executing query...\n');

  const executeMsg = {
    id: 'exec-1',
    pattern: 'ask' as const,
    type: 'execute',
    payload: { query: readyTasksQuery },
    from: address('domain/demo'),
    to: address('services/query-executor'),
    timestamp: Date.now(),
  };

  try {
    const response = await queryExecutor.receive(executeMsg);

    if (response.success) {
      console.log('✓ Query executed successfully');
      console.log(`  Plan ID: ${response.payload.plan.id}`);
      console.log(`  Cache hit: ${response.payload.plan.cacheHit}`);
      console.log(`  Steps executed: ${response.payload.result.stats.stepsExecuted}`);
      console.log(`  Duration: ${response.payload.result.stats.durationMs}ms`);
      console.log(`  Tasks found: ${response.payload.result.stats.resultsReturned}`);
    } else {
      console.log(`✗ Query failed: ${response.error}`);
    }
  } catch (error: any) {
    console.log(`✗ Error: ${error.message}`);
  }

  console.log('\n=== Demo Complete ===');
}

/**
 * Demo: Cached query execution
 *
 * Shows how query cache improves performance on repeated executions.
 */
export async function demoCachedQueryExecution() {
  console.log('\n=== Cached Query Execution Demo ===\n');

  const mockStore = createMockStore();
  const mockProgramManager = {} as ProgramManager;
  const router = new MessageRouter(mockStore, mockProgramManager);
  const queryExecutor = new QueryExecutor('query-executor', router);

  // Simple query
  const findOpenTasks = query()
    .match(pattern('task').label('Task').where({ status: 'open' }))
    .return(['task'])
    .build();

  const executeMsg = {
    id: 'exec-1',
    pattern: 'ask' as const,
    type: 'execute',
    payload: { query: findOpenTasks },
    from: address('domain/demo'),
    to: address('services/query-executor'),
    timestamp: Date.now(),
  };

  // First execution (cold)
  console.log('First execution (cold):');
  const response1 = await queryExecutor.receive(executeMsg);
  if (response1.success) {
    console.log(`  Cache hit: ${response1.payload.plan.cacheHit}`);
    console.log(`  Duration: ${response1.payload.result.stats.durationMs}ms`);
  }

  // Second execution (warm)
  console.log('\nSecond execution (warm):');
  executeMsg.id = 'exec-2';
  const response2 = await queryExecutor.receive(executeMsg);
  if (response2.success) {
    console.log(`  Cache hit: ${response2.payload.plan.cacheHit}`);
    console.log(`  Duration: ${response2.payload.result.stats.durationMs}ms`);
  }

  // Get cache stats
  console.log('\nCache statistics:');
  const statsMsg = {
    id: 'stats-1',
    pattern: 'ask' as const,
    type: 'get-cache-stats',
    payload: {},
    from: address('domain/demo'),
    to: address('services/query-executor'),
    timestamp: Date.now(),
  };

  const statsResponse = await queryExecutor.receive(statsMsg);
  if (statsResponse.success) {
    console.log(`  Size: ${statsResponse.payload.cache.size}`);
    console.log(`  Hit rate: ${statsResponse.payload.cache.hitRate.toFixed(2)}`);
    console.log(`  Avg access count: ${statsResponse.payload.cache.avgAccessCount.toFixed(1)}`);
  }

  console.log('\n=== Demo Complete ===');
}

/**
 * Demo: Complex workflow queries
 *
 * Shows advanced query patterns for workflow orchestration.
 */
export async function demoComplexWorkflowQueries() {
  console.log('\n=== Complex Workflow Queries Demo ===\n');

  // Query 1: Find high-priority ready tasks
  const highPriorityReady = query()
    .match(
      pattern('task')
        .label('Task')
        .where({ status: 'open', priority: 'high' })
        .notExists(
          pattern('blocker')
            .label('Task')
            .where({ status: 'open' })
            .relatedTo('task', { type: 'requires', direction: 'inbound' })
        )
    )
    .forEach(send('task').tell('start'))
    .build();

  console.log('Query 1: High-priority ready tasks');
  console.log('  Finds open high-priority tasks with no blockers');
  console.log('  Auto-starts them immediately\n');

  // Query 2: Find dependency chains
  const dependencyChain = query()
    .match(pattern('root').label('Task').where({ id: 'build' }))
    .traverse({
      from: 'root',
      relationship: 'requires',
      direction: 'outbound',
      depth: { max: 10 },
      as: 'dependencies',
    })
    .return(['root', 'dependencies'])
    .build();

  console.log('Query 2: Dependency chain traversal');
  console.log('  Starting from root task, traverse all dependencies');
  console.log('  Returns full dependency tree up to depth 10\n');

  // Query 3: Conditional workflow execution
  const conditionalExecution = query()
    .match(pattern('test').label('Task').where({ id: 'test' }))
    .when(
      pattern('test').where({
        lifecycle: 'completed',
        result: { passed: true },
      })
    )
    .then(send('deploy').tell('start'))
    .build();

  console.log('Query 3: Conditional execution');
  console.log('  If test task completed successfully');
  console.log('  Then start deploy task\n');

  console.log('=== Queries Defined (not executed) ===');
}

/**
 * Demo: WorkflowOrchestrator integration
 *
 * Shows how QueryExecutor integrates with WorkflowOrchestrator for
 * automatic workflow execution.
 */
export async function demoWorkflowIntegration() {
  console.log('\n=== WorkflowOrchestrator Integration Demo ===\n');

  console.log('Integration Pattern:');
  console.log('  1. WorkflowOrchestrator receives task completion event');
  console.log('  2. Sends query to QueryExecutor: "Find ready tasks"');
  console.log('  3. QueryExecutor returns ready task IDs');
  console.log('  4. WorkflowOrchestrator auto-starts them\n');

  console.log('Benefits:');
  console.log('  ✓ Declarative workflow definition (no manual polling)');
  console.log('  ✓ Query caching reduces overhead');
  console.log('  ✓ Complex dependency patterns (NOT EXISTS, etc.)');
  console.log('  ✓ Conditional execution (WHEN ... THEN)');
  console.log('  ✓ Adaptive optimization (learns from execution history)\n');

  console.log('Example: Build Pipeline');
  console.log('  Tasks: compile → link → test → deploy');
  console.log('  Query detects when each stage is ready');
  console.log('  Auto-starts next stage when dependencies satisfied\n');

  console.log('Workflow Definition:');
  console.log('  {');
  console.log('    tasks: [');
  console.log('      { id: "compile", dependsOn: [] },');
  console.log('      { id: "link", dependsOn: ["compile"] },');
  console.log('      { id: "test", dependsOn: ["link"] },');
  console.log('      { id: "deploy", dependsOn: ["test"] }');
  console.log('    ]');
  console.log('  }\n');

  console.log('Query Execution Flow:');
  console.log('  compile completes → query finds link ready → auto-start link');
  console.log('  link completes → query finds test ready → auto-start test');
  console.log('  test completes → query finds deploy ready → auto-start deploy\n');

  console.log('=== Integration Pattern Demonstrated ===');
}

// Helper function
function createMockStore(): GraphStore {
  const nodes = new Map();
  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
  } as any as GraphStore;
}

/**
 * Run all demos
 */
export async function runAllDemos() {
  try {
    await demoQueryDrivenWorkflow();
    await demoCachedQueryExecution();
    await demoComplexWorkflowQueries();
    await demoWorkflowIntegration();

    console.log('\n=== All Demos Complete ===');
  } catch (error: any) {
    console.error(`Demo error: ${error.message}`);
    console.error(error.stack);
  }
}

// Run if executed directly
if (import.meta.main) {
  await runAllDemos();
}
