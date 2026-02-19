#!/usr/bin/env bun
/**
 * Example: Subscribe (S1) - Reactive Queries
 *
 * Demonstrates live queries that automatically update when patterns match/unmatch.
 */

import GraphStore from '@src/graph.ts';
import { ProgramManager } from '@src/entities/program.ts';
import { MessageRouter } from '@src/messaging/router.ts';
import { QueryExecutor } from '@src/messaging/actors/query-executor.ts';
import { query } from '../builder.ts';
import { pattern } from '../pattern.ts';

async function main() {
  // Setup
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);
  const executor = new QueryExecutor('query-executor', router);

  console.log('=== Subscribe (S1) - Reactive Queries Example ===\n');

  // Example 1: Monitor failed tasks
  console.log('Example 1: Monitor failed tasks');
  console.log('--------------------------------');

  const failedTaskSub = await executor.subscribe(
    query()
      .match(pattern('task').where({ status: 'failed' }))
      .build(),
    {
      onMatch: (tasks) => {
        console.log(`⚠️  Failed tasks detected: ${tasks.length} tasks`);
        tasks.forEach((task: any) => {
          console.log(`   - ${task.id || 'unknown'}: ${task.title || 'no title'}`);
        });
      },
      onUnmatch: (tasks) => {
        console.log(`✅ Tasks recovered: ${tasks.length} tasks`);
        tasks.forEach((task: any) => {
          console.log(`   - ${task.id || 'unknown'} no longer failed`);
        });
      },
      onError: (error) => {
        console.error(`❌ Subscription error: ${error.message}`);
      },
    }
  );

  console.log('✓ Subscription active\n');

  // Example 2: Monitor high-priority tasks
  console.log('Example 2: Monitor high-priority tasks');
  console.log('---------------------------------------');

  const highPrioritySub = await executor.subscribe(
    query()
      .match(pattern('task').where({ priority: 'high', status: 'open' }))
      .build(),
    {
      onMatch: (tasks) => {
        console.log(`🔥 High-priority open tasks: ${tasks.length}`);
      },
      onUnmatch: (tasks) => {
        console.log(`✓ ${tasks.length} high-priority tasks completed or deprioritized`);
      },
    }
  );

  console.log('✓ Subscription active\n');

  // Example 3: Monitor task dependencies
  console.log('Example 3: Monitor task dependencies (with traversal)');
  console.log('-----------------------------------------------------');

  const dependencySub = await executor.subscribe(
    query()
      .match(pattern('root').where({ id: 'critical-task' }))
      .traverse({
        from: 'root',
        relationship: 'requires',
        direction: 'outbound',
        depth: { max: 3 },
        as: 'dependencies',
      })
      .build(),
    {
      onMatch: (deps) => {
        console.log(`📊 Critical task has ${deps.length} dependencies`);
      },
      onUnmatch: (deps) => {
        console.log(`✓ ${deps.length} dependencies resolved`);
      },
    }
  );

  console.log('✓ Subscription active\n');

  // Example 4: Complex pattern with multiple matches
  console.log('Example 4: Complex pattern (tasks + assignees)');
  console.log('-----------------------------------------------');

  const complexSub = await executor.subscribe(
    query()
      .match(
        pattern('task').where({ status: 'open', priority: 'high' }),
        pattern('user').where({ role: 'admin' })
      )
      .build(),
    {
      onMatch: (results) => {
        console.log(`🎯 Found ${results.length} matches for complex pattern`);
      },
    }
  );

  console.log('✓ Subscription active\n');

  // Show statistics
  console.log('Subscription Statistics:');
  console.log('------------------------');
  const stats = executor.getSubscriptionStats();
  console.log(`Active subscriptions: ${stats.activeSubscriptions}`);
  console.log(`Total subscriptions: ${stats.totalSubscriptions}\n`);

  // Simulate some time passing
  console.log('Subscriptions are now listening for changes...');
  console.log('(In production, these would react to actor port events)\n');

  await new Promise((resolve) => setTimeout(resolve, 100));

  // Cleanup
  console.log('Cleaning up subscriptions...');
  failedTaskSub.unsubscribe();
  highPrioritySub.unsubscribe();
  dependencySub.unsubscribe();
  complexSub.unsubscribe();

  const statsAfter = executor.getSubscriptionStats();
  console.log(`Active subscriptions after cleanup: ${statsAfter.activeSubscriptions}\n`);

  // Example 5: Subscription lifecycle
  console.log('Example 5: Subscription lifecycle management');
  console.log('--------------------------------------------');

  const lifecycleSub = await executor.subscribe(
    query()
      .match(pattern('task').where({ status: 'processing' }))
      .build(),
    {
      onMatch: (tasks) => {
        console.log(`⚙️  ${tasks.length} tasks currently processing`);
      },
    }
  );

  console.log(`Is active: ${lifecycleSub.isActive()}`);

  lifecycleSub.unsubscribe();
  console.log(`After unsubscribe - Is active: ${lifecycleSub.isActive()}\n`);

  // Example 6: Error handling
  console.log('Example 6: Error handling in subscriptions');
  console.log('------------------------------------------');

  let errorHandled = false;

  const errorSub = await executor.subscribe(
    query()
      .match(pattern('task').where({ status: 'open' }))
      .build(),
    {
      onMatch: (tasks) => {
        // Simulate a callback error
        if (tasks.length === 0) {
          // This won't throw in our stub, but demonstrates error handling
          console.log('onMatch: No tasks (no error)');
        }
      },
      onError: (error) => {
        errorHandled = true;
        console.log(`Error handler called: ${error.message}`);
      },
    }
  );

  await new Promise((resolve) => setTimeout(resolve, 50));

  console.log(`Error handling available: ${errorHandled ? 'Yes' : 'Ready'}\n`);

  errorSub.unsubscribe();

  console.log('=== Examples Complete ===');
  console.log('\nKey Features Demonstrated:');
  console.log('✓ Live query subscriptions');
  console.log('✓ onMatch and onUnmatch callbacks');
  console.log('✓ Error handling with onError');
  console.log('✓ Complex patterns and traversals');
  console.log('✓ Subscription lifecycle management');
  console.log('✓ Multiple concurrent subscriptions');
  console.log('✓ Statistics and monitoring');
}

// Run examples
main().catch(console.error);
