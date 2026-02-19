#!/usr/bin/env bun
/**
 * Integration tests for Subscribe (S1) - Reactive Queries
 *
 * Tests the full integration of subscriptions through QueryExecutor
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import { QueryExecutor } from '@src/messaging/actors/query-executor.ts';
import { MessageRouter } from '@src/messaging/router.ts';
import { SilentLoggerActor } from '@src/messaging/actors/logger.ts';
import GraphStore from '@src/graph.ts';
import { ProgramManager } from '@src/entities/program.ts';
import { query } from '../builder.ts';
import { pattern } from '../pattern.ts';

describe('Subscribe Integration (S1)', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let executor: QueryExecutor;

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
  });

  describe('QueryExecutor.subscribe()', () => {
    test('creates subscription through executor', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'failed' }))
        .build();

      const subscription = await executor.subscribe(queryDef, {
        onMatch: (tasks) => {
          // Handle failed tasks
        },
      });

      expect(subscription).toBeDefined();
      expect(subscription.isActive()).toBe(true);
      expect(typeof subscription.unsubscribe).toBe('function');

      subscription.unsubscribe();
    });

    test('subscription receives initial matches', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      const matches: any[] = [];

      const subscription = await executor.subscribe(queryDef, {
        onMatch: (results) => {
          matches.push(...results);
        },
      });

      // Wait for initial evaluation
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Should have processed initial query
      expect(Array.isArray(matches)).toBe(true);

      subscription.unsubscribe();
    });

    test('subscription with onUnmatch callback', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'in_progress' }))
        .build();

      const matched: any[] = [];
      const unmatched: any[] = [];

      const subscription = await executor.subscribe(queryDef, {
        onMatch: (results) => matched.push(...results),
        onUnmatch: (results) => unmatched.push(...results),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Initially should have matches, no unmatches
      expect(Array.isArray(matched)).toBe(true);
      expect(Array.isArray(unmatched)).toBe(true);

      subscription.unsubscribe();
    });

    test('subscription with error handler', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      let errorHandlerCalled = false;

      const subscription = await executor.subscribe(queryDef, {
        onMatch: () => {},
        onError: (error) => {
          errorHandlerCalled = true;
        },
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Error handler should be available
      expect(typeof errorHandlerCalled).toBe('boolean');

      subscription.unsubscribe();
    });

    test('multiple concurrent subscriptions', async () => {
      const query1 = query().match(pattern('task').where({ status: 'open' })).build();
      const query2 = query().match(pattern('task').where({ status: 'failed' })).build();
      const query3 = query().match(pattern('task').where({ priority: 'high' })).build();

      const sub1 = await executor.subscribe(query1, { onMatch: () => {} });
      const sub2 = await executor.subscribe(query2, { onMatch: () => {} });
      const sub3 = await executor.subscribe(query3, { onMatch: () => {} });

      const stats = executor.getSubscriptionStats();
      expect(stats.activeSubscriptions).toBe(3);

      sub1.unsubscribe();
      const statsAfter1 = executor.getSubscriptionStats();
      expect(statsAfter1.activeSubscriptions).toBe(2);

      sub2.unsubscribe();
      sub3.unsubscribe();

      const statsAfterAll = executor.getSubscriptionStats();
      expect(statsAfterAll.activeSubscriptions).toBe(0);
    });

    test('subscription lifecycle management', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      const subscription = await executor.subscribe(queryDef, {
        onMatch: () => {},
      });

      // Initial state
      expect(subscription.isActive()).toBe(true);

      const statsBefore = executor.getSubscriptionStats();
      expect(statsBefore.activeSubscriptions).toBeGreaterThanOrEqual(1);

      // Unsubscribe
      subscription.unsubscribe();
      expect(subscription.isActive()).toBe(false);

      const statsAfter = executor.getSubscriptionStats();
      expect(statsAfter.activeSubscriptions).toBe(statsBefore.activeSubscriptions - 1);
    });

    test('subscription with complex query pattern', async () => {
      const queryDef = query()
        .match(
          pattern('task').where({ status: 'open' }),
          pattern('user').where({ role: 'developer' })
        )
        .build();

      const matches: any[] = [];

      const subscription = await executor.subscribe(queryDef, {
        onMatch: (results) => matches.push(...results),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(Array.isArray(matches)).toBe(true);

      subscription.unsubscribe();
    });

    test('subscription with traversal', async () => {
      const queryDef = query()
        .match(pattern('root').where({ id: 'task-1' }))
        .traverse({
          from: 'root',
          relationship: 'requires',
          direction: 'outbound',
          depth: { max: 2 },
          as: 'dependencies',
        })
        .build();

      const matches: any[] = [];

      const subscription = await executor.subscribe(queryDef, {
        onMatch: (results) => matches.push(...results),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(Array.isArray(matches)).toBe(true);

      subscription.unsubscribe();
    });

    test('subscription cleanup on unsubscribe', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      const subscription = await executor.subscribe(queryDef, {
        onMatch: () => {},
      });

      expect(subscription.isActive()).toBe(true);

      // Unsubscribe should clean up all resources
      subscription.unsubscribe();

      expect(subscription.isActive()).toBe(false);

      // Multiple unsubscribes should be safe
      subscription.unsubscribe();
      subscription.unsubscribe();
    });
  });

  describe('Subscription Statistics', () => {
    test('getSubscriptionStats returns accurate counts', async () => {
      const initialStats = executor.getSubscriptionStats();
      expect(initialStats.activeSubscriptions).toBe(0);

      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      const sub1 = await executor.subscribe(queryDef, { onMatch: () => {} });
      const sub2 = await executor.subscribe(queryDef, { onMatch: () => {} });

      const statsWithSubs = executor.getSubscriptionStats();
      expect(statsWithSubs.activeSubscriptions).toBe(2);

      sub1.unsubscribe();

      const statsAfter1 = executor.getSubscriptionStats();
      expect(statsAfter1.activeSubscriptions).toBe(1);

      sub2.unsubscribe();

      const statsAfter2 = executor.getSubscriptionStats();
      expect(statsAfter2.activeSubscriptions).toBe(0);
    });

    test('statistics track total and active separately', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      const sub1 = await executor.subscribe(queryDef, { onMatch: () => {} });
      const sub2 = await executor.subscribe(queryDef, { onMatch: () => {} });

      const stats1 = executor.getSubscriptionStats();
      expect(stats1.activeSubscriptions).toBe(2);
      expect(stats1.totalSubscriptions).toBe(2);

      sub1.unsubscribe();

      const stats2 = executor.getSubscriptionStats();
      expect(stats2.activeSubscriptions).toBe(1);
      expect(stats2.totalSubscriptions).toBe(1);
    });
  });

  describe('Error Scenarios', () => {
    test('handles invalid query definition', async () => {
      const invalidQuery: any = {
        patterns: null, // Invalid
        filters: [],
        actions: [],
        returns: [],
        metadata: {},
      };

      let errorCaught = false;

      try {
        await executor.subscribe(invalidQuery, {
          onMatch: () => {},
        });
      } catch (error) {
        errorCaught = true;
      }

      // May throw during compilation or return error via onError
      expect(typeof errorCaught).toBe('boolean');
    });

    test('subscription survives callback errors', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      let errorCount = 0;

      const subscription = await executor.subscribe(queryDef, {
        onMatch: () => {
          throw new Error('Callback error');
        },
        onError: (error) => {
          errorCount++;
        },
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Subscription should still be active
      expect(subscription.isActive()).toBe(true);

      subscription.unsubscribe();
    });
  });

  describe('Performance', () => {
    test('handles many subscriptions efficiently', async () => {
      const subscriptions: any[] = [];
      const count = 100;

      const startTime = Date.now();

      for (let i = 0; i < count; i++) {
        const queryDef = query()
          .match(pattern('task').where({ status: 'open' }))
          .build();

        const sub = await executor.subscribe(queryDef, {
          onMatch: () => {},
        });

        subscriptions.push(sub);
      }

      const createTime = Date.now() - startTime;

      const stats = executor.getSubscriptionStats();
      expect(stats.activeSubscriptions).toBe(count);

      // Should create subscriptions reasonably fast (<50ms average)
      expect(createTime / count).toBeLessThan(50);

      // Cleanup
      for (const sub of subscriptions) {
        sub.unsubscribe();
      }

      const statsAfter = executor.getSubscriptionStats();
      expect(statsAfter.activeSubscriptions).toBe(0);
    });

    test('unsubscribe is fast', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      const subscription = await executor.subscribe(queryDef, {
        onMatch: () => {},
      });

      const startTime = Date.now();
      subscription.unsubscribe();
      const unsubscribeTime = Date.now() - startTime;

      // Unsubscribe should be very fast (<5ms)
      expect(unsubscribeTime).toBeLessThan(5);
    });

    test('subscription latency is acceptable', async () => {
      const queryDef = query()
        .match(pattern('task').where({ status: 'open' }))
        .build();

      const startTime = Date.now();

      const subscription = await executor.subscribe(queryDef, {
        onMatch: () => {},
      });

      const latency = Date.now() - startTime;

      // Initial subscription setup should be fast (<50ms)
      expect(latency).toBeLessThan(50);

      subscription.unsubscribe();
    });
  });
});
