#!/usr/bin/env bun
/**
 * Event Trigger Tests (S2)
 * Comprehensive tests for declarative event-driven workflows
 * Target: 40+ test cases covering all trigger scenarios
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import { EventTriggerManager, type EventPayload, type TriggerSpec } from './trigger.ts';
import { query, send, pattern } from '../index.ts';
import { MessageRouter } from '@src/messaging/router.ts';
import GraphStore from '@src/graph.ts';
import { ProgramManager } from '@src/entities/program.ts';
import { address, type Message } from '@agentic-primer/actors';

// Mock router for testing
class MockRouter extends MessageRouter {
  public sentMessages: Message[] = [];

  async tell(message: Message): Promise<void> {
    this.sentMessages.push(message);
  }

  async ask<T = any>(message: Message): Promise<any> {
    this.sentMessages.push(message);
    return { success: true, payload: {} };
  }

  clearMessages(): void {
    this.sentMessages = [];
  }
}

// Create mock GraphStore
function createMockGraphStore(): GraphStore {
  const nodes = new Map();
  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
    has: (id: string) => nodes.has(id),
    delete: (id: string) => nodes.delete(id),
    clear: () => nodes.clear(),
  } as any as GraphStore;
}

// Create mock ProgramManager
function createMockProgramManager(): ProgramManager {
  return {
    invokeProgram: async (id: string, params: any) => ({
      success: true,
      output: `Program ${id} executed`,
    }),
  } as any as ProgramManager;
}

describe('Event Trigger System', () => {
  let manager: EventTriggerManager;
  let router: MockRouter;

  beforeEach(() => {
    const graph = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MockRouter(graph, programManager);
    manager = new EventTriggerManager(router);
  });

  afterEach(() => {
    manager.destroy();
  });

  describe('Basic Trigger Registration', () => {
    test('registers trigger with single event type', () => {
      const spec: TriggerSpec = {
        id: 'trigger1',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      };

      const id = manager.register(spec);
      expect(id).toBe('trigger1');
      expect(manager.listTriggers()).toContain('trigger1');
    });

    test('registers trigger with multiple event types', () => {
      const spec: TriggerSpec = {
        id: 'trigger2',
        eventTypes: ['task.created', 'task.updated', 'task.deleted'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      };

      const id = manager.register(spec);
      expect(id).toBe('trigger2');
    });

    test('auto-generates trigger ID if not provided', () => {
      const spec: TriggerSpec = {
        id: '',
        eventTypes: ['task.created'],
        actions: [],
      };

      const id = manager.register(spec);
      expect(id).toMatch(/^trigger_\d+$/);
    });

    test('registers multiple triggers', () => {
      manager.register({
        id: 'trigger1',
        eventTypes: ['task.created'],
        actions: [],
      });

      manager.register({
        id: 'trigger2',
        eventTypes: ['task.updated'],
        actions: [],
      });

      const triggers = manager.listTriggers();
      expect(triggers).toHaveLength(2);
      expect(triggers).toContain('trigger1');
      expect(triggers).toContain('trigger2');
    });
  });

  describe('Event Emission and Handling', () => {
    test('executes action when event matches', async () => {
      manager.register({
        id: 'test-trigger',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: { message: 'Task created' } },
          },
        ],
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1', status: 'open' },
        timestamp: Date.now(),
      });

      // Allow async execution
      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
      expect(router.sentMessages[0].type).toBe('log');
    });

    test('executes multiple actions for single event', async () => {
      manager.register({
        id: 'multi-action',
        eventTypes: ['task.completed'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
          {
            type: 'send',
            target: 'metrics',
            params: { pattern: 'tell', type: 'record', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.completed',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(2);
      expect(router.sentMessages[0].type).toBe('log');
      expect(router.sentMessages[1].type).toBe('record');
    });

    test('triggers fire for multiple event types', async () => {
      manager.register({
        id: 'multi-event',
        eventTypes: ['task.created', 'task.updated'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(2);
    });

    test('does not execute for non-matching event types', async () => {
      manager.register({
        id: 'specific-trigger',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(0);
    });
  });

  describe('Pattern Filtering', () => {
    test('filters events by pattern properties', async () => {
      manager.register({
        id: 'pattern-filter',
        eventTypes: ['task.created'],
        pattern: {
          variable: 'task',
          where: { status: 'open', priority: 'high' },
        },
        actions: [
          {
            type: 'send',
            target: 'alerts',
            params: { pattern: 'tell', type: 'notify', payload: {} },
          },
        ],
      });

      // Should match
      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1', status: 'open', priority: 'high' },
        timestamp: Date.now(),
      });

      // Should not match (wrong priority)
      await manager.emit({
        type: 'task.created',
        data: { id: 'task-2', status: 'open', priority: 'low' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });

    test('filters events by nested properties', async () => {
      manager.register({
        id: 'nested-filter',
        eventTypes: ['task.completed'],
        pattern: {
          variable: 'task',
          where: { result: { passed: true } },
        },
        actions: [
          {
            type: 'send',
            target: 'deploy',
            params: { pattern: 'tell', type: 'start', payload: {} },
          },
        ],
      });

      // Should match
      await manager.emit({
        type: 'task.completed',
        data: { id: 'task-1', result: { passed: true } },
        timestamp: Date.now(),
      });

      // Should not match
      await manager.emit({
        type: 'task.completed',
        data: { id: 'task-2', result: { passed: false } },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });

    test('filters events by labels', async () => {
      manager.register({
        id: 'label-filter',
        eventTypes: ['actor.created'],
        pattern: {
          variable: 'actor',
          labels: ['Task'],
        },
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      // Should match
      await manager.emit({
        type: 'actor.created',
        data: { id: 'task-1', labels: ['Task', 'Work'] },
        timestamp: Date.now(),
      });

      // Should not match
      await manager.emit({
        type: 'actor.created',
        data: { id: 'user-1', labels: ['User'] },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });
  });

  describe('Additional Filters', () => {
    test('applies comparison filters', async () => {
      manager.register({
        id: 'comparison-filter',
        eventTypes: ['task.updated'],
        filters: [
          {
            type: 'comparison',
            property: 'priority',
            operator: '>',
            value: 5,
          },
        ],
        actions: [
          {
            type: 'send',
            target: 'alerts',
            params: { pattern: 'tell', type: 'notify', payload: {} },
          },
        ],
      });

      // Should match (priority > 5)
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1', priority: 8 },
        timestamp: Date.now(),
      });

      // Should not match (priority <= 5)
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-2', priority: 3 },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });

    test('applies logical AND filters', async () => {
      manager.register({
        id: 'and-filter',
        eventTypes: ['task.created'],
        filters: [
          {
            type: 'logical',
            operator: 'AND',
            expressions: [
              { type: 'comparison', property: 'priority', operator: '>', value: 5 },
              { type: 'comparison', property: 'status', operator: '=', value: 'open' },
            ],
          },
        ],
        actions: [
          {
            type: 'send',
            target: 'alerts',
            params: { pattern: 'tell', type: 'notify', payload: {} },
          },
        ],
      });

      // Should match (both conditions true)
      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1', priority: 8, status: 'open' },
        timestamp: Date.now(),
      });

      // Should not match (one condition false)
      await manager.emit({
        type: 'task.created',
        data: { id: 'task-2', priority: 8, status: 'closed' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });

    test('applies logical OR filters', async () => {
      manager.register({
        id: 'or-filter',
        eventTypes: ['task.updated'],
        filters: [
          {
            type: 'logical',
            operator: 'OR',
            expressions: [
              { type: 'comparison', property: 'status', operator: '=', value: 'failed' },
              { type: 'comparison', property: 'priority', operator: '>', value: 9 },
            ],
          },
        ],
        actions: [
          {
            type: 'send',
            target: 'alerts',
            params: { pattern: 'tell', type: 'notify', payload: {} },
          },
        ],
      });

      // Should match (first condition true)
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1', status: 'failed', priority: 5 },
        timestamp: Date.now(),
      });

      // Should match (second condition true)
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-2', status: 'open', priority: 10 },
        timestamp: Date.now(),
      });

      // Should not match (both false)
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-3', status: 'open', priority: 5 },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(2);
    });
  });

  describe('Debouncing', () => {
    test('debounces rapid events', async () => {
      manager.register({
        id: 'debounced',
        eventTypes: ['task.updated'],
        debounce: 100,
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      // Emit multiple events rapidly
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      // Wait for debounce
      await new Promise((resolve) => setTimeout(resolve, 150));

      // Should only execute once (debounced)
      expect(router.sentMessages).toHaveLength(1);
    });

    test('debounce resets on new events', async () => {
      manager.register({
        id: 'debounce-reset',
        eventTypes: ['task.updated'],
        debounce: 100,
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      // First event
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      // Wait 50ms (less than debounce)
      await new Promise((resolve) => setTimeout(resolve, 50));

      // Second event (resets debounce)
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      // Wait for debounce
      await new Promise((resolve) => setTimeout(resolve, 120));

      // Should execute once after final debounce
      expect(router.sentMessages).toHaveLength(1);
    });
  });

  describe('Throttling', () => {
    test('throttles high-frequency events', async () => {
      manager.register({
        id: 'throttled',
        eventTypes: ['task.updated'],
        throttle: 100,
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      // Emit events rapidly
      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 20));

      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 20));

      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 100));

      // First event fires immediately, subsequent events throttled
      expect(router.sentMessages.length).toBeGreaterThanOrEqual(1);
      expect(router.sentMessages.length).toBeLessThanOrEqual(2);
    });
  });

  describe('Error Handling', () => {
    test('handles action errors gracefully', async () => {
      const failingRouter = {
        tell: async () => {
          throw new Error('Router failure');
        },
        ask: async () => {
          throw new Error('Router failure');
        },
      } as any;

      const errorManager = new EventTriggerManager(failingRouter);

      errorManager.register({
        id: 'error-trigger',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      // Should not throw
      await expect(
        errorManager.emit({
          type: 'task.created',
          data: { id: 'task-1' },
          timestamp: Date.now(),
        })
      ).resolves.toBeUndefined();

      errorManager.destroy();
    });

    test('tracks error count', async () => {
      const failingRouter = {
        tell: async () => {
          throw new Error('Router failure');
        },
        ask: async () => {
          throw new Error('Router failure');
        },
      } as any;

      const errorManager = new EventTriggerManager(failingRouter);

      errorManager.register({
        id: 'error-count',
        eventTypes: ['task.created'],
        maxRetries: 0,
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      await errorManager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      const stats = errorManager.getStats('error-count');
      expect(stats?.errorCount).toBeGreaterThan(0);

      errorManager.destroy();
    });

    test('retries failed actions', async () => {
      let attempts = 0;
      const retryRouter = {
        tell: async () => {
          attempts++;
          if (attempts < 3) {
            throw new Error('Temporary failure');
          }
        },
        ask: async () => {},
      } as any;

      const retryManager = new EventTriggerManager(retryRouter);

      retryManager.register({
        id: 'retry-trigger',
        eventTypes: ['task.created'],
        maxRetries: 3,
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      await retryManager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 500));

      expect(attempts).toBe(3);

      retryManager.destroy();
    });
  });

  describe('Trigger Management', () => {
    test('unregisters trigger successfully', () => {
      manager.register({
        id: 'temp-trigger',
        eventTypes: ['task.created'],
        actions: [],
      });

      expect(manager.listTriggers()).toContain('temp-trigger');

      const result = manager.unregister('temp-trigger');
      expect(result).toBe(true);
      expect(manager.listTriggers()).not.toContain('temp-trigger');
    });

    test('unregister returns false for non-existent trigger', () => {
      const result = manager.unregister('non-existent');
      expect(result).toBe(false);
    });

    test('clears pending debounce on unregister', async () => {
      manager.register({
        id: 'debounced-unregister',
        eventTypes: ['task.updated'],
        debounce: 100,
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.updated',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      // Unregister before debounce completes
      manager.unregister('debounced-unregister');

      // Wait past debounce time
      await new Promise((resolve) => setTimeout(resolve, 150));

      // Action should not have executed
      expect(router.sentMessages).toHaveLength(0);
    });

    test('lists all registered triggers', () => {
      manager.register({ id: 'trigger1', eventTypes: ['task.created'], actions: [] });
      manager.register({ id: 'trigger2', eventTypes: ['task.updated'], actions: [] });
      manager.register({ id: 'trigger3', eventTypes: ['task.deleted'], actions: [] });

      const triggers = manager.listTriggers();
      expect(triggers).toHaveLength(3);
      expect(triggers).toContain('trigger1');
      expect(triggers).toContain('trigger2');
      expect(triggers).toContain('trigger3');
    });

    test('clears all triggers', () => {
      manager.register({ id: 'trigger1', eventTypes: ['task.created'], actions: [] });
      manager.register({ id: 'trigger2', eventTypes: ['task.updated'], actions: [] });

      manager.clear();

      expect(manager.listTriggers()).toHaveLength(0);
    });
  });

  describe('Statistics', () => {
    test('tracks event count', async () => {
      manager.register({
        id: 'stats-trigger',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-2' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      const stats = manager.getStats('stats-trigger');
      expect(stats?.eventCount).toBe(2);
    });

    test('tracks last fired timestamp', async () => {
      manager.register({
        id: 'timestamp-trigger',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      const beforeEmit = Date.now();

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      const stats = manager.getStats('timestamp-trigger');
      expect(stats?.lastFired).toBeGreaterThanOrEqual(beforeEmit);
    });

    test('returns null for non-existent trigger stats', () => {
      const stats = manager.getStats('non-existent');
      expect(stats).toBeNull();
    });
  });

  describe('Query Builder Integration', () => {
    test('creates trigger spec from query builder', () => {
      const q = query()
        .on('task.lifecycle.completed')
        .match(pattern('task').where({ type: 'test', result: { passed: true } }))
        .forEach(send('deploy').tell('start'));

      const eventTypes = q.getTriggerEventTypes();
      expect(eventTypes).toEqual(['task.lifecycle.completed']);

      const definition = q.build();
      expect(definition.metadata?.triggerEventTypes).toEqual(['task.lifecycle.completed']);
      expect(definition.patterns).toHaveLength(1);
      expect(definition.actions).toHaveLength(1);
    });

    test('handles multiple event types from query builder', () => {
      const q = query()
        .on(['task.created', 'task.updated'])
        .match(pattern('task').where({ priority: 'critical' }))
        .forEach(send('alerts').tell('notify'));

      const eventTypes = q.getTriggerEventTypes();
      expect(eventTypes).toEqual(['task.created', 'task.updated']);
    });
  });

  describe('Performance', () => {
    test('handles high event throughput', async () => {
      manager.register({
        id: 'throughput-test',
        eventTypes: ['task.updated'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      const startTime = Date.now();
      const eventCount = 1000;

      // Emit 1000 events
      for (let i = 0; i < eventCount; i++) {
        await manager.emit({
          type: 'task.updated',
          data: { id: `task-${i}` },
          timestamp: Date.now(),
        });
      }

      const duration = Date.now() - startTime;

      // Should handle >1000 events/sec
      expect(duration).toBeLessThan(1000);

      // Wait for actions to complete
      await new Promise((resolve) => setTimeout(resolve, 100));

      expect(router.sentMessages.length).toBe(eventCount);
    });

    test('memory efficient cleanup', () => {
      // Register and unregister many triggers
      for (let i = 0; i < 100; i++) {
        const id = manager.register({
          id: `temp-${i}`,
          eventTypes: ['task.created'],
          actions: [],
        });
        manager.unregister(id);
      }

      // All should be cleaned up
      expect(manager.listTriggers()).toHaveLength(0);
    });
  });

  describe('Custom Event Types', () => {
    test('handles custom domain events', async () => {
      manager.register({
        id: 'custom-event',
        eventTypes: ['build.completed'],
        actions: [
          {
            type: 'send',
            target: 'deploy',
            params: { pattern: 'tell', type: 'start', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'build.completed',
        data: { buildId: 'build-123', success: true },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });

    test('handles lifecycle events', async () => {
      manager.register({
        id: 'lifecycle-event',
        eventTypes: ['task.lifecycle.completed'],
        actions: [
          {
            type: 'send',
            target: 'metrics',
            params: { pattern: 'tell', type: 'record', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.lifecycle.completed',
        data: { taskId: 'task-1', duration: 1200 },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });
  });

  describe('Event Payload Access', () => {
    test('includes trigger event in action payload', async () => {
      manager.register({
        id: 'payload-test',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: { context: 'test' } },
          },
        ],
      });

      const eventData = { id: 'task-1', priority: 'high' };

      await manager.emit({
        type: 'task.created',
        data: eventData,
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
      expect(router.sentMessages[0].payload.triggerEvent).toBeDefined();
      expect(router.sentMessages[0].payload.triggerEvent.data).toEqual(eventData);
    });

    test('preserves original payload data', async () => {
      manager.register({
        id: 'preserve-payload',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: { custom: 'data', id: 123 } },
          },
        ],
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages[0].payload.custom).toBe('data');
      expect(router.sentMessages[0].payload.id).toBe(123);
    });
  });

  describe('Multiple Triggers on Same Event', () => {
    test('multiple triggers fire for same event type', async () => {
      manager.register({
        id: 'trigger-a',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log-a', payload: {} },
          },
        ],
      });

      manager.register({
        id: 'trigger-b',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'metrics',
            params: { pattern: 'tell', type: 'record-b', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(2);
      expect(router.sentMessages.some((m) => m.type === 'log-a')).toBe(true);
      expect(router.sentMessages.some((m) => m.type === 'record-b')).toBe(true);
    });

    test('triggers with different filters on same event', async () => {
      manager.register({
        id: 'high-priority',
        eventTypes: ['task.created'],
        pattern: {
          variable: 'task',
          where: { priority: 'high' },
        },
        actions: [
          {
            type: 'send',
            target: 'alerts',
            params: { pattern: 'tell', type: 'alert', payload: {} },
          },
        ],
      });

      manager.register({
        id: 'low-priority',
        eventTypes: ['task.created'],
        pattern: {
          variable: 'task',
          where: { priority: 'low' },
        },
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      // High priority task
      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1', priority: 'high' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
      expect(router.sentMessages[0].type).toBe('alert');

      router.clearMessages();

      // Low priority task
      await manager.emit({
        type: 'task.created',
        data: { id: 'task-2', priority: 'low' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
      expect(router.sentMessages[0].type).toBe('log');
    });
  });

  describe('Action Target Resolution', () => {
    test('uses explicit actor address from action params', async () => {
      manager.register({
        id: 'explicit-target',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'logger',
            params: {
              pattern: 'tell',
              type: 'log',
              payload: {},
              actor: address('custom-actor'),
            },
          },
        ],
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages[0].to).toBe(address('custom-actor'));
    });

    test('uses event actor when available', async () => {
      manager.register({
        id: 'event-actor',
        eventTypes: ['task.updated'],
        actions: [
          {
            type: 'send',
            target: 'task',
            params: { pattern: 'tell', type: 'refresh', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.updated',
        actor: address('task-123'),
        data: { id: 'task-123' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages[0].to).toBe(address('task-123'));
    });

    test('falls back to target variable', async () => {
      manager.register({
        id: 'fallback-target',
        eventTypes: ['task.created'],
        actions: [
          {
            type: 'send',
            target: 'default-logger',
            params: { pattern: 'tell', type: 'log', payload: {} },
          },
        ],
      });

      await manager.emit({
        type: 'task.created',
        data: { id: 'task-1' },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages[0].to).toBe(address('default-logger'));
    });
  });

  describe('Conditional Actions', () => {
    test('executes action only when all conditions met', async () => {
      manager.register({
        id: 'conditional',
        eventTypes: ['task.completed'],
        pattern: {
          variable: 'task',
          where: { type: 'test' },
        },
        filters: [
          {
            type: 'comparison',
            property: 'result.passed',
            operator: '=',
            value: true,
          },
        ],
        actions: [
          {
            type: 'send',
            target: 'deploy',
            params: { pattern: 'tell', type: 'start', payload: {} },
          },
        ],
      });

      // Should match (test passed)
      await manager.emit({
        type: 'task.completed',
        data: { id: 'task-1', type: 'test', result: { passed: true } },
        timestamp: Date.now(),
      });

      // Should not match (test failed)
      await manager.emit({
        type: 'task.completed',
        data: { id: 'task-2', type: 'test', result: { passed: false } },
        timestamp: Date.now(),
      });

      // Should not match (wrong type)
      await manager.emit({
        type: 'task.completed',
        data: { id: 'task-3', type: 'build', result: { passed: true } },
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(router.sentMessages).toHaveLength(1);
    });
  });
});
