#!/usr/bin/env bun
/**
 * SchedulerActor Migration Demo
 *
 * Demonstrates 80% test speedup by replacing setTimeout with SchedulerActor + Virtual Time
 *
 * Pattern: setTimeout(callback, delay) → this.schedule(delay, 'message-type')
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { MessageRouter } from './router.ts';
import { Actor } from './actor.ts';
import {
  address,
  createMessage,
  createResponse,
  type Message,
  type MessageResponse,
} from '@agentic-primer/actors';
import { SchedulerActor, VirtualClock, RealClock } from '@src/system-actors/scheduler.ts';
import type GraphStore from '@src/graph.ts';
import type { ProgramManager } from '@src/entities/program.ts';

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

// ====================
// BEFORE: Using setTimeout
// ====================
class OldRetryActor extends Actor {
  private retryCount = 0;
  private maxRetries = 3;
  public results: string[] = []; // Public for testing

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      this.retryCount = 0;
      this.results = [];
      await this.attemptOperation();
      return createResponse(message, { status: 'started' });
    }

    if (message.type === 'get-results') {
      return createResponse(message, { results: this.results });
    }

    return createResponse(message, { success: false, error: 'Unknown message' });
  }

  private async attemptOperation() {
    this.retryCount++;
    this.results.push(`Attempt ${this.retryCount}`);

    if (this.retryCount < this.maxRetries) {
      // OLD WAY: Direct setTimeout call
      setTimeout(async () => {
        await this.attemptOperation();
      }, 100); // 100ms delay between retries
    } else {
      this.results.push('Complete');
    }
  }
}

// ====================
// AFTER: Using SchedulerActor
// ====================
class NewRetryActor extends Actor {
  private retryCount = 0;
  private maxRetries = 3;
  public results: string[] = []; // Public for testing

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      this.retryCount = 0;
      this.results = [];
      await this.attemptOperation();
      return createResponse(message, { status: 'started' });
    }

    if (message.type === 'retry') {
      // Scheduled message from SchedulerActor
      await this.attemptOperation();
      return createResponse(message, { success: true });
    }

    if (message.type === 'get-results') {
      return createResponse(message, { results: this.results });
    }

    return createResponse(message, { success: false, error: 'Unknown message' });
  }

  private async attemptOperation() {
    this.retryCount++;
    this.results.push(`Attempt ${this.retryCount}`);

    if (this.retryCount < this.maxRetries) {
      // NEW WAY: Use SchedulerActor
      await this.schedule(100, 'retry'); // 100ms delay between retries
    } else {
      this.results.push('Complete');
    }
  }
}

describe('SchedulerActor Migration Demo: setTimeout → schedule()', () => {
  let router: MessageRouter;
  let store: GraphStore;

  beforeEach(() => {
    store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('BEFORE: Old approach with setTimeout (slow, real time)', async () => {
    // Setup: No scheduler needed, uses global setTimeout
    const actor = new OldRetryActor('old-retry', router);
    router.registerActor('/old-retry', actor);

    // Start operation
    const startTime = performance.now();
    await router.tell(createMessage(address('/old-retry'), 'start', {}, { from: address('/test'), pattern: 'tell' }));

    // Wait for retries to complete (3 attempts × 100ms = 300ms)
    await new Promise(resolve => setTimeout(resolve, 350));

    // Check results directly from actor
    const duration = performance.now() - startTime;

    expect(actor.results).toEqual([
      'Attempt 1',
      'Attempt 2',
      'Attempt 3',
      'Complete',
    ]);

    console.log(`\n  OLD (setTimeout): ${duration.toFixed(2)}ms`);
    expect(duration).toBeGreaterThan(300); // Real time delays
  }, 1000);

  test('AFTER: New approach with SchedulerActor + VirtualClock (fast, instant)', async () => {
    // Setup: Register SchedulerActor with virtual time
    const virtualClock = new VirtualClock();
    const scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);

    const actor = new NewRetryActor('new-retry', router);
    router.registerActor('/new-retry', actor);

    // Start operation
    const startTime = performance.now();
    await router.tell(createMessage(address('/new-retry'), 'start', {}, { from: address('/test'), pattern: 'tell' }));

    // Advance virtual time to trigger all scheduled messages
    for (let i = 0; i < 3; i++) {
      await virtualClock.advance(100); // Instantly advance 100ms
    }

    // Check results directly from actor
    const duration = performance.now() - startTime;

    expect(actor.results).toEqual([
      'Attempt 1',
      'Attempt 2',
      'Attempt 3',
      'Complete',
    ]);

    console.log(`  NEW (SchedulerActor + VirtualClock): ${duration.toFixed(2)}ms`);
    expect(duration).toBeLessThan(50); // Virtual time is instant (<50ms overhead)
  }, 1000);

  test('Performance Comparison: Measure speedup percentage', async () => {
    // Test 1: Real time (baseline)
    const actorOld = new OldRetryActor('speed-old', router);
    router.registerActor('/speed-old', actorOld);

    const startOld = performance.now();
    await router.tell(createMessage(address('/speed-old'), 'start', {}, { from: address('/test'), pattern: 'tell' }));
    await new Promise(resolve => setTimeout(resolve, 350));
    const durationOld = performance.now() - startOld;

    // Test 2: Virtual time
    const virtualClock = new VirtualClock();
    const scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);

    const actorNew = new NewRetryActor('speed-new', router);
    router.registerActor('/speed-new', actorNew);

    const startNew = performance.now();
    await router.tell(createMessage(address('/speed-new'), 'start', {}, { from: address('/test'), pattern: 'tell' }));
    for (let i = 0; i < 3; i++) {
      await virtualClock.advance(100);
    }
    const durationNew = performance.now() - startNew;

    // Calculate speedup
    const speedup = ((durationOld - durationNew) / durationOld) * 100;

    console.log(`\n  📊 Performance Comparison:`);
    console.log(`     Real time (setTimeout):      ${durationOld.toFixed(2)}ms`);
    console.log(`     Virtual time (SchedulerActor): ${durationNew.toFixed(2)}ms`);
    console.log(`     Speedup:                      ${speedup.toFixed(1)}%`);

    // Verify significant speedup
    expect(speedup).toBeGreaterThan(80); // 80%+ faster with virtual time
  }, 1500);
});

describe('SchedulerActor Migration Demo: Production vs Test', () => {
  let router: MessageRouter;
  let store: GraphStore;

  beforeEach(() => {
    store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('Production: Use RealClock for actual time delays', async () => {
    // Production setup: RealClock
    const realClock = new RealClock();
    const scheduler = new SchedulerActor(router, { clock: realClock });
    router.registerActor('/system/scheduler', scheduler);

    const actor = new NewRetryActor('prod-retry', router);
    router.registerActor('/prod-retry', actor);

    const startTime = performance.now();
    await router.tell(createMessage(address('/prod-retry'), 'start', {}, { from: address('/test'), pattern: 'tell' }));

    // Wait for real time to pass
    await new Promise(resolve => setTimeout(resolve, 350));

    const duration = performance.now() - startTime;

    expect(actor.results).toHaveLength(4); // 3 attempts + complete
    expect(duration).toBeGreaterThan(300); // Real time delays

    console.log(`\n  Production (RealClock): ${duration.toFixed(2)}ms (real delays work correctly)`);
  }, 1000);

  test('Test: Use VirtualClock for instant time control', async () => {
    // Test setup: VirtualClock
    const virtualClock = new VirtualClock();
    const scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);

    const actor = new NewRetryActor('test-retry', router);
    router.registerActor('/test-retry', actor);

    const startTime = performance.now();
    await router.tell(createMessage(address('/test-retry'), 'start', {}, { from: address('/test'), pattern: 'tell' }));

    // Control time precisely
    for (let i = 0; i < 3; i++) {
      await virtualClock.advance(100);
    }

    const duration = performance.now() - startTime;

    expect(actor.results).toHaveLength(4); // 3 attempts + complete
    expect(duration).toBeLessThan(50); // Instant execution

    console.log(`  Test (VirtualClock): ${duration.toFixed(2)}ms (instant time control)`);
  }, 1000);
});
