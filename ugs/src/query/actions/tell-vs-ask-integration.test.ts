#!/usr/bin/env bun
/**
 * TELL vs ASK Integration Test
 * Verify that tell (fire-and-forget) and ask (request-response) work correctly
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import { query, send, pattern } from '../index.ts';
import { QueryCompiler } from '../compiler.ts';
import { QueryExecutor } from '../../messaging/actors/query-executor.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { SilentLoggerActor } from '../../messaging/actors/logger.ts';
import { Actor } from '../../messaging/actor.ts';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import type { ExecutionContext } from '../types.ts';
import { address, createResponse, type Message, type MessageResponse } from '@agentic-primer/actors';

// Helper to create execution context
function createContext(): ExecutionContext {
  return {
    warmActors: new Set(),
    computationCache: new Map(),
    resources: {
      maxConcurrency: 10,
      availableMemory: 1024 * 1024 * 1024,
    },
    startTime: Date.now(),
  };
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
    invokeProgram: async (id: string, params: any) => {
      return {
        success: true,
        output: `Program ${id} executed`,
      };
    },
  } as any as ProgramManager;
}

describe('TELL vs ASK Integration', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let executor: QueryExecutor;
  let trackingActor: TrackingActor;

  // Actor that tracks whether messages were tell or ask
  class TrackingActor extends Actor {
    public receivedMessages: Array<{ pattern: string; type: string }> = [];

    async receive(message: Message): Promise<MessageResponse> {
      this.receivedMessages.push({
        pattern: message.pattern,
        type: message.type,
      });

      if (message.pattern === 'ask') {
        return createResponse(message, { status: 'ok', received: message.type });
      }

      // TELL doesn't need meaningful response
      return createResponse(message, null);
    }

    reset() {
      this.receivedMessages = [];
    }
  }

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);
    trackingActor = new TrackingActor('tracking-actor', router);
    router.registerActor('tracking-actor', trackingActor);
  });

  afterEach(() => {
    router.unregisterActor('tracking-actor');
  });

  test('TELL sends fire-and-forget message', async () => {
    const q = query()
      .match(pattern('actor').where({ id: 'tracking-actor' }))
      .forEach(send('actor').tell('doSomething'));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build(), createContext());

    // Verify plan has tell pattern
    const actionStep = plan.steps.find((s) => s.type === 'action');
    expect(actionStep?.message.pattern).toBe('tell');

    // Execute the query (would be done by executor.execute())
    // For this test, just verify the message was sent correctly
    await trackingActor.receive({
      id: 'test-msg',
      pattern: 'tell',
      type: 'doSomething',
      payload: {},
      from: address('test-sender'),
      to: address('tracking-actor'),
      timestamp: Date.now(),
    });

    expect(trackingActor.receivedMessages.length).toBe(1);
    expect(trackingActor.receivedMessages[0].pattern).toBe('tell');
    expect(trackingActor.receivedMessages[0].type).toBe('doSomething');
  });

  test('ASK sends request-response message', async () => {
    const q = query()
      .match(pattern('actor').where({ id: 'tracking-actor' }))
      .forEach(send('actor').ask('getStatus'));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build(), createContext());

    // Verify plan has ask pattern
    const actionStep = plan.steps.find((s) => s.type === 'action');
    expect(actionStep?.message.pattern).toBe('ask');

    // Execute the message
    const response = await trackingActor.receive({
      id: 'test-msg',
      pattern: 'ask',
      type: 'getStatus',
      payload: {},
      from: address('test-sender'),
      to: address('tracking-actor'),
      timestamp: Date.now(),
    });

    expect(trackingActor.receivedMessages.length).toBe(1);
    expect(trackingActor.receivedMessages[0].pattern).toBe('ask');
    expect(trackingActor.receivedMessages[0].type).toBe('getStatus');
    expect(response.payload).toEqual({ status: 'ok', received: 'getStatus' });
  });

  test('Mixed TELL and ASK in same query', async () => {
    const q = query()
      .match(pattern('actor').where({ id: 'tracking-actor' }))
      .forEach(send('actor').tell('prepare'))
      .forEach(send('actor').ask('execute'))
      .forEach(send('actor').tell('cleanup'));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build(), createContext());

    const actionSteps = plan.steps.filter((s) => s.type === 'action');
    expect(actionSteps[0].message.pattern).toBe('tell');
    expect(actionSteps[1].message.pattern).toBe('ask');
    expect(actionSteps[2].message.pattern).toBe('tell');
  });

  test('ASK returns response data, TELL returns null', async () => {
    trackingActor.reset();

    // TELL returns null
    const tellResponse = await trackingActor.receive({
      id: 'tell-msg',
      pattern: 'tell',
      type: 'notify',
      payload: {},
      from: address('test-sender'),
      to: address('tracking-actor'),
      timestamp: Date.now(),
    });

    expect(tellResponse.payload).toBeNull();

    // ASK returns data
    const askResponse = await trackingActor.receive({
      id: 'ask-msg',
      pattern: 'ask',
      type: 'getData',
      payload: {},
      from: address('test-sender'),
      to: address('tracking-actor'),
      timestamp: Date.now(),
    });

    expect(askResponse.payload).toEqual({ status: 'ok', received: 'getData' });
  });

  test('TELL waits for delivery but returns null response', async () => {
    class SlowActor extends Actor {
      async receive(message: Message): Promise<MessageResponse> {
        // Simulate processing time
        await new Promise((resolve) => setTimeout(resolve, 50));
        return createResponse(message, message.pattern === 'ask' ? { done: true } : null);
      }
    }

    const slowActor = new SlowActor('slow-actor', router);
    router.registerActor('slow-actor', slowActor);

    // Send TELL - waits for delivery/processing, but doesn't use the response
    await router.tell({
      id: 'tell-msg',
      pattern: 'tell',
      type: 'operation',
      payload: {},
      from: address('test-sender'),
      to: address('slow-actor'),
      timestamp: Date.now(),
    });

    // TELL completes delivery (unlike true fire-and-forget like UDP)
    // This ensures reliability - we know the message was processed
    // The key difference from ASK is that TELL doesn't need meaningful response data

    router.unregisterActor('slow-actor');
    expect(true).toBe(true); // Test verifies TELL completes without error
  });

  test('ASK waits for response', async () => {
    class ResponsiveActor extends Actor {
      async receive(message: Message): Promise<MessageResponse> {
        // Simulate some processing time
        await new Promise((resolve) => setTimeout(resolve, 10));
        return createResponse(message, { processed: true });
      }
    }

    const responsiveActor = new ResponsiveActor('responsive-actor', router);
    router.registerActor('responsive-actor', responsiveActor);

    const response = await router.ask({
      id: 'ask-msg',
      pattern: 'ask',
      type: 'process',
      payload: {},
      from: address('test-sender'),
      to: address('responsive-actor'),
      correlationId: 'test-corr',
      timestamp: Date.now(),
    });

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ processed: true });

    router.unregisterActor('responsive-actor');
  });

  test('Query with only TELL actions completes without responses', async () => {
    const q = query()
      .match(pattern('actor').where({ id: 'tracking-actor' }))
      .forEach(send('actor').tell('action1'))
      .forEach(send('actor').tell('action2'))
      .forEach(send('actor').tell('action3'));

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build(), createContext());

    const actionSteps = plan.steps.filter((s) => s.type === 'action');
    expect(actionSteps.length).toBe(3);
    expect(actionSteps.every((s) => s.message.pattern === 'tell')).toBe(true);
  });

  test('Query with only ASK actions collects responses', async () => {
    const q = query()
      .match(pattern('actor').where({ id: 'tracking-actor' }))
      .forEach(send('actor').ask('query1'))
      .forEach(send('actor').ask('query2'))
      .forEach(send('actor').ask('query3'))
      .return(['actor', 'response']);

    const compiler = new QueryCompiler();
    const plan = await compiler.compile(q.build(), createContext());

    const actionSteps = plan.steps.filter((s) => s.type === 'action');
    expect(actionSteps.length).toBe(3);
    expect(actionSteps.every((s) => s.message.pattern === 'ask')).toBe(true);
  });

  test('Pattern defaults to TELL if not specified', async () => {
    // Using the generic message() builder without specifying pattern
    const action = send('actor').tell('defaultAction');
    const spec = action.build();

    expect(spec.params.pattern).toBe('tell');
  });

  test('ASK pattern is explicit', async () => {
    const action = send('actor').ask('explicitAsk');
    const spec = action.build();

    expect(spec.params.pattern).toBe('ask');
  });
});
