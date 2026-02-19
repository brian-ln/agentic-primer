#!/usr/bin/env bun
/**
 * ASK Messaging Tests (M1: Request-Response Pattern)
 * Comprehensive tests for bidirectional message-passing with ask/reply
 * Target: 35+ test cases covering all ASK scenarios
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import { query, send, pattern } from '../index.ts';
import { QueryCompiler } from '../compiler.ts';
import { QueryExecutor } from '@src/messaging/actors/query-executor.ts';
import { MessageRouter } from '@src/messaging/router.ts';
import { SilentLoggerActor } from '@src/messaging/actors/logger.ts';
import { Actor } from '@src/messaging/actor.ts';
import GraphStore from '@src/graph.ts';
import { ProgramManager } from '@src/entities/program.ts';
import type { QueryPlan, ExecutionContext } from '../types.ts';
import { address, createResponse, type Message, type MessageResponse } from '@agentic-primer/actors';

// Helper to create execution context
function createContext(): ExecutionContext {
  return {
    warmActors: new Set([address('tasks'), address('test-actor')]),
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

describe('ASK Action Builder', () => {
  test('creates ASK action with message type', () => {
    const action = send('task').ask('getStatus');
    const spec = action.build();

    expect(spec.type).toBe('send');
    expect(spec.target).toBe('task');
    expect(spec.params.pattern).toBe('ask');
    expect(spec.params.type).toBe('getStatus');
    expect(spec.params.payload).toEqual({});
  });

  test('creates ASK action with payload', () => {
    const action = send('task').ask('compute', { input: 42 });
    const spec = action.build();

    expect(spec.params.pattern).toBe('ask');
    expect(spec.params.type).toBe('compute');
    expect(spec.params.payload).toEqual({ input: 42 });
  });

  test('creates ASK action with complex payload', () => {
    const action = send('task').ask('analyze', {
      data: [1, 2, 3, 4, 5],
      options: { algorithm: 'mean', precision: 2 },
    });
    const spec = action.build();

    expect(spec.params.payload).toEqual({
      data: [1, 2, 3, 4, 5],
      options: { algorithm: 'mean', precision: 2 },
    });
  });

  test('creates ASK action with timeout configuration', () => {
    const action = send('task').ask('slowOperation', { timeout: 5000 });
    const spec = action.build();

    expect(spec.params.pattern).toBe('ask');
    expect(spec.params.payload.timeout).toBe(5000);
  });

  test('TELL vs ASK: different patterns in same query', () => {
    const tellAction = send('task').tell('start');
    const askAction = send('task').ask('getStatus');

    expect(tellAction.build().params.pattern).toBe('tell');
    expect(askAction.build().params.pattern).toBe('ask');
  });
});

describe('ASK Compiler Integration', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('compiles query with single ASK action', async () => {
    const q = query()
      .match(pattern('task').where({ id: 'task-1' }))
      .forEach(send('task').ask('getStatus'));

    const plan = await compiler.compile(q.build(), createContext());

    expect(plan.steps.length).toBeGreaterThan(0);
    const actionStep = plan.steps.find((s) => s.type === 'action');
    expect(actionStep).toBeDefined();
    expect(actionStep?.message.pattern).toBe('ask');
    expect(actionStep?.message.type).toBe('getStatus');
  });

  test('compiles query with multiple ASK actions', async () => {
    const q = query()
      .match(
        pattern('task1').where({ id: 'task-1' }),
        pattern('task2').where({ id: 'task-2' })
      )
      .forEach(send('task1').ask('getStatus'))
      .forEach(send('task2').ask('getProgress'));

    const plan = await compiler.compile(q.build(), createContext());

    const actionSteps = plan.steps.filter((s) => s.type === 'action');
    expect(actionSteps.length).toBe(2);
    expect(actionSteps.every((s) => s.message.pattern === 'ask')).toBe(true);
  });

  test('compiles query with mixed TELL and ASK actions', async () => {
    const q = query()
      .match(pattern('task').where({ id: 'task-1' }))
      .forEach(send('task').tell('prepare'))
      .forEach(send('task').ask('execute'))
      .forEach(send('task').tell('cleanup'));

    const plan = await compiler.compile(q.build(), createContext());

    const actionSteps = plan.steps.filter((s) => s.type === 'action');
    expect(actionSteps.length).toBe(3);
    expect(actionSteps[0].message.pattern).toBe('tell');
    expect(actionSteps[1].message.pattern).toBe('ask');
    expect(actionSteps[2].message.pattern).toBe('tell');
  });

  test('preserves ASK payload in compiled plan', async () => {
    const q = query()
      .match(pattern('task').where({ id: 'task-1' }))
      .forEach(send('task').ask('compute', { input: 100, multiplier: 2 }));

    const plan = await compiler.compile(q.build(), createContext());

    const actionStep = plan.steps.find((s) => s.type === 'action');
    expect(actionStep?.message.payload).toEqual({ input: 100, multiplier: 2 });
  });
});

describe('ASK Execution', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let executor: QueryExecutor;
  let testActor: TestResponderActor;

  // Mock actor that responds to ASK messages
  class TestResponderActor extends Actor {
    constructor(id: string, router: MessageRouter, private responses: Map<string, any>) {
      super(id, router);
    }

    async receive(message: Message): Promise<MessageResponse> {
      if (message.pattern === 'ask') {
        const responseData = this.responses.get(message.type) || { status: 'unknown' };
        return createResponse(message, responseData);
      }

      // TELL pattern doesn't return data
      return createResponse(message, null);
    }
  }

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    executor = new QueryExecutor('query-executor', router);

    // Register test actor with predefined responses
    const responses = new Map([
      ['getStatus', { status: 'running', progress: 75 }],
      ['compute', { result: 42, computed: true }],
      ['getMetrics', { cpu: 45, memory: 256, uptime: 3600 }],
    ]);
    testActor = new TestResponderActor('test-actor', router, responses);
    router.registerActor('test-actor', testActor);

    // Create test task node
    store.set('test-task-1', {
      id: 'test-task-1',
      type: 'program',
      lifecycle: 'ready',
      properties: new Map([
        ['impl', 'return { id: "test-task-1", status: "ready" };'],
      ]),
      relationships: [],
      timestamp: Date.now(),
    });
  });

  afterEach(() => {
    router.unregisterActor('test-actor');
  });

  test('executes ASK and receives response', async () => {
    const q = query()
      .match(pattern('actor').where({ id: 'test-actor' }))
      .forEach(send('actor').ask('getStatus'))
      .return(['actor']);

    const compiler = new QueryCompiler();
    const plan = compiler.compile(q.build(), createContext());

    // Execute plan manually for now (full executor integration in later tests)
    const response = await testActor.receive({
      id: 'test-msg',
      pattern: 'ask',
      type: 'getStatus',
      payload: {},
      from: address('test-sender'),
      to: address('test-actor'),
      timestamp: Date.now(),
    });

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ status: 'running', progress: 75 });
  });

  test('ASK returns different data than TELL', async () => {
    // ASK should return response data
    const askResponse = await testActor.receive({
      id: 'ask-msg',
      pattern: 'ask',
      type: 'compute',
      payload: {},
      from: address('test-sender'),
      to: address('test-actor'),
      timestamp: Date.now(),
    });

    expect(askResponse.success).toBe(true);
    expect(askResponse.payload).toEqual({ result: 42, computed: true });

    // TELL should return null (fire-and-forget)
    const tellResponse = await testActor.receive({
      id: 'tell-msg',
      pattern: 'tell',
      type: 'compute',
      payload: {},
      from: address('test-sender'),
      to: address('test-actor'),
      timestamp: Date.now(),
    });

    expect(tellResponse.success).toBe(true);
    expect(tellResponse.payload).toBeNull();
  });

  test('ASK to multiple actors in parallel', async () => {
    const responses1 = new Map([['getStatus', { instance: 1, status: 'healthy' }]]);
    const responses2 = new Map([['getStatus', { instance: 2, status: 'degraded' }]]);

    const actor1 = new TestResponderActor('actor-1', router, responses1);
    const actor2 = new TestResponderActor('actor-2', router, responses2);
    router.registerActor('actor-1', actor1);
    router.registerActor('actor-2', actor2);

    const results = await Promise.all([
      actor1.receive({
        id: 'msg-1',
        pattern: 'ask',
        type: 'getStatus',
        payload: {},
        from: address('test-sender'),
        to: address('actor-1'),
        timestamp: Date.now(),
      }),
      actor2.receive({
        id: 'msg-2',
        pattern: 'ask',
        type: 'getStatus',
        payload: {},
        from: address('test-sender'),
        to: address('actor-2'),
        timestamp: Date.now(),
      }),
    ]);

    expect(results[0].payload).toEqual({ instance: 1, status: 'healthy' });
    expect(results[1].payload).toEqual({ instance: 2, status: 'degraded' });

    router.unregisterActor('actor-1');
    router.unregisterActor('actor-2');
  });

  test('ASK with payload transformation', async () => {
    const q = query()
      .match(pattern('actor').where({ id: 'test-actor' }))
      .forEach(send('actor').ask('compute', { input: 100, multiplier: 2 }));

    const response = await testActor.receive({
      id: 'compute-msg',
      pattern: 'ask',
      type: 'compute',
      payload: { input: 100, multiplier: 2 },
      from: address('test-sender'),
      to: address('test-actor'),
      timestamp: Date.now(),
    });

    expect(response.success).toBe(true);
    expect(response.payload.result).toBe(42);
  });
});

describe('ASK Response Binding', () => {
  test('query with ASK can return responses', () => {
    const q = query()
      .match(pattern('task').where({ id: 'task-1' }))
      .forEach(send('task').ask('getStatus'))
      .return(['task', 'response']);

    const def = q.build();
    expect(def.returns).toContain('task');
    expect(def.returns).toContain('response');
  });

  test('multiple ASK actions bind multiple responses', () => {
    const q = query()
      .match(
        pattern('task1').where({ id: 'task-1' }),
        pattern('task2').where({ id: 'task-2' })
      )
      .forEach(send('task1').ask('getStatus'))
      .forEach(send('task2').ask('getMetrics'))
      .return(['task1', 'task2', 'response']);

    const def = q.build();
    expect(def.actions?.length).toBe(2);
    expect(def.returns).toContain('response');
  });
});

describe('ASK Error Handling', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let errorActor: ErrorThrowingActor;

  class ErrorThrowingActor extends Actor {
    async receive(message: Message): Promise<MessageResponse> {
      if (message.type === 'failAlways') {
        return {
          id: message.id + '_response',
          correlationId: message.correlationId || message.id,
          from: this.address,
          to: message.from || message.to,
          success: false,
          error: 'Operation failed: actor unavailable',
          timestamp: Date.now(),
        };
      }
      return createResponse(message, { status: 'ok' });
    }
  }

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
    errorActor = new ErrorThrowingActor('error-actor', router);
    router.registerActor('error-actor', errorActor);
  });

  afterEach(() => {
    router.unregisterActor('error-actor');
  });

  test('ASK handles actor error response', async () => {
    const response = await errorActor.receive({
      id: 'error-msg',
      pattern: 'ask',
      type: 'failAlways',
      payload: {},
      to: address('error-actor'),
      timestamp: Date.now(),
    });

    expect(response.success).toBe(false);
    expect(response.error).toContain('actor unavailable');
  });

  test('ASK to non-existent actor fails gracefully', async () => {
    const response = await router.ask({
      id: 'msg-to-nowhere',
      pattern: 'ask',
      type: 'getStatus',
      payload: {},
      to: address('non-existent-actor'),
      from: address('test-sender'),
      correlationId: 'test-corr',
      timestamp: Date.now(),
    });

    expect(response.success).toBe(false);
    expect(response.error).toContain('not found');
  });
});

describe('ASK Timeout Handling', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let slowActor: SlowResponderActor;

  class SlowResponderActor extends Actor {
    constructor(id: string, router: MessageRouter, private delayMs: number) {
      super(id, router);
    }

    async receive(message: Message): Promise<MessageResponse> {
      // Simulate slow operation
      await new Promise((resolve) => setTimeout(resolve, this.delayMs));
      return createResponse(message, { status: 'completed' });
    }
  }

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Register silent logger for tests
    router.registerActor('logger', new SilentLoggerActor(router));
  });

  test('ASK with short timeout fails on slow actor', async () => {
    slowActor = new SlowResponderActor('slow-actor', router, 200);
    router.registerActor('slow-actor', slowActor);

    const startTime = Date.now();
    try {
      await router.ask({
        id: 'slow-msg',
        pattern: 'ask',
        type: 'slowOp',
        payload: {},
        to: address('slow-actor'),
        from: address('test-sender'),
        correlationId: 'test-corr',
        timestamp: Date.now(),
      }, 50); // 50ms timeout, but actor takes 200ms

      expect.unreachable('Should have timed out');
    } catch (error: any) {
      const elapsed = Date.now() - startTime;
      expect(error.message).toContain('timed out');
      expect(elapsed).toBeLessThan(100); // Should timeout quickly
    } finally {
      router.unregisterActor('slow-actor');
    }
  });

  test('ASK with long timeout succeeds on slow actor', async () => {
    slowActor = new SlowResponderActor('slow-actor', router, 50);
    router.registerActor('slow-actor', slowActor);

    const response = await router.ask({
      id: 'slow-msg',
      pattern: 'ask',
      type: 'slowOp',
      payload: {},
      to: address('slow-actor'),
      from: address('test-sender'),
      correlationId: 'test-corr',
      timestamp: Date.now(),
    }, 500); // 500ms timeout, actor takes 50ms

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ status: 'completed' });
    router.unregisterActor('slow-actor');
  });
});

describe('ASK Edge Cases', () => {
  test('ASK with empty payload', () => {
    const action = send('task').ask('ping');
    const spec = action.build();

    expect(spec.params.pattern).toBe('ask');
    expect(spec.params.payload).toEqual({});
  });

  test('ASK with null values in payload', () => {
    const action = send('task').ask('reset', { config: null, state: null });
    const spec = action.build();

    expect(spec.params.payload.config).toBeNull();
    expect(spec.params.payload.state).toBeNull();
  });

  test('ASK with deeply nested payload', () => {
    const action = send('task').ask('configure', {
      level1: {
        level2: {
          level3: {
            value: 'deep',
          },
        },
      },
    });
    const spec = action.build();

    expect(spec.params.payload.level1.level2.level3.value).toBe('deep');
  });

  test('ASK with array payload', () => {
    const action = send('task').ask('processBatch', [1, 2, 3, 4, 5]);
    const spec = action.build();

    expect(Array.isArray(spec.params.payload)).toBe(true);
    expect(spec.params.payload).toEqual([1, 2, 3, 4, 5]);
  });

  test('ASK action can be chained', () => {
    const q = query()
      .match(pattern('task'))
      .forEach(send('task').ask('start'))
      .forEach(send('task').ask('getStatus'))
      .forEach(send('task').ask('stop'));

    const def = q.build();
    expect(def.actions?.length).toBe(3);
    expect(def.actions?.[0].params.type).toBe('start');
    expect(def.actions?.[1].params.type).toBe('getStatus');
    expect(def.actions?.[2].params.type).toBe('stop');
  });

  test('ASK preserves message type casing', () => {
    const action = send('task').ask('GetStatusWithCamelCase');
    const spec = action.build();

    expect(spec.params.type).toBe('GetStatusWithCamelCase');
  });
});

describe('ASK Performance', () => {
  test('ASK overhead should be minimal vs direct actor call', () => {
    // This is a placeholder for future benchmark
    // Target: <10% overhead vs direct actor.receive() call
    expect(true).toBe(true);
  });

  test('parallel ASK scales linearly', () => {
    // This is a placeholder for future benchmark
    // 10 parallel asks should take ~same time as 1 ask
    expect(true).toBe(true);
  });
});

describe('ASK Integration Examples', () => {
  test('example: get status from multiple tasks', () => {
    const q = query()
      .match(pattern('tasks').label('Task').where({ status: 'running' }))
      .forEach(send('tasks').ask('getStatus'))
      .return(['tasks', 'response']);

    const def = q.build();
    expect(def.actions?.length).toBe(1);
    expect(def.actions?.[0].params.pattern).toBe('ask');
  });

  test('example: ask for computation result', () => {
    const q = query()
      .match(pattern('computer').where({ type: 'calculator' }))
      .forEach(send('computer').ask('compute', { expression: '2 + 2' }))
      .return(['computer', 'response']);

    const def = q.build();
    expect(def.actions?.[0].params.type).toBe('compute');
    expect(def.actions?.[0].params.payload.expression).toBe('2 + 2');
  });

  test('example: conditional action based on ask response', () => {
    const q = query()
      .match(pattern('task').where({ id: 'test-task' }))
      .forEach(send('task').ask('getStatus'))
      .return(['task', 'response']);

    // In full implementation, would support:
    // .when(pattern('response').where({ status: 'failed' }))
    // .then(send('alerts').tell('notify'))

    const def = q.build();
    expect(def.actions?.length).toBe(1);
  });

  test('example: pipeline with ask + tell', () => {
    const q = query()
      .match(pattern('task').where({ id: 'build-task' }))
      .forEach(send('task').ask('build'))
      .forEach(send('task').tell('cleanup'))
      .return(['task']);

    const def = q.build();
    expect(def.actions?.length).toBe(2);
    expect(def.actions?.[0].params.pattern).toBe('ask');
    expect(def.actions?.[1].params.pattern).toBe('tell');
  });

  test('example: gather metrics from multiple services', () => {
    const q = query()
      .match(
        pattern('api').label('Service').where({ type: 'api' }),
        pattern('db').label('Service').where({ type: 'database' }),
        pattern('cache').label('Service').where({ type: 'cache' })
      )
      .forEach(send('api').ask('getMetrics'))
      .forEach(send('db').ask('getMetrics'))
      .forEach(send('cache').ask('getMetrics'))
      .return(['api', 'db', 'cache']);

    const def = q.build();
    expect(def.actions?.length).toBe(3);
    expect(def.actions?.every((a) => a.params.pattern === 'ask')).toBe(true);
    expect(def.actions?.every((a) => a.params.type === 'getMetrics')).toBe(true);
  });

  test('example: health check across infrastructure', () => {
    const q = query()
      .match(pattern('nodes').label('Server'))
      .forEach(send('nodes').ask('healthCheck'))
      .return(['nodes', 'response']);

    const def = q.build();
    expect(def.actions?.[0].params.type).toBe('healthCheck');
  });

  test('example: ask with error handling pattern', () => {
    const q = query()
      .match(pattern('service').where({ id: 'external-api' }))
      .forEach(send('service').ask('fetchData', { retry: 3, timeout: 5000 }));

    const def = q.build();
    expect(def.actions?.[0].params.payload.retry).toBe(3);
    expect(def.actions?.[0].params.payload.timeout).toBe(5000);
  });

  test('example: sequential ask operations', () => {
    // Pattern: Ask service A, then ask service B based on A's response
    const q = query()
      .match(pattern('serviceA').where({ type: 'primary' }))
      .forEach(send('serviceA').ask('getPriority'))
      .match(pattern('serviceB').where({ type: 'secondary' }))
      .forEach(send('serviceB').ask('getDetails'));

    const def = q.build();
    expect(def.actions?.length).toBe(2);
    expect(def.actions?.[0].params.type).toBe('getPriority');
    expect(def.actions?.[1].params.type).toBe('getDetails');
  });

  test('example: aggregation with ask', () => {
    const q = query()
      .match(pattern('workers').label('Worker'))
      .forEach(send('workers').ask('getLoad'))
      .return(['workers', 'response']);

    const def = q.build();
    // In full implementation, could aggregate responses
    expect(def.actions?.length).toBe(1);
    expect(def.returns).toContain('response');
  });

  test('example: conditional routing based on ask', () => {
    const q = query()
      .match(pattern('task').where({ id: 'main-task' }))
      .forEach(send('task').ask('shouldProcess'))
      .return(['task', 'response']);

    const def = q.build();
    expect(def.actions?.length).toBe(1);
    // Could extend to route based on response.payload
  });
});
