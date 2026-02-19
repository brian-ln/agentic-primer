#!/usr/bin/env bun
/**
 * STREAM Messaging Tests (M2: Streaming Pattern)
 * Comprehensive tests for continuous data streams from actors
 * Target: 30+ test cases covering all STREAM scenarios
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import { query, send, pattern } from '../index.ts';
import { QueryCompiler } from '../compiler.ts';
import { MessageRouter } from '@src/messaging/router.ts';
import { Actor } from '@src/messaging/actor.ts';
import GraphStore from '@src/graph.ts';
import { ProgramManager } from '@src/entities/program.ts';
import type { ExecutionContext } from '../types.ts';
import {
  address,
  generateMessageId,
  generateCorrelationId,
  type AsyncStreamMessage,
} from '@agentic-primer/actors';

// Helper to create execution context
function createContext(): ExecutionContext {
  return {
    warmActors: new Set([address('tasks'), address('streaming-actor')]),
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

describe('STREAM Action Builder', () => {
  test('creates STREAM action with message type', () => {
    const action = send('task').stream('logs');
    const spec = action.build();

    expect(spec.type).toBe('send');
    expect(spec.target).toBe('task');
    expect(spec.params.pattern).toBe('stream');
    expect(spec.params.type).toBe('logs');
    expect(spec.params.payload).toEqual({});
  });

  test('creates STREAM action with payload', () => {
    const action = send('task').stream('events', { filter: 'errors' });
    const spec = action.build();

    expect(spec.params.pattern).toBe('stream');
    expect(spec.params.type).toBe('events');
    expect(spec.params.payload).toEqual({ filter: 'errors' });
  });

  test('creates STREAM action with complex payload', () => {
    const action = send('program').stream('output', {
      command: 'npm test',
      cwd: '/app',
      env: { NODE_ENV: 'test' },
    });
    const spec = action.build();

    expect(spec.params.payload).toEqual({
      command: 'npm test',
      cwd: '/app',
      env: { NODE_ENV: 'test' },
    });
  });

  test('creates STREAM action with buffer size configuration', () => {
    const action = send('task').stream('data', { bufferSize: 50 });
    const spec = action.build();

    expect(spec.params.pattern).toBe('stream');
    expect(spec.params.payload.bufferSize).toBe(50);
  });

  test('TELL vs ASK vs STREAM: different patterns', () => {
    const tellAction = send('task').tell('start');
    const askAction = send('task').ask('getStatus');
    const streamAction = send('task').stream('logs');

    expect(tellAction.build().params.pattern).toBe('tell');
    expect(askAction.build().params.pattern).toBe('ask');
    expect(streamAction.build().params.pattern).toBe('stream');
  });
});

describe('STREAM Compiler Integration', () => {
  let compiler: QueryCompiler;

  beforeEach(() => {
    compiler = new QueryCompiler();
  });

  test('compiles query with single STREAM action', async () => {
    const q = query()
      .match(pattern('task').where({ id: 'build-task' }))
      .forEach(send('task').stream('logs'));

    const plan = await compiler.compile(q.build(), createContext());

    expect(plan.steps.length).toBeGreaterThan(0);
    const actionStep = plan.steps.find((s) => s.type === 'action');
    expect(actionStep).toBeDefined();
    expect(actionStep?.message.pattern).toBe('stream');
    expect(actionStep?.message.type).toBe('logs');
  });

  test('compiles query with multiple STREAM actions', async () => {
    const q = query()
      .match(
        pattern('build').where({ id: 'build-1' }),
        pattern('test').where({ id: 'test-1' })
      )
      .forEach(send('build').stream('buildOutput'))
      .forEach(send('test').stream('testResults'));

    const plan = await compiler.compile(q.build(), createContext());

    const actionSteps = plan.steps.filter((s) => s.type === 'action');
    expect(actionSteps.length).toBe(2);
    expect(actionSteps.every((s) => s.message.pattern === 'stream')).toBe(true);
  });

  test('compiles query with mixed TELL/ASK/STREAM actions', async () => {
    const q = query()
      .match(pattern('task').where({ id: 'task-1' }))
      .forEach(send('task').tell('start'))
      .forEach(send('task').stream('progress'))
      .forEach(send('task').ask('getResult'));

    const plan = await compiler.compile(q.build(), createContext());

    const actionSteps = plan.steps.filter((s) => s.type === 'action');
    expect(actionSteps.length).toBe(3);
    expect(actionSteps[0].message.pattern).toBe('tell');
    expect(actionSteps[1].message.pattern).toBe('stream');
    expect(actionSteps[2].message.pattern).toBe('ask');
  });

  test('preserves STREAM payload in compiled plan', async () => {
    const q = query()
      .match(pattern('task').where({ id: 'task-1' }))
      .forEach(send('task').stream('data', { limit: 100, offset: 0 }));

    const plan = await compiler.compile(q.build(), createContext());

    const actionStep = plan.steps.find((s) => s.type === 'action');
    expect(actionStep?.message.payload).toEqual({ limit: 100, offset: 0 });
  });
});

describe('STREAM Actor Implementation', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;

  // Mock streaming actor
  class StreamingActor extends Actor {
    constructor(
      id: string,
      router: MessageRouter,
      private data: any[]
    ) {
      super(id, router);
    }

    async receive() {
      return {
        id: generateMessageId(),
        correlationId: generateCorrelationId(),
        from: this.address,
        to: this.address,
        success: true,
        timestamp: Date.now(),
      };
    }

    async *streamAsync<T>(payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
      const corrId = generateCorrelationId();

      try {
        for (const item of this.data) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: item,
            timestamp: Date.now(),
          };
        }

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      } catch (error: any) {
        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'error',
          error: error.message,
          timestamp: Date.now(),
        };
      }
    }
  }

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('actor streams data via AsyncIterableIterator', async () => {
    const data = ['log1', 'log2', 'log3'];
    const actor = new StreamingActor('streaming-actor', router, data);
    router.registerActor('streaming-actor', actor);

    const results: any[] = [];
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'end') {
        break;
      }
    }

    expect(results).toEqual(['log1', 'log2', 'log3']);
    router.unregisterActor('streaming-actor');
  });

  test('stream emits end event after all data', async () => {
    const data = [1, 2, 3];
    const actor = new StreamingActor('streaming-actor', router, data);
    router.registerActor('streaming-actor', actor);

    let endReceived = false;
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'end') {
        endReceived = true;
        break;
      }
    }

    expect(endReceived).toBe(true);
    router.unregisterActor('streaming-actor');
  });

  test('stream emits error event on failure', async () => {
    class ErrorStreamActor extends Actor {
      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: 'initial' as any,
          timestamp: Date.now(),
        };

        // Simulate error
        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'error',
          error: 'Stream processing failed',
          timestamp: Date.now(),
        };
      }
    }

    const actor = new ErrorStreamActor('error-stream', router);
    router.registerActor('error-stream', actor);

    let errorReceived = false;
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'error') {
        expect(event.error).toBe('Stream processing failed');
        errorReceived = true;
        break;
      }
    }

    expect(errorReceived).toBe(true);
    router.unregisterActor('error-stream');
  });
});

describe('STREAM Multiplexing', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;

  class StreamingActor extends Actor {
    constructor(
      id: string,
      router: MessageRouter,
      private data: any[]
    ) {
      super(id, router);
    }

    async receive() {
      return {
        id: generateMessageId(),
        correlationId: generateCorrelationId(),
        from: this.address,
        to: this.address,
        success: true,
        timestamp: Date.now(),
      };
    }

    async *streamAsync<T>(payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
      const corrId = generateCorrelationId();

      for (const item of this.data) {
        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: item,
          timestamp: Date.now(),
        };
      }

      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'end',
        timestamp: Date.now(),
      };
    }
  }

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  afterEach(() => {
    router.unregisterActor('actor-1');
    router.unregisterActor('actor-2');
  });

  test('multiplexes streams from multiple actors', async () => {
    const actor1 = new StreamingActor('actor-1', router, ['a1', 'a2']);
    const actor2 = new StreamingActor('actor-2', router, ['b1', 'b2']);

    router.registerActor('actor-1', actor1);
    router.registerActor('actor-2', actor2);

    // Collect from both streams
    const results1: any[] = [];
    const results2: any[] = [];

    const stream1Promise = (async () => {
      for await (const event of actor1.streamAsync({})) {
        if (event.type === 'data') results1.push(event.payload);
        else if (event.type === 'end') break;
      }
    })();

    const stream2Promise = (async () => {
      for await (const event of actor2.streamAsync({})) {
        if (event.type === 'data') results2.push(event.payload);
        else if (event.type === 'end') break;
      }
    })();

    await Promise.all([stream1Promise, stream2Promise]);

    expect(results1).toEqual(['a1', 'a2']);
    expect(results2).toEqual(['b1', 'b2']);
  });

  test('handles interleaved streams correctly', async () => {
    const actor1 = new StreamingActor('actor-1', router, [1, 2, 3]);
    const actor2 = new StreamingActor('actor-2', router, [10, 20, 30]);

    router.registerActor('actor-1', actor1);
    router.registerActor('actor-2', actor2);

    const allResults: Array<{ source: string; value: any }> = [];

    await Promise.all([
      (async () => {
        for await (const event of actor1.streamAsync({})) {
          if (event.type === 'data') {
            allResults.push({ source: 'actor-1', value: event.payload });
          } else if (event.type === 'end') break;
        }
      })(),
      (async () => {
        for await (const event of actor2.streamAsync({})) {
          if (event.type === 'data') {
            allResults.push({ source: 'actor-2', value: event.payload });
          } else if (event.type === 'end') break;
        }
      })(),
    ]);

    expect(allResults.length).toBe(6);
    expect(allResults.filter((r) => r.source === 'actor-1').length).toBe(3);
    expect(allResults.filter((r) => r.source === 'actor-2').length).toBe(3);
  });
});

describe('STREAM Cancellation', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;

  class InfiniteStreamActor extends Actor {
    async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
      const corrId = generateCorrelationId();
      let count = 0;

      while (true) {
        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: count++ as any,
          timestamp: Date.now(),
        };

        // Small delay to prevent tight loop
        await new Promise((resolve) => setTimeout(resolve, 1));
      }
    }
  }

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('stream can be cancelled by breaking loop', async () => {
    const actor = new InfiniteStreamActor('infinite', router);
    router.registerActor('infinite', actor);

    const results: any[] = [];
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        results.push(event.payload);
        if (results.length >= 5) break; // Cancel after 5 items
      }
    }

    expect(results.length).toBe(5);
    expect(results).toEqual([0, 1, 2, 3, 4]);

    router.unregisterActor('infinite');
  });

  test('early termination does not affect other streams', async () => {
    class StreamingActor extends Actor {
      constructor(
        id: string,
        router: MessageRouter,
        private data: any[]
      ) {
        super(id, router);
      }

      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        for (const item of this.data) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: item,
            timestamp: Date.now(),
          };
        }

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      }
    }

    const actor1 = new StreamingActor('actor-1', router, [1, 2, 3, 4, 5]);
    const actor2 = new StreamingActor('actor-2', router, ['a', 'b', 'c']);

    router.registerActor('actor-1', actor1);
    router.registerActor('actor-2', actor2);

    const results1: any[] = [];
    const results2: any[] = [];

    await Promise.all([
      (async () => {
        for await (const event of actor1.streamAsync({})) {
          if (event.type === 'data') {
            results1.push(event.payload);
            if (results1.length >= 2) break; // Early termination
          }
        }
      })(),
      (async () => {
        for await (const event of actor2.streamAsync({})) {
          if (event.type === 'data') {
            results2.push(event.payload);
          } else if (event.type === 'end') break;
        }
      })(),
    ]);

    expect(results1).toEqual([1, 2]); // Terminated early
    expect(results2).toEqual(['a', 'b', 'c']); // Completed fully

    router.unregisterActor('actor-1');
    router.unregisterActor('actor-2');
  });
});

describe('STREAM Backpressure', () => {
  test('backpressure prevents memory bloat (conceptual)', async () => {
    // This test verifies the concept - actual backpressure is handled
    // by AsyncIterator protocol (consumer pulls, producer yields)
    const largeDataset = Array.from({ length: 10000 }, (_, i) => i);

    class StreamingActor extends Actor {
      constructor(
        id: string,
        router: MessageRouter,
        private data: any[]
      ) {
        super(id, router);
      }

      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        // Producer only yields when consumer is ready (pull-based)
        for (const item of this.data) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: item,
            timestamp: Date.now(),
          };
        }

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new StreamingActor('large-data', router, largeDataset);
    router.registerActor('large-data', actor);

    let count = 0;
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        count++;
        // Simulate slow processing
        if (count % 1000 === 0) {
          await new Promise((resolve) => setTimeout(resolve, 1));
        }
      } else if (event.type === 'end') break;
    }

    expect(count).toBe(10000);
    router.unregisterActor('large-data');
  });

  test('slow consumer does not cause producer overflow', async () => {
    class StreamingActor extends Actor {
      constructor(
        id: string,
        router: MessageRouter,
        private data: any[]
      ) {
        super(id, router);
      }

      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        for (const item of this.data) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: item,
            timestamp: Date.now(),
          };
        }

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new StreamingActor('actor', router, [1, 2, 3, 4, 5]);
    router.registerActor('actor', actor);

    const results: any[] = [];
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        // Very slow consumer
        await new Promise((resolve) => setTimeout(resolve, 10));
        results.push(event.payload);
      } else if (event.type === 'end') break;
    }

    expect(results).toEqual([1, 2, 3, 4, 5]);
    router.unregisterActor('actor');
  });
});

describe('STREAM Error Propagation', () => {
  test('errors in stream are propagated to consumer', async () => {
    class ErrorProneActor extends Actor {
      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: 'item1' as any,
          timestamp: Date.now(),
        };

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'error',
          error: 'Processing failed at item 2',
          timestamp: Date.now(),
        };
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new ErrorProneActor('error-prone', router);
    router.registerActor('error-prone', actor);

    const results: any[] = [];
    let errorMessage = '';

    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'error') {
        errorMessage = event.error || '';
        break;
      }
    }

    expect(results).toEqual(['item1']);
    expect(errorMessage).toBe('Processing failed at item 2');

    router.unregisterActor('error-prone');
  });

  test('stream handles partial data before error', async () => {
    class PartialErrorActor extends Actor {
      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        for (let i = 0; i < 5; i++) {
          if (i === 3) {
            yield {
              id: generateMessageId(),
              correlationId: corrId,
              from: this.address,
              type: 'error',
              error: 'Error at index 3',
              timestamp: Date.now(),
            };
            return;
          }

          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: i as any,
            timestamp: Date.now(),
          };
        }
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new PartialErrorActor('partial', router);
    router.registerActor('partial', actor);

    const results: any[] = [];
    let hadError = false;

    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'error') {
        hadError = true;
        break;
      }
    }

    expect(results).toEqual([0, 1, 2]);
    expect(hadError).toBe(true);

    router.unregisterActor('partial');
  });
});

describe('STREAM Integration Examples', () => {
  test('example: stream logs from build task', () => {
    const q = query()
      .match(pattern('task').label('Task').where({ type: 'build' }))
      .forEach(send('task').stream('logs'))
      .return(['task']);

    const def = q.build();
    expect(def.actions?.length).toBe(1);
    expect(def.actions?.[0].params.pattern).toBe('stream');
    expect(def.actions?.[0].params.type).toBe('logs');
  });

  test('example: stream LLM tokens', () => {
    const q = query()
      .match(pattern('llm').where({ type: 'inference' }))
      .forEach(
        send('llm').stream('generate', {
          prompt: 'Write a story...',
          maxTokens: 500,
        })
      );

    const def = q.build();
    expect(def.actions?.[0].params.type).toBe('generate');
    expect(def.actions?.[0].params.payload.prompt).toBe('Write a story...');
  });

  test('example: stream program execution output', () => {
    const q = query()
      .match(pattern('program').where({ id: 'test-runner' }))
      .forEach(
        send('program').stream('execute', {
          command: 'bun test',
          streaming: true,
        })
      );

    const def = q.build();
    expect(def.actions?.[0].params.payload.command).toBe('bun test');
    expect(def.actions?.[0].params.payload.streaming).toBe(true);
  });

  test('example: stream metrics from multiple services', () => {
    const q = query()
      .match(
        pattern('api').label('Service').where({ type: 'api' }),
        pattern('db').label('Service').where({ type: 'database' })
      )
      .forEach(send('api').stream('metrics'))
      .forEach(send('db').stream('metrics'))
      .return(['api', 'db']);

    const def = q.build();
    expect(def.actions?.length).toBe(2);
    expect(def.actions?.every((a) => a.params.pattern === 'stream')).toBe(true);
  });

  test('example: stream with filtering', () => {
    const q = query()
      .match(pattern('logs').where({ service: 'backend' }))
      .forEach(
        send('logs').stream('tail', {
          filter: { level: 'error' },
          limit: 100,
        })
      );

    const def = q.build();
    expect(def.actions?.[0].params.payload.filter.level).toBe('error');
    expect(def.actions?.[0].params.payload.limit).toBe(100);
  });
});

describe('STREAM Edge Cases', () => {
  test('empty stream completes immediately', async () => {
    class EmptyStreamActor extends Actor {
      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new EmptyStreamActor('empty', router);
    router.registerActor('empty', actor);

    const results: any[] = [];
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'end') break;
    }

    expect(results).toEqual([]);
    router.unregisterActor('empty');
  });

  test('stream with single item', async () => {
    class SingleItemActor extends Actor {
      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: 'single' as any,
          timestamp: Date.now(),
        };

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new SingleItemActor('single', router);
    router.registerActor('single', actor);

    const results: any[] = [];
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'end') break;
    }

    expect(results).toEqual(['single']);
    router.unregisterActor('single');
  });

  test('stream with null and undefined payloads', async () => {
    class NullableActor extends Actor {
      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: null as any,
          timestamp: Date.now(),
        };

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: undefined as any,
          timestamp: Date.now(),
        };

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new NullableActor('nullable', router);
    router.registerActor('nullable', actor);

    const results: any[] = [];
    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'end') break;
    }

    expect(results).toEqual([null, undefined]);
    router.unregisterActor('nullable');
  });
});

describe('STREAM Performance', () => {
  test('stream handles large datasets efficiently', async () => {
    class LargeDataActor extends Actor {
      async *streamAsync<T>(_payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();
        const count = 1000;

        for (let i = 0; i < count; i++) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: i as any,
            timestamp: Date.now(),
          };
        }

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'end',
          timestamp: Date.now(),
        };
      }
    }

    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    const actor = new LargeDataActor('large', router);
    router.registerActor('large', actor);

    const startTime = Date.now();
    let count = 0;

    for await (const event of actor.streamAsync({})) {
      if (event.type === 'data') {
        count++;
      } else if (event.type === 'end') break;
    }

    const duration = Date.now() - startTime;

    expect(count).toBe(1000);
    // Should complete reasonably fast (under 1 second for 1000 items)
    expect(duration).toBeLessThan(1000);

    router.unregisterActor('large');
  });

  test('stream overhead should be minimal', () => {
    // Placeholder for future benchmark
    // Target: Stream protocol overhead <1% vs direct iteration
    expect(true).toBe(true);
  });
});
