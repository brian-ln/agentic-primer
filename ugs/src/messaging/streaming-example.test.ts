#!/usr/bin/env bun
/**
 * AsyncIterator Streaming Example
 * Demonstrates usage patterns for the new streaming API
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { MessageRouter } from './router.ts';
import { Actor } from './actor.ts';
import {
  type AsyncStreamMessage,
  address,
  generateCorrelationId,
  generateMessageId,
} from '@agentic-primer/actors';
import type GraphStore from '@src/graph.ts';
import type { ProgramManager } from '@src/entities/program.ts';

// Example: SessionActor that streams messages from a session
class SessionActor extends Actor {
  private messages: any[];

  constructor(id: string, router: MessageRouter, messages: any[] = []) {
    super(id, router);
    this.messages = messages;
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

  // Example: Implement streamAsync using the helper
  async *streamAsync(payload: { query?: string }): AsyncIterableIterator<AsyncStreamMessage<any>> {
    // Filter messages based on query
    const filtered = payload.query
      ? this.messages.filter((m) => m.content.includes(payload.query!))
      : this.messages;

    // Use the helper to create the stream
    yield* this.createAsyncStream(filtered);
  }
}

// Example: AI Model Actor that streams tokens
class AIModelActor extends Actor {
  constructor(id: string, router: MessageRouter) {
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

  // Example: Stream AI-generated tokens
  async *streamAsync(payload: { prompt: string }): AsyncIterableIterator<AsyncStreamMessage<string>> {
    const corrId = generateCorrelationId();

    // Simulate AI token generation
    const response = `This is a response to: ${payload.prompt}`;
    const tokens = response.split(' ');

    for (const token of tokens) {
      // Simulate processing delay
      await new Promise((resolve) => setTimeout(resolve, 10));

      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'data',
        payload: token,
        timestamp: Date.now(),
      };
    }

    // End of stream
    yield {
      id: generateMessageId(),
      correlationId: corrId,
      from: this.address,
      type: 'end',
      timestamp: Date.now(),
    };
  }
}

function createMockGraphStore(): GraphStore {
  const nodes = new Map();
  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
  } as any as GraphStore;
}

function createMockProgramManager(): ProgramManager {
  return {} as any as ProgramManager;
}

describe('Streaming Examples', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('Example: Stream session messages with filtering', async () => {
    const messages = [
      { id: 1, content: 'Hello world' },
      { id: 2, content: 'How are you?' },
      { id: 3, content: 'world peace' },
    ];

    const session = new SessionActor('session-123', router, messages);
    router.registerActor('session-123', session);

    // Query for messages containing 'world'
    const results: any[] = [];
    for await (const msg of router.streamAsync(address('session-123'), 'query', {
      query: 'world',
    })) {
      if (msg.type === 'data') {
        results.push(msg.payload);
      }
    }

    expect(results).toHaveLength(2);
    expect(results[0].content).toBe('Hello world');
    expect(results[1].content).toBe('world peace');
  });

  test('Example: Stream AI tokens with cancellation', async () => {
    const model = new AIModelActor('ai-model', router);
    router.registerActor('ai-model', model);

    const controller = new AbortController();
    const tokens: string[] = [];

    // Cancel after receiving 3 tokens
    setTimeout(() => {
      if (tokens.length >= 3) {
        controller.abort('Got enough tokens');
      }
    }, 50);

    try {
      for await (const msg of router.streamAsync(address('ai-model'), 'generate', {
        prompt: 'Hello',
      }, {
        signal: controller.signal,
      })) {
        if (msg.type === 'data') {
          tokens.push(msg.payload);
          if (tokens.length >= 3) {
            controller.abort('Got enough tokens');
          }
        }
      }
    } catch (error: any) {
      // Expected cancellation
      expect(error.message).toMatch(/cancelled|Got enough tokens/);
    }

    expect(tokens.length).toBeGreaterThanOrEqual(3);
  }, 5000);

  test('Example: Stream with backpressure monitoring', async () => {
    const messages = Array.from({ length: 100 }, (_, i) => ({
      id: i,
      content: `Message ${i}`,
    }));

    const session = new SessionActor('session-slow', router, messages);
    router.registerActor('session-slow', session);

    const results: any[] = [];
    const startTime = Date.now();

    // Slow consumer: 5ms per message
    for await (const msg of router.streamAsync(address('session-slow'), 'query', {}, {
      bufferSize: 20, // Small buffer to test backpressure
    })) {
      if (msg.type === 'data') {
        await new Promise((resolve) => setTimeout(resolve, 5));
        results.push(msg.payload);
      }
      if (msg.type === 'end') break;
    }

    const duration = Date.now() - startTime;

    expect(results).toHaveLength(100);
    // Should take at least 500ms (5ms * 100)
    expect(duration).toBeGreaterThan(400);
  }, 10000);

  test('Example: Early termination pattern', async () => {
    const messages = Array.from({ length: 1000 }, (_, i) => ({
      id: i,
      content: `Message ${i}`,
    }));

    const session = new SessionActor('session-large', router, messages);
    router.registerActor('session-large', session);

    const results: any[] = [];

    // Find first 10 messages matching a pattern
    for await (const msg of router.streamAsync(address('session-large'), 'query', {})) {
      if (msg.type === 'data') {
        results.push(msg.payload);
        // Early termination: stop after 10 items
        if (results.length >= 10) break;
      }
    }

    expect(results).toHaveLength(10);
  });

  test('Example: Timeout for slow streams', async () => {
    class SlowActor extends Actor {
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

      async *streamAsync(): AsyncIterableIterator<AsyncStreamMessage<string>> {
        const corrId = generateCorrelationId();
        // Very slow stream: 200ms per item
        for (let i = 0; i < 100; i++) {
          await new Promise((resolve) => setTimeout(resolve, 200));
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: `item${i}`,
            timestamp: Date.now(),
          };
        }
      }
    }

    const slow = new SlowActor('slow-actor', router);
    router.registerActor('slow-actor', slow);

    await expect(async () => {
      for await (const msg of router.streamAsync(address('slow-actor'), 'test', {}, {
        timeout: 500, // Timeout after 500ms
      })) {
        // Should timeout before completing
      }
    }).toThrow(/timeout/i);
  }, 3000);
});

describe('Streaming API Documentation Examples', () => {
  test('Example: Basic usage pattern', async () => {
    // This is the recommended pattern for using streamAsync
    const router = new MessageRouter(
      createMockGraphStore(),
      createMockProgramManager()
    );

    const messages = ['msg1', 'msg2', 'msg3'];
    const actor = new SessionActor('demo', router, messages);
    router.registerActor('demo', actor);

    // Standard for-await-of loop
    const results: string[] = [];
    for await (const event of router.streamAsync(address('demo'), 'query', {})) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'end') {
        break; // Stream completed successfully
      } else if (event.type === 'error') {
        throw new Error(event.error);
      }
    }

    expect(results).toEqual(messages);
  });

  test('Example: With all options', async () => {
    const router = new MessageRouter(
      createMockGraphStore(),
      createMockProgramManager()
    );

    const actor = new SessionActor('demo2', router, [1, 2, 3]);
    router.registerActor('demo2', actor);

    const controller = new AbortController();
    const results: number[] = [];

    for await (const event of router.streamAsync(address('demo2'), 'query', {}, {
      signal: controller.signal,
      bufferSize: 50,
      timeout: 10000,
    })) {
      if (event.type === 'data') {
        results.push(event.payload);
      } else if (event.type === 'end') {
        break;
      }
    }

    expect(results).toEqual([1, 2, 3]);
  });
});
