#!/usr/bin/env bun
/**
 * Test suite for SubprocessCodeComputeActor
 *
 * Tests cover:
 * - Basic code execution
 * - Console output capture
 * - Error handling
 * - Timeout protection (including synchronous infinite loops)
 * - Process isolation
 * - TypeScript support
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import { MessageRouter } from '../router.ts';
import { SubprocessCodeComputeActor } from './compute/subprocess-code.ts';
import type { Message } from '@agentic-primer/actors';

describe('SubprocessCodeComputeActor', () => {
  let router: MessageRouter;
  let actor: SubprocessCodeComputeActor;

  beforeEach(() => {
    router = new MessageRouter();
    actor = new SubprocessCodeComputeActor(router, 5000);
  });

  test('executes simple JavaScript code', async () => {
    const message: Message = {
      id: 'test-1',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: '1 + 1',
        language: 'javascript',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.result).toBe(2);
    expect(response.payload.executionTime).toBeGreaterThan(0);
  });

  test('captures console.log output', async () => {
    const message: Message = {
      id: 'test-2',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: `
          console.log('Hello');
          console.log('World');
          42
        `,
        language: 'javascript',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.result).toBe(42);
    expect(response.payload.logs).toContain('Hello');
    expect(response.payload.logs).toContain('World');
  });

  test('handles errors gracefully', async () => {
    const message: Message = {
      id: 'test-3',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: 'throw new Error("Test error")',
        language: 'javascript',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Test error');
  });

  test('handles synchronous infinite loop with timeout (CRITICAL TEST)', async () => {
    const message: Message = {
      id: 'test-4',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: 'while(true) {}',  // Synchronous infinite loop
        language: 'javascript',
        timeout: 1000,  // 1 second timeout
      },
      timestamp: Date.now(),
    };

    const startTime = Date.now();
    const response = await actor.receive(message);
    const elapsed = Date.now() - startTime;

    // Should timeout and kill the process
    expect(response.success).toBe(false);
    expect(response.error).toContain('timed out');
    expect(response.payload.killedByTimeout).toBe(true);

    // Should complete within reasonable time after timeout
    expect(elapsed).toBeGreaterThanOrEqual(1000);
    expect(elapsed).toBeLessThan(2000);  // Allow some overhead
  }, 5000);

  test('executes async code with timeout', async () => {
    const message: Message = {
      id: 'test-5',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: `
          await new Promise(resolve => setTimeout(resolve, 100));
          'completed'
        `,
        language: 'javascript',
        timeout: 500,
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    // Note: setTimeout is blocked in sandbox, so this will fail
    // but won't hang - it will error immediately
    expect(response.success).toBe(false);
  });

  test('supports TypeScript code', async () => {
    const message: Message = {
      id: 'test-6',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: `
          const x: number = 42;
          const y: string = 'hello';
          x + y.length
        `,
        language: 'typescript',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.result).toBe(47);
  });

  test('isolates execution environment', async () => {
    const message: Message = {
      id: 'test-7',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: `
          // Try to access blocked globals
          const results = {
            process: typeof process,
            Bun: typeof Bun,
            fetch: typeof fetch,
            require: typeof require,
          };
          results
        `,
        language: 'javascript',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.result.process).toBe('undefined');
    expect(response.payload.result.Bun).toBe('undefined');
    expect(response.payload.result.fetch).toBe('undefined');
    expect(response.payload.result.require).toBe('undefined');
  });

  test('handles multiple executions independently', async () => {
    const message1: Message = {
      id: 'test-8a',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: '1 + 1',
        language: 'javascript',
      },
      timestamp: Date.now(),
    };

    const message2: Message = {
      id: 'test-8b',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: '2 + 2',
        language: 'javascript',
      },
      timestamp: Date.now(),
    };

    const response1 = await actor.receive(message1);
    const response2 = await actor.receive(message2);

    expect(response1.success).toBe(true);
    expect(response1.payload.result).toBe(2);
    expect(response2.success).toBe(true);
    expect(response2.payload.result).toBe(4);
  });

  test('validates required parameters', async () => {
    const message: Message = {
      id: 'test-9',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        language: 'javascript',
        // Missing 'code' parameter
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('No code provided');
  });

  test('validates language parameter', async () => {
    const message: Message = {
      id: 'test-10',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: 'print("hello")',
        language: 'python',  // Unsupported language
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Unsupported language');
  });

  test('handles complex objects in output', async () => {
    const message: Message = {
      id: 'test-11',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: `
          const obj = { a: 1, b: [2, 3], c: { d: 4 } };
          console.log(obj);
          obj
        `,
        language: 'javascript',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.result).toEqual({ a: 1, b: [2, 3], c: { d: 4 } });
    expect(response.payload.logs[0]).toContain('"a"');
  });

  test('rejects wrong message type', async () => {
    const message: Message = {
      id: 'test-12',
      from: 'test',
      to: 'code-execution',
      type: 'unknown' as any,
      payload: {},
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain("Expected message type 'execute'");
  });

  test('handles CPU-intensive synchronous code with timeout', async () => {
    const message: Message = {
      id: 'test-13',
      from: 'test',
      to: 'code-execution',
      type: 'execute',
      payload: {
        code: `
          let sum = 0;
          for (let i = 0; i < 100000000000; i++) {
            sum += i;
          }
          sum
        `,
        language: 'javascript',
        timeout: 500,
      },
      timestamp: Date.now(),
    };

    const startTime = Date.now();
    const response = await actor.receive(message);
    const elapsed = Date.now() - startTime;

    // Should timeout and kill the process
    expect(response.success).toBe(false);
    expect(response.payload.killedByTimeout).toBe(true);
    expect(elapsed).toBeGreaterThanOrEqual(500);
    expect(elapsed).toBeLessThan(1500);
  }, 5000);
});
