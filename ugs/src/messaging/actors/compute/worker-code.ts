#!/usr/bin/env bun
/**
 * WorkerCodeComputeActor - Code execution using Web Workers
 *
 * ⚠️  IMPORTANT: Bun's Web Worker Implementation Limitations
 * ⚠️  Bun Workers may share process space with parent - NOT fully isolated like browser Workers
 * ⚠️  Constructor escapes (CE-001, CE-002) may still be possible in Bun's Worker implementation
 *
 * Security Model:
 * - Each execution spawns a new Web Worker thread
 * - Worker terminates after execution (no state persistence)
 * - Timeout enforced by terminating worker
 * - ✅ CE-003 BLOCKED: Prototype pollution (worker terminates, no persistence)
 * - ✅ Synchronous infinite loops can be terminated (kill worker thread)
 *
 * Potential Vulnerabilities (Bun-specific):
 * - ⚠️  CE-001 POSSIBLE: Constructor chain escapes (Bun Workers may access parent process)
 * - ⚠️  CE-002 POSSIBLE: Function.constructor escapes (Bun Workers share process space)
 * - ⚠️  CE-004 POSSIBLE: Global scope via 'this' (if Bun Worker isn't fully isolated)
 *
 * Advantages over UnsafeCodeExecutionActor:
 * - ✅ Can terminate infinite loops (UnsafeCodeExecutionActor cannot)
 * - ✅ No prototype pollution persistence (worker terminates)
 * - ✅ Better resource isolation (separate thread)
 *
 * Trade-offs:
 * - Slower than UnsafeCodeExecutionActor (~10-50ms overhead per execution)
 * - Worker spawn cost adds latency
 * - Still faster than subprocess isolation (~100-500ms)
 * - May not provide full security isolation in Bun
 *
 * Use Cases:
 * - Protecting against infinite loops
 * - Preventing prototype pollution
 * - Better resource management
 * - Trusted code with some isolation
 *
 * For Maximum Security with Untrusted Code:
 * - Use SubprocessCodeExecutionActor (OS-level isolation)
 * - Or use in browser environment where Workers are fully isolated
 *
 * Features:
 * - Worker-based execution (thread-level)
 * - Timeout with hard termination (can stop infinite loops)
 * - Configurable timeout limits
 * - Output capture
 * - Clean worker lifecycle management
 */

import { MessageRouter } from '../../router.ts';
import { Actor } from '../../actor.ts';
import type { Message, MessageResponse } from '@agentic-primer/actors';

interface ExecutionResult {
  result: any;
  logs: string[];
  executionTime: number;
}

/**
 * WorkerCodeComputeActor - Secure code execution via Web Workers
 */
export class WorkerCodeComputeActor extends Actor {
  private defaultTimeout: number;
  private workerPath: string;

  constructor(router: MessageRouter, defaultTimeout: number = 5000) {
    super('code-execution', router);
    this.defaultTimeout = defaultTimeout;
    // Worker path is resolved relative to this file
    this.workerPath = new URL('./workers/code-executor-worker.ts', import.meta.url).href;
  }

  /**
   * Execute code in isolated Web Worker
   */
  private async executeInWorker(code: string, language: string, timeout: number): Promise<ExecutionResult> {
    return new Promise((resolve, reject) => {
      const startTime = Date.now();
      let worker: Worker | null = null;
      let timeoutId: Timer | null = null;
      let hasCompleted = false;

      try {
        // Create new worker for this execution
        worker = new Worker(this.workerPath);

        // Set up timeout to terminate worker if it runs too long
        timeoutId = setTimeout(() => {
          if (!hasCompleted && worker) {
            hasCompleted = true;
            worker.terminate();
            reject(new Error(`Execution timed out after ${timeout}ms`));
          }
        }, timeout);

        // Handle worker messages (results)
        worker.onmessage = (event: MessageEvent) => {
          if (hasCompleted) return;
          hasCompleted = true;

          // Clear timeout
          if (timeoutId) {
            clearTimeout(timeoutId);
          }

          const executionTime = Date.now() - startTime;
          const { success, result, logs, error } = event.data;

          // Terminate worker immediately after getting result
          if (worker) {
            worker.terminate();
          }

          if (success) {
            resolve({
              result,
              logs,
              executionTime,
            });
          } else {
            reject(new Error(error || 'Unknown execution error'));
          }
        };

        // Handle worker errors
        worker.onerror = (error: ErrorEvent) => {
          if (hasCompleted) return;
          hasCompleted = true;

          // Clear timeout
          if (timeoutId) {
            clearTimeout(timeoutId);
          }

          // Terminate worker
          if (worker) {
            worker.terminate();
          }

          reject(new Error(`Worker error: ${error.message}`));
        };

        // Send code to worker for execution
        worker.postMessage({
          code,
          language,
          timeout,
        });
      } catch (error: any) {
        hasCompleted = true;

        // Cleanup on error
        if (timeoutId) {
          clearTimeout(timeoutId);
        }
        if (worker) {
          worker.terminate();
        }

        reject(new Error(`Failed to spawn worker: ${error.message}`));
      }
    });
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type !== 'execute') {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: `Expected message type 'execute', got '${message.type}'`,
        timestamp: Date.now(),
      };
    }

    try {
      const {
        code,
        language = 'javascript',
        timeout = this.defaultTimeout
      } = message.payload;

      if (!code) {
        throw new Error('No code provided');
      }

      if (!['javascript', 'typescript'].includes(language)) {
        throw new Error(`Unsupported language: ${language}. Supported: javascript, typescript`);
      }

      // Execute in isolated worker
      const { result, logs, executionTime } = await this.executeInWorker(code, language, timeout);

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: {
          result,
          logs,
          executionTime,
          language,
        },
        timestamp: Date.now(),
      };
    } catch (error: any) {
      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: false,
        error: error.message || String(error),
        payload: {
          logs: [],  // No logs on worker spawn/termination errors
        },
        timestamp: Date.now(),
      };
    }
  }
}
