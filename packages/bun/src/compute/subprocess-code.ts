/**
 * SubprocessCodeComputeActor - Maximum isolation via separate process
 *
 * Executes untrusted code in an isolated subprocess using Bun.spawn().
 * Communication via stdin/stdout JSON. Process can be killed on timeout.
 */

import { join } from 'node:path';
import {
  Actor,
  type Message,
  type MessageResponse,
  type IMessageRouter,
  createResponse,
  createErrorResponse,
} from '@agentic-primer/actors';

interface ExecutionResult {
  success: boolean;
  result?: unknown;
  logs: string[];
  error?: string;
  executionTime?: number;
  killedByTimeout?: boolean;
}

export class SubprocessCodeComputeActor extends Actor {
  private defaultTimeout: number;
  private workerScriptPath: string;

  constructor(id: string, router: IMessageRouter, workerScriptPath: string, defaultTimeout: number = 5000) {
    super(id, router);
    this.defaultTimeout = defaultTimeout;
    this.workerScriptPath = workerScriptPath;
  }

  private async executeInSubprocess(
    code: string,
    language: string,
    timeout: number
  ): Promise<ExecutionResult> {
    const startTime = Date.now();

    return new Promise((resolve) => {
      let timedOut = false;
      let timeoutHandle: ReturnType<typeof setTimeout> | undefined;

      try {
        const proc = Bun.spawn({
          cmd: ['bun', 'run', this.workerScriptPath],
          stdin: 'pipe',
          stdout: 'pipe',
          stderr: 'pipe',
        });

        timeoutHandle = setTimeout(() => {
          timedOut = true;
          proc.kill();
          resolve({
            success: false,
            error: `Execution timed out after ${timeout}ms (process killed)`,
            logs: [],
            executionTime: Date.now() - startTime,
            killedByTimeout: true,
          });
        }, timeout);

        const request = JSON.stringify({ code, language, timeout });
        proc.stdin.write(request);
        proc.stdin.end();

        (async () => {
          try {
            if (timedOut) return;

            const stdout = await new Response(proc.stdout).text();
            const stderr = await new Response(proc.stderr).text();

            if (timeoutHandle) {
              clearTimeout(timeoutHandle);
              timeoutHandle = undefined;
            }

            if (timedOut) return;

            const executionTime = Date.now() - startTime;

            if (stdout.trim()) {
              try {
                const result: ExecutionResult = JSON.parse(stdout);
                resolve({ ...result, executionTime });
              } catch (parseError: any) {
                resolve({
                  success: false,
                  error: `Failed to parse subprocess output: ${parseError.message}`,
                  logs: [stdout],
                  executionTime,
                });
              }
            } else if (stderr.trim()) {
              resolve({
                success: false,
                error: `Subprocess error: ${stderr}`,
                logs: [],
                executionTime,
              });
            } else {
              resolve({
                success: false,
                error: 'Subprocess produced no output',
                logs: [],
                executionTime,
              });
            }
          } catch (error: any) {
            if (timeoutHandle) clearTimeout(timeoutHandle);
            if (!timedOut) {
              resolve({
                success: false,
                error: `Subprocess communication error: ${error.message}`,
                logs: [],
                executionTime: Date.now() - startTime,
              });
            }
          }
        })();
      } catch (error: any) {
        if (timeoutHandle) clearTimeout(timeoutHandle);
        resolve({
          success: false,
          error: `Failed to spawn subprocess: ${error.message}`,
          logs: [],
          executionTime: Date.now() - startTime,
        });
      }
    });
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type !== 'execute') {
      return createErrorResponse(message, `Expected message type 'execute', got '${message.type}'`);
    }

    try {
      const {
        code,
        language = 'javascript',
        timeout = this.defaultTimeout,
      } = message.payload as { code?: string; language?: string; timeout?: number };

      if (!code) {
        throw new Error('No code provided');
      }

      if (!['javascript', 'typescript'].includes(language)) {
        throw new Error(`Unsupported language: ${language}. Supported: javascript, typescript`);
      }

      const result = await this.executeInSubprocess(code, language, timeout);

      if (result.success) {
        return createResponse(message, {
          result: result.result,
          logs: result.logs,
          executionTime: result.executionTime,
          language,
        });
      } else {
        return createErrorResponse(message, result.error || 'Execution failed');
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message || String(error));
    }
  }
}
