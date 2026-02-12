#!/usr/bin/env bun
/**
 * Code Executor Worker - Web Worker for code execution
 *
 * ⚠️  IMPORTANT: Bun's Worker Implementation Limitations
 * ⚠️  Bun Workers may NOT be fully isolated from parent process like browser Workers
 * ⚠️  Constructor-based escapes may still be possible
 *
 * Security Features:
 * - Runs in Worker thread (separate from parent thread)
 * - Clean termination ensures no state leakage
 * - Prevents prototype pollution across executions
 *
 * This worker provides:
 * - ✅ CE-003 Protection: Prototype pollution (worker terminates after execution)
 * - ✅ Infinite loop termination (parent can kill worker)
 * - ⚠️  CE-001 LIMITED: Constructor chain escapes may still work in Bun
 * - ⚠️  CE-002 LIMITED: Function.constructor escapes may still work in Bun
 * - ⚠️  CE-004 LIMITED: Global scope access may still be possible in Bun
 *
 * Note: For browser environments, Workers provide full isolation.
 * For maximum security in Bun, use SubprocessCodeExecutionActor instead.
 */

// Message structure from parent
interface ExecutionRequest {
  code: string;
  language: string;
  timeout: number;
}

// Result structure to parent
interface ExecutionResult {
  success: boolean;
  result?: any;
  logs: string[];
  error?: string;
}

// Capture logs from console methods
const logs: string[] = [];

// Override console methods to capture output
const captureConsole = {
  log: (...args: any[]) => {
    logs.push(args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
  error: (...args: any[]) => {
    logs.push('ERROR: ' + args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
  warn: (...args: any[]) => {
    logs.push('WARN: ' + args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
  info: (...args: any[]) => {
    logs.push('INFO: ' + args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
};

/**
 * Execute code in isolated worker context
 * Worker provides natural isolation - no parent access possible
 */
self.onmessage = async (event: MessageEvent<ExecutionRequest>) => {
  const { code, language, timeout } = event.data;

  // Reset logs for this execution
  logs.length = 0;

  try {
    // Prepare code for execution
    let executableCode = code;

    // Simple TypeScript handling - strip type annotations
    if (language === 'typescript') {
      executableCode = code
        .replace(/:\s*\w+(\[\])?/g, '')  // Remove type annotations
        .replace(/interface\s+\w+\s*\{[^}]*\}/g, '') // Remove interfaces
        .replace(/type\s+\w+\s*=\s*[^;]+;/g, '');    // Remove type aliases
    }

    // Execute code with captured console
    // Note: In Bun, Workers may not be fully isolated from parent process
    // This provides some protection but is not a complete security boundary
    const result = await (async function() {
      'use strict';  // Enable strict mode to prevent 'this' global access

      // Replace console with our capture implementation
      const console = captureConsole;

      // Attempt to block dangerous globals (may not be fully effective in Bun Workers)
      const process = undefined;
      const Bun = undefined;
      const Buffer = undefined;
      const require = undefined;
      const global = undefined;
      const globalThis = undefined;
      const setTimeout = undefined;
      const setInterval = undefined;
      const fetch = undefined;

      // Execute the user code
      // Note: Constructor escapes may still work in Bun's Worker implementation
      return eval(executableCode);
    })();

    // Send success response
    const response: ExecutionResult = {
      success: true,
      result,
      logs: [...logs],  // Copy logs array
    };

    self.postMessage(response);
  } catch (error: any) {
    // Send error response
    const response: ExecutionResult = {
      success: false,
      error: error.message || String(error),
      logs: [...logs],  // Copy logs array
    };

    self.postMessage(response);
  }
};
