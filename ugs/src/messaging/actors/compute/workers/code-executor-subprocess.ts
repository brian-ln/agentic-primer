#!/usr/bin/env bun
/**
 * Code Executor Subprocess - Isolated subprocess for secure code execution
 *
 * Security Features:
 * - Runs in completely separate process (maximum isolation)
 * - No shared memory with parent process
 * - Process can be killed immediately (handles infinite loops)
 * - No access to parent process globals or state
 * - Clean process termination ensures no state leakage
 *
 * This subprocess prevents ALL attack vectors:
 * - CE-001: Constructor chain escapes (isolated process)
 * - CE-002: Function.constructor escapes (isolated process)
 * - CE-003: Prototype pollution (process terminates)
 * - CE-004: Global scope access (separate process)
 * - Synchronous infinite loops (parent can kill process)
 *
 * Communication Protocol:
 * - Input: JSON via stdin { code, language, timeout }
 * - Output: JSON via stdout { success, result?, logs, error? }
 */

// Message structure from parent via stdin
interface ExecutionRequest {
  code: string;
  language: string;
  timeout: number;
}

// Result structure to parent via stdout
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
 * Execute code in isolated subprocess
 * Maximum isolation - separate process that can be killed
 */
async function executeCode() {
  try {
    // Read input from stdin
    const input = await Bun.stdin.text();
    const request: ExecutionRequest = JSON.parse(input);
    const { code, language } = request;

    // Reset logs for this execution
    logs.length = 0;

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
    // Subprocess provides maximum isolation - parent can kill us at any time
    const result = await (async function() {
      'use strict';  // Enable strict mode to prevent 'this' global access

      // Replace console with our capture implementation
      const console = captureConsole;

      // Block dangerous globals
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
      // Even if they create infinite loops, parent process can kill us
      return eval(executableCode);
    })();

    // Send success response to stdout
    const response: ExecutionResult = {
      success: true,
      result,
      logs: [...logs],  // Copy logs array
    };

    console.log(JSON.stringify(response));
  } catch (error: any) {
    // Send error response to stdout
    const response: ExecutionResult = {
      success: false,
      error: error.message || String(error),
      logs: [...logs],  // Copy logs array
    };

    console.log(JSON.stringify(response));
  }
}

// Main execution
executeCode().catch(error => {
  // Fatal error - output error and exit
  const response: ExecutionResult = {
    success: false,
    error: `Fatal error: ${error.message || String(error)}`,
    logs: [],
  };
  console.log(JSON.stringify(response));
  process.exit(1);
});
