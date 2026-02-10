#!/usr/bin/env bun
/**
 * Code Executor Subprocess - Isolated subprocess for secure code execution
 *
 * Communication Protocol:
 * - Input: JSON via stdin { code, language, timeout }
 * - Output: JSON via stdout { success, result?, logs, error? }
 */

interface ExecutionRequest {
  code: string;
  language: string;
  timeout: number;
}

interface ExecutionResult {
  success: boolean;
  result?: unknown;
  logs: string[];
  error?: string;
}

const logs: string[] = [];

const captureConsole = {
  log: (...args: unknown[]) => {
    logs.push(args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
  error: (...args: unknown[]) => {
    logs.push('ERROR: ' + args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
  warn: (...args: unknown[]) => {
    logs.push('WARN: ' + args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
  info: (...args: unknown[]) => {
    logs.push('INFO: ' + args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  },
};

async function executeCode() {
  try {
    const input = await Bun.stdin.text();
    const request: ExecutionRequest = JSON.parse(input);
    const { code, language } = request;

    logs.length = 0;

    let executableCode = code;
    if (language === 'typescript') {
      executableCode = code
        .replace(/:\s*\w+(\[\])?/g, '')
        .replace(/interface\s+\w+\s*\{[^}]*\}/g, '')
        .replace(/type\s+\w+\s*=\s*[^;]+;/g, '');
    }

    const result = await (async function() {
      'use strict';
      const console = captureConsole;
      const process = undefined;
      const Bun = undefined;
      const Buffer = undefined;
      const require = undefined;
      const global = undefined;
      const globalThis = undefined;
      const setTimeout = undefined;
      const setInterval = undefined;
      const fetch = undefined;
      return eval(executableCode);
    })();

    const response: ExecutionResult = {
      success: true,
      result,
      logs: [...logs],
    };
    console.log(JSON.stringify(response));
  } catch (error: any) {
    const response: ExecutionResult = {
      success: false,
      error: error.message || String(error),
      logs: [...logs],
    };
    console.log(JSON.stringify(response));
  }
}

executeCode().catch(error => {
  const response: ExecutionResult = {
    success: false,
    error: `Fatal error: ${error.message || String(error)}`,
    logs: [],
  };
  console.log(JSON.stringify(response));
  process.exit(1);
});
