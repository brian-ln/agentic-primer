#!/usr/bin/env bun
/**
 * UnsafeCodeComputeActor - Fast but insecure code execution
 *
 * ⚠️  CRITICAL WARNING: This actor has KNOWN SECURITY VULNERABILITIES
 * ⚠️  DO NOT USE WITH UNTRUSTED CODE - sandbox can be completely bypassed
 * ⚠️  See SECURITY_FINDINGS.md for details (CE-001, CE-002, CE-003, CE-004)
 *
 * Known Vulnerabilities:
 * - CE-001: Constructor chain escapes (process access via async function constructor)
 * - CE-002: Function.constructor escapes (global scope access)
 * - CE-003: Prototype pollution persists across executions
 * - CE-004: Global scope via 'this' in non-strict mode
 * - Synchronous infinite loops cannot be interrupted
 *
 * Use Cases:
 * - Demo/testing with trusted code only
 * - Internal tools where code source is controlled
 *
 * For Production with Untrusted Code:
 * - Use WorkerCodeExecutionActor (good isolation, fast)
 * - Use SubprocessCodeExecutionActor (best isolation, secure)
 *
 * Features:
 * - Fast execution (~0.1ms overhead)
 * - No file system access (blocked)
 * - No network access (blocked)
 * - No process access (blocked via sandbox, but escapable via constructors)
 * - Configurable timeout limits (async only)
 * - Output capture
 * - Sanitized error messages (CE-005 fixed)
 */

import { MessageRouter } from '../../router.ts';
import { Actor } from '../../actor.ts';
import type { Message, MessageResponse } from '@agentic-primer/actors';

interface ExecutionResult {
  output: any;
  logs: string[];
  executionTime: number;
}

/**
 * Static analysis mode for optional security checks
 */
type StaticAnalysisMode = 'off' | 'warn' | 'strict';

/**
 * Configuration for UnsafeCodeComputeActor
 */
interface UnsafeCodeExecutionConfig {
  /** Timeout in milliseconds (default: 5000) */
  timeout?: number;

  /**
   * REQUIRED: Explicit acknowledgment of security risks
   * Must be true to construct this actor
   */
  iUnderstandThisIsUnsafe: boolean;

  /**
   * Optional static analysis mode (default: 'off')
   * - 'off': No checks (fastest, ~0.1ms)
   * - 'warn': Log suspicious patterns (~0.2ms)
   * - 'strict': Block dangerous patterns (~0.2ms)
   */
  staticAnalysis?: StaticAnalysisMode;
}

/**
 * Result of static code analysis
 */
interface AnalysisResult {
  safe: boolean;
  violations: Array<{
    pattern: string;
    risk: string;
    line?: number;
  }>;
}

/**
 * UnsafeCodeComputeActor - Executes code with minimal overhead but NO security
 */
export class UnsafeCodeComputeActor extends Actor {
  private defaultTimeout: number;
  private logs: string[];
  private staticAnalysisMode: StaticAnalysisMode;

  constructor(router: MessageRouter, config: UnsafeCodeExecutionConfig) {
    super('code-execution', router);

    // REQUIRED: Explicit acknowledgment of risks
    if (!config.iUnderstandThisIsUnsafe) {
      throw new Error(
        'UnsafeCodeComputeActor requires explicit acknowledgment of security risks.\n' +
        'Set iUnderstandThisIsUnsafe: true in config.\n\n' +
        'For production code with untrusted input, use:\n' +
        '  - WorkerCodeExecutionActor (secure, ~10-50ms)\n' +
        '  - SubprocessCodeExecutionActor (maximum security, ~10ms, kills infinite loops)\n\n' +
        'UnsafeCodeComputeActor should ONLY be used with:\n' +
        '  - Trusted code from known sources\n' +
        '  - Internal tools and demos\n' +
        '  - Already-sandboxed environments\n' +
        '  - High-frequency executions where speed is critical'
      );
    }

    this.defaultTimeout = config.timeout ?? 5000;
    this.staticAnalysisMode = config.staticAnalysis ?? 'off';
    this.logs = [];
  }

  /**
   * Capture console output
   */
  private captureOutput(...args: any[]): void {
    this.logs.push(args.map(arg =>
      typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
    ).join(' '));
  }

  /**
   * Sanitize error messages to prevent information disclosure (CE-005 fix)
   * Removes file system paths and internal implementation details
   */
  private sanitizeError(error: Error): string {
    let message = error.message;

    // Strip file:// URLs and absolute paths
    message = message.replace(/file:\/\/\/[^\s)]+/g, '[path]');
    message = message.replace(/\/[^\s:)]+\/[^\s:)]+/g, '[path]');

    // Remove line/column references that leak internal structure
    message = message.replace(/:\d+:\d+/g, '');

    return message;
  }

  /**
   * Analyze code for dangerous patterns (optional defense-in-depth)
   *
   * This is NOT a complete security solution - it can be bypassed.
   * It catches obvious exploits and provides warnings.
   *
   * For real security, use WorkerCodeExecutionActor or SubprocessCodeExecutionActor.
   */
  private analyzeCode(code: string): AnalysisResult {
    const violations: AnalysisResult['violations'] = [];

    // Dangerous patterns that enable known vulnerabilities
    const patterns = [
      {
        regex: /\.constructor(?!\s*\()/gi, // .constructor but not constructor calls
        pattern: '.constructor',
        risk: 'Constructor chain escape (CE-001)',
      },
      {
        regex: /Function\s*\(/gi,
        pattern: 'Function(',
        risk: 'Function constructor escape (CE-002)',
      },
      {
        regex: /Object\.prototype\./gi,
        pattern: 'Object.prototype.',
        risk: 'Prototype pollution (CE-003)',
      },
      {
        regex: /\bglobalThis\b/gi,
        pattern: 'globalThis',
        risk: 'Global scope access (CE-004)',
      },
      {
        regex: /\beval\s*\(/gi,
        pattern: 'eval(',
        risk: 'Dynamic code evaluation',
      },
      {
        regex: /\b(async\s+)?\s*function\s*\(\s*\)\s*\{\s*\}\.constructor/gi,
        pattern: '(function(){}).constructor',
        risk: 'Function constructor access via prototype',
      },
      {
        regex: /\bprocess\./gi,
        pattern: 'process.',
        risk: 'Process object access attempt',
      },
      {
        regex: /\bBun\./gi,
        pattern: 'Bun.',
        risk: 'Bun runtime access attempt',
      },
    ];

    const lines = code.split('\n');

    for (const { regex, pattern, risk } of patterns) {
      // Reset regex
      regex.lastIndex = 0;

      if (regex.test(code)) {
        // Find line number
        let lineNum: number | undefined;
        for (let i = 0; i < lines.length; i++) {
          if (new RegExp(regex.source, regex.flags).test(lines[i])) {
            lineNum = i + 1;
            break;
          }
        }

        violations.push({
          pattern,
          risk,
          line: lineNum,
        });
      }
    }

    return {
      safe: violations.length === 0,
      violations,
    };
  }

  /**
   * Create a timeout promise
   */
  private timeoutPromise(timeout: number): Promise<never> {
    return new Promise((_, reject) => {
      setTimeout(() => {
        reject(new Error(`Execution timed out after ${timeout}ms`));
      }, timeout);
    });
  }

  /**
   * Execute code in a sandboxed environment
   */
  private async executeInSandbox(code: string, language: string, timeout: number): Promise<any> {
    // Reset logs for this execution
    this.logs = [];

    // Optional static analysis (defense-in-depth)
    if (this.staticAnalysisMode !== 'off') {
      const analysis = this.analyzeCode(code);

      if (!analysis.safe) {
        const violationSummary = analysis.violations
          .map(v => `  - Line ${v.line ?? '?'}: ${v.pattern} (${v.risk})`)
          .join('\n');

        const message = `Static analysis detected dangerous patterns:\n${violationSummary}`;

        if (this.staticAnalysisMode === 'strict') {
          // Block execution
          throw new Error(
            `Code execution blocked by static analysis.\n\n${message}\n\n` +
            'To execute this code:\n' +
            '  - Remove dangerous patterns, OR\n' +
            '  - Set staticAnalysis: "off" (not recommended), OR\n' +
            '  - Use WorkerCodeExecutionActor or SubprocessCodeExecutionActor for safe execution'
          );
        } else if (this.staticAnalysisMode === 'warn') {
          // Log warning but continue
          console.warn(`⚠️  ${message}`);
          this.logs.push(`SECURITY WARNING: ${message}`);
        }
      }
    }

    // Create sandbox with limited globals
    const sandbox = {
      console: {
        log: (...args: any[]) => this.captureOutput(...args),
        error: (...args: any[]) => this.captureOutput('ERROR:', ...args),
        warn: (...args: any[]) => this.captureOutput('WARN:', ...args),
        info: (...args: any[]) => this.captureOutput('INFO:', ...args),
      },
      Math,
      Date,
      JSON,
      Array,
      Object,
      String,
      Number,
      Boolean,
      Promise,
      setTimeout: undefined,  // Disable async operations that could bypass timeout
      setInterval: undefined,
      fetch: undefined,       // Disable network
      require: undefined,     // Disable module loading
      import: undefined,      // Disable dynamic imports
      process: undefined,     // Disable process access
      Bun: undefined,        // Disable Bun APIs
      Buffer: undefined,     // Disable Buffer
      global: undefined,     // Disable global access
      globalThis: undefined, // Disable globalThis access
    };

    // For TypeScript, transpile to JavaScript first
    let executableCode = code;
    if (language === 'typescript') {
      // Simple TypeScript handling - Bun can execute TS directly
      // but we'll strip type annotations for safety
      executableCode = code
        .replace(/:\s*\w+(\[\])?/g, '')  // Remove type annotations
        .replace(/interface\s+\w+\s*\{[^}]*\}/g, '') // Remove interfaces
        .replace(/type\s+\w+\s*=\s*[^;]+;/g, '');    // Remove type aliases
    }

    // NOTE: Synchronous infinite loops cannot be interrupted in JavaScript
    // The timeout only works for async operations and finite synchronous code
    // For production use, consider using a separate Worker thread or process

    // Execute in sandbox using with statement (controlled scope)
    // Note: Cannot use strict mode as it disallows 'with' statements
    // This means CE-001 through CE-004 vulnerabilities remain exploitable
    try {
      const wrappedCode = `
        with(sandbox) {
          return (function() {
            ${executableCode}
          })();
        }
      `;
      const fn = new Function('sandbox', wrappedCode);
      return fn(sandbox);
    } catch (error: any) {
      // Sanitize error message to prevent path disclosure (CE-005 fix)
      throw new Error(`Execution error: ${this.sanitizeError(error)}`);
    }
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

      // Execute with timeout protection (only works for async operations)
      const startTime = Date.now();
      const result = await Promise.race([
        this.executeInSandbox(code, language, timeout),
        this.timeoutPromise(timeout),
      ]);
      const executionTime = Date.now() - startTime;

      return {
        id: message.id + '_response',
        correlationId: message.correlationId || message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: {
          result,
          logs: this.logs,
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
        error: this.sanitizeError(error), // CE-005 fix: Sanitize all error messages
        payload: {
          logs: this.logs,
        },
        timestamp: Date.now(),
      };
    }
  }
}
