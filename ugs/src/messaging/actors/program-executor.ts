#!/usr/bin/env bun
/**
 * ProgramExecutorActor - Execute shell commands and scripts safely
 *
 * Provides a message-based interface for executing external programs.
 * Supports process management, timeouts, and environment control.
 *
 * Message types:
 * - execute: Run a command with args
 * - kill: Terminate a running process
 * - getStatus: Check process status
 *
 * Examples:
 *   @(program-executor).ask('execute', { command: 'echo', args: ['hello'] })
 *   @(program-executor).ask('kill', { processId: 'proc_123' })
 */

import { Actor } from '../actor.ts';
import type { MessageRouter } from '../router.ts';
import type { Message, MessageResponse } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { spawn, type ChildProcess, type SpawnOptions } from 'child_process';
import { promisify } from 'util';
import { randomUUID } from 'crypto';
import * as path from 'path';
import * as fs from 'fs';

/**
 * Execution result structure
 */
export interface ExecutionResult {
  processId: string;
  exitCode: number;
  stdout: string;
  stderr: string;
  duration: number;
  error?: string;
}

/**
 * Execute message payload
 */
export interface ExecutePayload {
  command: string;
  args?: string[];
  cwd?: string;
  env?: Record<string, string>;
  timeout?: number;
}

/**
 * Process status
 */
export interface ProcessStatus {
  processId: string;
  pid?: number;
  running: boolean;
  startTime: number;
  endTime?: number;
  command: string;
  args: string[];
}

/**
 * Running process tracking
 */
interface RunningProcess {
  processId: string;
  process: ChildProcess;
  command: string;
  args: string[];
  startTime: number;
  timeoutHandle?: NodeJS.Timeout;
}

/**
 * ProgramExecutorActor - Manages external process execution
 */
export class ProgramExecutorActor extends Actor {
  private processes = new Map<string, RunningProcess>();
  private maxConcurrentProcesses: number;
  private defaultTimeout: number;
  private safeCommands: Set<string>;

  constructor(
    router: MessageRouter,
    options: {
      maxConcurrentProcesses?: number;
      defaultTimeout?: number;
      safeCommands?: string[];
    } = {}
  ) {
    super('program-executor', router);
    this.maxConcurrentProcesses = options.maxConcurrentProcesses ?? 10;
    this.defaultTimeout = options.defaultTimeout ?? 30000; // 30 seconds default

    // Whitelist of safe commands (can be expanded)
    this.safeCommands = new Set(options.safeCommands ?? [
      'echo', 'ls', 'pwd', 'date', 'whoami',
      'cat', 'head', 'tail', 'grep', 'find',
      'node', 'bun', 'npm', 'git',
      'mkdir', 'rm', 'cp', 'mv', 'touch',
      'which', 'env', 'test',
    ]);
  }

  /**
   * Handle incoming messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'execute':
          return await this.handleExecute(message, payload);

        case 'kill':
          return await this.handleKill(message, payload);

        case 'getStatus':
          return await this.handleGetStatus(message, payload);

        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message);
    }
  }

  /**
   * Execute a command
   */
  private async handleExecute(message: Message, payload: ExecutePayload): Promise<MessageResponse> {
    const { command, args = [], cwd, env, timeout } = payload;

    // Validation
    if (!command) {
      return createErrorResponse(message, 'Missing required field: command');
    }

    // Check concurrent process limit
    if (this.processes.size >= this.maxConcurrentProcesses) {
      return createErrorResponse(message,
        `Maximum concurrent processes (${this.maxConcurrentProcesses}) reached`
      );
    }

    // Validate command (basic security check)
    // Allow absolute paths to scripts, or commands in safe list
    const baseCommand = path.basename(command);
    const isAbsolutePath = path.isAbsolute(command);
    const isScriptFile = isAbsolutePath && (command.endsWith('.sh') || command.endsWith('.js') || command.endsWith('.ts'));

    if (!isScriptFile && !this.safeCommands.has(baseCommand) && !this.safeCommands.has(command)) {
      return createErrorResponse(message,
        `Command not in safe list: ${baseCommand}. Add to safeCommands if trusted.`
      );
    }

    // Validate working directory
    if (cwd) {
      if (!path.isAbsolute(cwd)) {
        return createErrorResponse(message, 'Working directory must be an absolute path');
      }
      if (!fs.existsSync(cwd)) {
        return createErrorResponse(message, `Working directory does not exist: ${cwd}`);
      }
    }

    // Execute the command
    const processId = `proc_${randomUUID()}`;
    const result = await this.executeCommand(processId, command, args, {
      cwd,
      env: env ? { ...process.env, ...env } : undefined,
      timeout: timeout ?? this.defaultTimeout,
    });

    return createResponse(message, result);
  }

  /**
   * Execute a command and track it
   */
  private executeCommand(
    processId: string,
    command: string,
    args: string[],
    options: {
      cwd?: string;
      env?: Record<string, string>;
      timeout: number;
    }
  ): Promise<ExecutionResult> {
    return new Promise((resolve) => {
      const startTime = Date.now();
      let stdout = '';
      let stderr = '';
      let timedOut = false;

      // Spawn options
      const spawnOptions: SpawnOptions = {
        cwd: options.cwd,
        env: options.env,
        stdio: ['pipe', 'pipe', 'pipe'],
      };

      // Spawn the process
      const childProcess = spawn(command, args, spawnOptions);

      // Track the process
      const runningProcess: RunningProcess = {
        processId,
        process: childProcess,
        command,
        args,
        startTime,
      };
      this.processes.set(processId, runningProcess);

      // Set up timeout
      const timeoutHandle = setTimeout(() => {
        timedOut = true;
        childProcess.kill('SIGKILL'); // Use SIGKILL directly for reliable timeout
      }, options.timeout);

      runningProcess.timeoutHandle = timeoutHandle;

      // Capture stdout
      if (childProcess.stdout) {
        childProcess.stdout.on('data', (data) => {
          stdout += data.toString();
        });
      }

      // Capture stderr
      if (childProcess.stderr) {
        childProcess.stderr.on('data', (data) => {
          stderr += data.toString();
        });
      }

      // Handle process completion
      childProcess.on('close', (code, signal) => {
        clearTimeout(timeoutHandle);
        this.processes.delete(processId);

        const duration = Date.now() - startTime;
        const exitCode = code ?? -1;

        const result: ExecutionResult = {
          processId,
          exitCode,
          stdout,
          stderr,
          duration,
        };

        if (timedOut) {
          result.error = `Process timed out after ${options.timeout}ms`;
        } else if (signal) {
          result.error = `Process terminated by signal: ${signal}`;
        }

        resolve(result);
      });

      // Handle errors
      childProcess.on('error', (error) => {
        clearTimeout(timeoutHandle);
        this.processes.delete(processId);

        const duration = Date.now() - startTime;
        resolve({
          processId,
          exitCode: -1,
          stdout,
          stderr,
          duration,
          error: error.message,
        });
      });
    });
  }

  /**
   * Kill a running process
   */
  private async handleKill(message: Message, payload: any): Promise<MessageResponse> {
    const { processId } = payload;

    if (!processId) {
      return createErrorResponse(message, 'Missing required field: processId');
    }

    const runningProcess = this.processes.get(processId);
    if (!runningProcess) {
      return createErrorResponse(message, `Process not found: ${processId}`);
    }

    // Clear timeout if set
    if (runningProcess.timeoutHandle) {
      clearTimeout(runningProcess.timeoutHandle);
    }

    // Kill the process immediately with SIGKILL
    runningProcess.process.kill('SIGKILL');

    return createResponse(message, {
      processId,
      killed: true,
    });
  }

  /**
   * Get process status
   */
  private async handleGetStatus(message: Message, payload: any): Promise<MessageResponse> {
    const { processId } = payload;

    if (!processId) {
      return createErrorResponse(message, 'Missing required field: processId');
    }

    const runningProcess = this.processes.get(processId);

    if (!runningProcess) {
      return createResponse(message, {
        processId,
        running: false,
        error: 'Process not found or already completed',
      });
    }

    const status: ProcessStatus = {
      processId,
      pid: runningProcess.process.pid,
      running: true,
      startTime: runningProcess.startTime,
      command: runningProcess.command,
      args: runningProcess.args,
    };

    return createResponse(message, status);
  }

  /**
   * Clean up all running processes (call on shutdown)
   */
  async cleanup(): Promise<void> {
    const killPromises: Promise<void>[] = [];

    for (const [processId, runningProcess] of this.processes.entries()) {
      killPromises.push(
        new Promise<void>((resolve) => {
          if (runningProcess.timeoutHandle) {
            clearTimeout(runningProcess.timeoutHandle);
          }

          // Use SIGKILL for immediate cleanup
          runningProcess.process.on('close', () => resolve());
          runningProcess.process.kill('SIGKILL');

          // Safety timeout in case close never fires
          setTimeout(() => resolve(), 500);
        })
      );
    }

    await Promise.all(killPromises);
    this.processes.clear();
  }

  /**
   * Get actor statistics
   */
  getStats() {
    return {
      runningProcesses: this.processes.size,
      maxConcurrent: this.maxConcurrentProcesses,
      processes: Array.from(this.processes.values()).map(p => ({
        processId: p.processId,
        pid: p.process.pid,
        command: p.command,
        args: p.args,
        startTime: p.startTime,
        duration: Date.now() - p.startTime,
      })),
    };
  }
}

export default ProgramExecutorActor;
