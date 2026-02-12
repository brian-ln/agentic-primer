#!/usr/bin/env bun
/**
 * Test suite for ProgramExecutorActor
 *
 * Tests cover:
 * - Basic command execution
 * - stdout/stderr capture
 * - Exit codes
 * - Timeout handling
 * - Process killing
 * - Process status queries
 * - Environment variables
 * - Working directory
 * - Concurrent processes
 * - Input validation
 * - Integration with ask()
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { MessageRouter } from '../router.ts';
import { ProgramExecutorActor } from './program-executor.ts';
import type { Message } from '@agentic-primer/actors';
import { address, createMessage, generateCorrelationId } from '@agentic-primer/actors';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';

describe('ProgramExecutorActor', () => {
  let router: MessageRouter;
  let actor: ProgramExecutorActor;
  let testDir: string;

  beforeEach(() => {
    router = new MessageRouter();
    actor = new ProgramExecutorActor(router, {
      defaultTimeout: 5000,
      maxConcurrentProcesses: 5,
    });

    // Create a temporary test directory
    testDir = path.join(os.tmpdir(), `test-${Date.now()}`);
    fs.mkdirSync(testDir, { recursive: true });
  });

  afterEach(async () => {
    // Clean up running processes
    await actor.cleanup();

    // Clean up test directory
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  // ===== Basic Execution Tests =====

  test('executes simple echo command', async () => {
    const message: Message = {
      id: 'test-1',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'echo',
        args: ['hello', 'world'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
    expect(response.payload.stdout.trim()).toBe('hello world');
    expect(response.payload.processId).toMatch(/^proc_/);
    expect(response.payload.duration).toBeGreaterThan(0);
  });

  test('executes ls command', async () => {
    const message: Message = {
      id: 'test-2',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'ls',
        args: [testDir],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
  });

  test('executes pwd command', async () => {
    const message: Message = {
      id: 'test-3',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'pwd',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
    expect(response.payload.stdout).toContain('/');
  });

  // ===== stdout/stderr Capture Tests =====

  test('captures stdout correctly', async () => {
    const message: Message = {
      id: 'test-4',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'echo',
        args: ['test output'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.stdout.trim()).toBe('test output');
    expect(response.payload.stderr).toBe('');
  });

  test('captures stderr correctly', async () => {
    const message: Message = {
      id: 'test-5',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'ls',
        args: ['/nonexistent-directory-12345'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).not.toBe(0);
    expect(response.payload.stderr).toContain('No such file');
  });

  test('captures multiline output', async () => {
    const message: Message = {
      id: 'test-6',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'ls',
        args: ['-la', '/'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    const lines = response.payload.stdout.trim().split('\n');
    expect(lines.length).toBeGreaterThan(1);
  });

  // ===== Exit Code Tests =====

  test('handles successful exit code (0)', async () => {
    const message: Message = {
      id: 'test-7',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'echo',
        args: ['success'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
  });

  test('handles non-zero exit code', async () => {
    const message: Message = {
      id: 'test-8',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'ls',
        args: ['/nonexistent-directory-12345'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).not.toBe(0);
  });

  // ===== Timeout Tests =====

  test('enforces timeout on long-running command', async () => {
    // Create a Node.js script that loops (easier to kill than bash sleep)
    const scriptPath = path.join(testDir, 'long-running.js');
    fs.writeFileSync(scriptPath, `
      const start = Date.now();
      while (Date.now() - start < 10000) {
        // Busy loop
      }
    `);

    const message: Message = {
      id: 'test-9',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'node',
        args: [scriptPath],
        timeout: 500,
      },
      timestamp: Date.now(),
    };

    const startTime = Date.now();
    const response = await actor.receive(message);
    const elapsed = Date.now() - startTime;

    expect(response.success).toBe(true);
    expect(response.payload.error).toContain('timed out');
    expect(elapsed).toBeGreaterThanOrEqual(500);
    expect(elapsed).toBeLessThan(1500); // Should kill quickly with SIGKILL
  }, 5000);

  test('completes before timeout', async () => {
    const message: Message = {
      id: 'test-10',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'echo',
        args: ['fast'],
        timeout: 5000,
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
    expect(response.payload.error).toBeUndefined();
  });

  // ===== Process Management Tests =====

  test('kills running process', async () => {
    // Create a long-running Node.js script
    const scriptPath = path.join(testDir, 'long-kill.js');
    fs.writeFileSync(scriptPath, `
      const start = Date.now();
      while (Date.now() - start < 30000) {
        // Busy loop
      }
    `);

    // Start a long-running process
    const executeMsg: Message = {
      id: 'test-11a',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'node',
        args: [scriptPath],
        timeout: 60000,
      },
      timestamp: Date.now(),
    };

    // Don't await - let it start
    const executePromise = actor.receive(executeMsg);

    // Give it time to start
    await new Promise(resolve => setTimeout(resolve, 200));

    // Get stats to find the processId
    const stats = actor.getStats();
    expect(stats.runningProcesses).toBe(1);
    const processId = stats.processes[0].processId;

    // Kill it
    const killMsg: Message = {
      id: 'test-11b',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'kill',
      payload: { processId },
      timestamp: Date.now(),
    };

    const killResponse = await actor.receive(killMsg);
    expect(killResponse.success).toBe(true);
    expect(killResponse.payload.killed).toBe(true);

    // Wait for execute to finish
    const executeResponse = await executePromise;
    expect(executeResponse.success).toBe(true);
  }, 10000);

  test('queries process status', async () => {
    // Create a long-running Node.js script
    const scriptPath = path.join(testDir, 'long-status.js');
    fs.writeFileSync(scriptPath, `
      const start = Date.now();
      while (Date.now() - start < 5000) {
        // Busy loop
      }
    `);

    // Start a process
    const executeMsg: Message = {
      id: 'test-12a',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'node',
        args: [scriptPath],
        timeout: 10000,
      },
      timestamp: Date.now(),
    };

    // Start but don't await
    const executePromise = actor.receive(executeMsg);
    await new Promise(resolve => setTimeout(resolve, 200));

    // Get stats
    const stats = actor.getStats();
    const processId = stats.processes[0].processId;

    // Query status
    const statusMsg: Message = {
      id: 'test-12b',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'getStatus',
      payload: { processId },
      timestamp: Date.now(),
    };

    const statusResponse = await actor.receive(statusMsg);
    expect(statusResponse.success).toBe(true);
    expect(statusResponse.payload.running).toBe(true);
    expect(statusResponse.payload.command).toBe('node');
    expect(statusResponse.payload.args).toEqual([scriptPath]);
    expect(statusResponse.payload.pid).toBeGreaterThan(0);

    // Clean up
    await actor.cleanup();
    await executePromise.catch(() => {}); // Might be killed
  }, 10000);

  test('status returns not found for completed process', async () => {
    const message: Message = {
      id: 'test-13',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'getStatus',
      payload: { processId: 'nonexistent' },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.running).toBe(false);
    expect(response.payload.error).toContain('not found');
  });

  // ===== Environment Variable Tests =====

  test('injects environment variables', async () => {
    const message: Message = {
      id: 'test-14',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'env',
        env: {
          TEST_VAR: 'test-value',
          ANOTHER_VAR: 'another-value',
        },
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.stdout).toContain('TEST_VAR=test-value');
    expect(response.payload.stdout).toContain('ANOTHER_VAR=another-value');
  });

  // ===== Working Directory Tests =====

  test('changes working directory', async () => {
    const message: Message = {
      id: 'test-15',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'pwd',
        cwd: testDir,
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    // Use fs.realpathSync to normalize paths (macOS uses /private/var symlink)
    expect(fs.realpathSync(response.payload.stdout.trim())).toBe(fs.realpathSync(testDir));
  });

  test('validates working directory exists', async () => {
    const message: Message = {
      id: 'test-16',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'pwd',
        cwd: '/nonexistent-dir-12345',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('does not exist');
  });

  test('validates working directory is absolute', async () => {
    const message: Message = {
      id: 'test-17',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'pwd',
        cwd: 'relative/path',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('absolute path');
  });

  // ===== Concurrent Process Tests =====

  test('runs multiple processes concurrently', async () => {
    const messages = [1, 2, 3].map(i => ({
      id: `test-18-${i}`,
      pattern: 'ask' as const,
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'echo',
        args: [`message-${i}`],
      },
      timestamp: Date.now(),
    }));

    const responses = await Promise.all(
      messages.map(msg => actor.receive(msg))
    );

    expect(responses).toHaveLength(3);
    responses.forEach((response, i) => {
      expect(response.success).toBe(true);
      expect(response.payload.stdout.trim()).toBe(`message-${i + 1}`);
    });
  });

  test('enforces max concurrent process limit', async () => {
    // Create a long-running Node.js script
    const scriptPath = path.join(testDir, 'long-concurrent.js');
    fs.writeFileSync(scriptPath, `
      const start = Date.now();
      while (Date.now() - start < 5000) {
        // Busy loop
      }
    `);

    // Start 5 long-running processes (the limit)
    const messages = Array.from({ length: 5 }, (_, i) => ({
      id: `test-19-${i}`,
      pattern: 'ask' as const,
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'node',
        args: [scriptPath],
        timeout: 10000,
      },
      timestamp: Date.now(),
    }));

    // Start them all
    const promises = messages.map(msg => actor.receive(msg));
    await new Promise(resolve => setTimeout(resolve, 200));

    // Try to start one more
    const extraMsg: Message = {
      id: 'test-19-extra',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'echo',
        args: ['should-fail'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(extraMsg);
    expect(response.success).toBe(false);
    expect(response.error).toContain('Maximum concurrent processes');

    // Clean up
    await actor.cleanup();
    await Promise.all(promises.map(p => p.catch(() => {})));
  }, 15000);

  // ===== Validation Tests =====

  test('validates command is provided', async () => {
    const message: Message = {
      id: 'test-20',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {},
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Missing required field: command');
  });

  test('validates command is in safe list', async () => {
    const message: Message = {
      id: 'test-21',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'dangerous-command',
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('not in safe list');
  });

  test('handles unknown message type', async () => {
    const message: Message = {
      id: 'test-22',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'unknown' as any,
      payload: {},
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Unknown message type');
  });

  test('validates kill requires processId', async () => {
    const message: Message = {
      id: 'test-23',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'kill',
      payload: {},
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Missing required field: processId');
  });

  test('validates getStatus requires processId', async () => {
    const message: Message = {
      id: 'test-24',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'getStatus',
      payload: {},
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Missing required field: processId');
  });

  // ===== Integration Tests =====

  test('integrates with ask() pattern', async () => {
    // Create a simple router test by manually calling actor
    const message = createMessage(
      address('program-executor'),
      'execute',
      {
        command: 'echo',
        args: ['integration-test'],
      },
      {
        pattern: 'ask',
        from: address('test'),
        correlationId: generateCorrelationId(),
      }
    );

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.stdout.trim()).toBe('integration-test');
  });

  test('handles command with no output', async () => {
    const message: Message = {
      id: 'test-26',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'test',
        args: ['-d', testDir],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
    expect(response.payload.stdout).toBe('');
    expect(response.payload.stderr).toBe('');
  });

  test('getStats returns accurate information', async () => {
    const stats1 = actor.getStats();
    expect(stats1.runningProcesses).toBe(0);
    expect(stats1.maxConcurrent).toBe(5);

    // Create a long-running Node.js script
    const scriptPath = path.join(testDir, 'long-stats.js');
    fs.writeFileSync(scriptPath, `
      const start = Date.now();
      while (Date.now() - start < 2000) {
        // Busy loop
      }
    `);

    // Start a process
    const executeMsg: Message = {
      id: 'test-27',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'node',
        args: [scriptPath],
      },
      timestamp: Date.now(),
    };

    const promise = actor.receive(executeMsg);
    await new Promise(resolve => setTimeout(resolve, 200));

    const stats2 = actor.getStats();
    expect(stats2.runningProcesses).toBe(1);
    expect(stats2.processes[0].command).toBe('node');

    await actor.cleanup();
    await promise.catch(() => {});
  }, 10000);

  test('cleanup kills all running processes', async () => {
    // Create a long-running Node.js script
    const scriptPath = path.join(testDir, 'long-cleanup.js');
    fs.writeFileSync(scriptPath, `
      const start = Date.now();
      while (Date.now() - start < 30000) {
        // Busy loop
      }
    `);

    // Start multiple processes
    const messages = Array.from({ length: 3 }, (_, i) => ({
      id: `test-28-${i}`,
      pattern: 'ask' as const,
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'node',
        args: [scriptPath],
      },
      timestamp: Date.now(),
    }));

    const promises = messages.map(msg => actor.receive(msg));
    await new Promise(resolve => setTimeout(resolve, 200));

    const stats1 = actor.getStats();
    expect(stats1.runningProcesses).toBe(3);

    await actor.cleanup();

    const stats2 = actor.getStats();
    expect(stats2.runningProcesses).toBe(0);

    await Promise.all(promises.map(p => p.catch(() => {})));
  }, 10000);

  test('handles error in command execution', async () => {
    const message: Message = {
      id: 'test-29',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'cat',
        args: ['/nonexistent-file-12345.txt'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).not.toBe(0);
    expect(response.payload.stderr).toContain('No such file');
  });

  test('executes git command with args', async () => {
    const message: Message = {
      id: 'test-30',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'git',
        args: ['--version'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
    expect(response.payload.stdout).toContain('git version');
  });

  test('executes bun command', async () => {
    const message: Message = {
      id: 'test-31',
      pattern: 'ask',
      from: address('test'),
      to: address('program-executor'),
      type: 'execute',
      payload: {
        command: 'bun',
        args: ['--version'],
      },
      timestamp: Date.now(),
    };

    const response = await actor.receive(message);

    expect(response.success).toBe(true);
    expect(response.payload.exitCode).toBe(0);
    expect(response.payload.stdout.trim().length).toBeGreaterThan(0);
  });
});
