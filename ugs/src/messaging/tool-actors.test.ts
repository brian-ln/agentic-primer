#!/usr/bin/env bun
/**
 * Tool Actors Tests
 * Tests for src/messaging/actors/tool.ts and filesystem.ts
 * Target: >80% coverage
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import { BashToolActor, ReadToolActor, WriteToolActor } from './actors/tool.ts';
import { FileSystemActor } from './actors/filesystem.ts';
import { MessageRouter } from './router.ts';
import {
  address,
  createMessage,
  generateCorrelationId,
} from '@agentic-primer/actors';
import { readFile, writeFile, unlink, mkdir } from 'fs/promises';
import { existsSync } from 'fs';
import { join } from 'path';
import type GraphStore from '@src/graph.ts';
import type { ProgramManager } from '@src/entities/program.ts';

// Mock implementations
function createMockGraphStore(): GraphStore {
  return {
    get: () => null,
    set: () => {},
  } as any as GraphStore;
}

function createMockProgramManager(): ProgramManager {
  return {} as any as ProgramManager;
}

// Test temp directory
const TEST_DIR = join(__dirname, '.test-tmp');

describe('BashToolActor', () => {
  let router: MessageRouter;
  let actor: BashToolActor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new BashToolActor(router);
  });

  test('creates with @(tool-bash) address', () => {
    expect(actor.address).toBe('@(tool-bash)');
  });

  test('executes simple bash command', async () => {
    const msg = createMessage(address('tool-bash'), 'execute', {
      command: 'echo "hello world"',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);
    expect(response.payload?.stdout).toContain('hello world');
  });

  test('captures stdout', async () => {
    const msg = createMessage(address('tool-bash'), 'execute', {
      command: 'echo "test output"',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.payload?.stdout?.trim()).toBe('test output');
  });

  test('captures stderr on error', async () => {
    const msg = createMessage(address('tool-bash'), 'execute', {
      command: 'echo "error message" >&2 && exit 1',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
    expect(response.payload?.stderr).toContain('error message');
    expect(response.payload?.exitCode).toBe(1);
  });

  test('respects timeout', async () => {
    const msg = createMessage(address('tool-bash'), 'execute', {
      command: 'sleep 5',
      timeout: 100,
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
    expect(response.error).toMatch(/timeout|killed|failed|Command failed/i);
  }, 1000);

  test('handles command not found', async () => {
    const msg = createMessage(address('tool-bash'), 'execute', {
      command: 'nonexistentcommand12345',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
  });
});

describe('ReadToolActor', () => {
  let router: MessageRouter;
  let actor: ReadToolActor;
  const testFile = join(TEST_DIR, 'test-read.txt');

  beforeEach(async () => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new ReadToolActor(router);

    // Create test file
    await mkdir(TEST_DIR, { recursive: true });
    await writeFile(testFile, 'test content\nline 2\nline 3');
  });

  afterEach(async () => {
    try {
      if (existsSync(testFile)) await unlink(testFile);
    } catch {}
  });

  test('creates with @(tool-read) address', () => {
    expect(actor.address).toBe('@(tool-read)');
  });

  test('reads file content', async () => {
    const msg = createMessage(address('tool-read'), 'read', {
      path: testFile,
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);
    expect(response.payload?.content).toContain('test content');
  });

  test('returns file size', async () => {
    const msg = createMessage(address('tool-read'), 'read', {
      path: testFile,
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.payload?.size).toBeGreaterThan(0);
  });

  test('handles file not found', async () => {
    const msg = createMessage(address('tool-read'), 'read', {
      path: '/nonexistent/file.txt',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
    expect(response.error).toMatch(/not found|ENOENT/i);
  });
});

describe('WriteToolActor', () => {
  let router: MessageRouter;
  let actor: WriteToolActor;
  const testFile = join(TEST_DIR, 'test-write.txt');

  beforeEach(async () => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new WriteToolActor(router);
    await mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    try {
      if (existsSync(testFile)) await unlink(testFile);
    } catch {}
  });

  test('creates with @(tool-write) address', () => {
    expect(actor.address).toBe('@(tool-write)');
  });

  test('writes content to file', async () => {
    const content = 'Hello, World!\nLine 2';
    const msg = createMessage(address('tool-write'), 'write', {
      path: testFile,
      content,
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);

    // Verify file was written
    const written = await readFile(testFile, 'utf-8');
    expect(written).toBe(content);
  });

  test('returns file size', async () => {
    const msg = createMessage(address('tool-write'), 'write', {
      path: testFile,
      content: 'test content',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.payload?.size).toBeGreaterThan(0);
  });

  test('overwrites existing file', async () => {
    await writeFile(testFile, 'old content');

    const msg = createMessage(address('tool-write'), 'write', {
      path: testFile,
      content: 'new content',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);

    const written = await readFile(testFile, 'utf-8');
    expect(written).toBe('new content');
  });

  test('handles write errors gracefully', async () => {
    const msg = createMessage(address('tool-write'), 'write', {
      path: '/root/forbidden.txt',
      content: 'should fail',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
  });
});

describe('FileSystemActor', () => {
  let router: MessageRouter;
  let actor: FileSystemActor;
  const testFile = join(TEST_DIR, 'fs-test.txt');

  beforeEach(async () => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    actor = new FileSystemActor(router, TEST_DIR);
    await mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    try {
      if (existsSync(testFile)) await unlink(testFile);
    } catch {}
  });

  test('creates with basePath constraint', () => {
    expect((actor as any).basePath).toBe(TEST_DIR);
  });

  test('prevents directory traversal', async () => {
    const msg = createMessage(address('filesystem'), 'read_file', {
      path: '../../etc/passwd',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
    expect(response.error).toMatch(/outside|Path outside|forbidden/i);
  });

  test('allows reading within basePath', async () => {
    await writeFile(testFile, 'allowed content');

    const msg = createMessage(address('filesystem'), 'read_file', {
      path: 'fs-test.txt',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);
    expect(response.payload?.content).toBe('allowed content');
  });

  test('lists directory contents', async () => {
    await writeFile(join(TEST_DIR, 'file1.txt'), 'test');
    await writeFile(join(TEST_DIR, 'file2.txt'), 'test');

    const msg = createMessage(address('filesystem'), 'list_dir', {
      path: '.',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);
    expect(Array.isArray(response.payload?.entries)).toBe(true);
    const fileNames = response.payload?.entries.map((f: any) => f.name);
    expect(fileNames).toContain('file1.txt');
    expect(fileNames).toContain('file2.txt');
  });

  test('deletes file within basePath', async () => {
    await writeFile(testFile, 'to delete');

    const msg = createMessage(address('filesystem'), 'delete_file', {
      path: 'fs-test.txt',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);
    expect(existsSync(testFile)).toBe(false);
  });

  test('prevents deleting outside basePath', async () => {
    const msg = createMessage(address('filesystem'), 'delete_file', {
      path: '../../important-file.txt',
    }, {
      from: address('test'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
  });
});
