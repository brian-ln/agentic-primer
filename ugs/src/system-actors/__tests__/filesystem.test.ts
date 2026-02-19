#!/usr/bin/env bun
import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { FileSystemActor } from '../filesystem.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { Actor } from '../../messaging/actor.ts';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import { address, createMessage } from '@agentic-primer/actors';
import type { Message, MessageResponse } from '@agentic-primer/actors';
import * as fs from 'node:fs/promises';
import { mkdtemp } from 'node:fs/promises';
import * as path from 'node:path';
import { tmpdir } from 'node:os';
import { existsSync } from 'node:fs';

describe('FileSystemActor', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let fsActor: FileSystemActor;
  let testDir: string;
  let allowedDir: string;
  let restrictedDir: string;

  beforeEach(async () => {
    testDir = await mkdtemp(path.join(tmpdir(), 'ugs-filesystem-'));
    allowedDir = path.join(testDir, 'allowed');
    restrictedDir = path.join(testDir, 'restricted');

    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    // Create test directories
    await fs.mkdir(allowedDir, { recursive: true });
    await fs.mkdir(restrictedDir, { recursive: true });

    // Create filesystem actor with limited access
    fsActor = new FileSystemActor('fs', router, {
      allowedPaths: [allowedDir],
      operations: ['read', 'write', 'delete'],
    });

    router.registerActor('/system/fs', fsActor);
  });

  afterEach(async () => {
    // Cleanup watchers
    await fsActor.shutdown();

    // Cleanup test directories
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch (e) {
      // Ignore if directory doesn't exist
    }
  });

  describe('fs.read', () => {
    test('should read file from allowed path', async () => {
      const filePath = path.join(allowedDir, 'test.txt');
      await fs.writeFile(filePath, 'Hello, World!');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.content).toBe('Hello, World!');
    });

    test('should read file with utf-8 encoding', async () => {
      const filePath = path.join(allowedDir, 'utf8.txt');
      await fs.writeFile(filePath, '🚀 Unicode test');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: filePath, encoding: 'utf-8' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.content).toBe('🚀 Unicode test');
    });

    test('should reject read from restricted path', async () => {
      const filePath = path.join(restrictedDir, 'secret.txt');
      await fs.writeFile(filePath, 'Secret data');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
      expect(response.error).toContain(restrictedDir);
    });

    test('should reject read when operation not permitted', async () => {
      const writeOnlyFs = new FileSystemActor('write-only', router, {
        allowedPaths: [allowedDir],
        operations: ['write'], // No read permission
      });

      const filePath = path.join(allowedDir, 'test.txt');
      await fs.writeFile(filePath, 'Test');

      const response = await writeOnlyFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Read operation not permitted');
    });

    test('should reject path traversal attacks', async () => {
      const filePath = path.join(allowedDir, '../restricted/secret.txt');
      await fs.writeFile(path.join(restrictedDir, 'secret.txt'), 'Secret');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
    });

    test('should handle file not found error', async () => {
      const filePath = path.join(allowedDir, 'nonexistent.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toBeDefined();
    });
  });

  describe('fs.write', () => {
    test('should write file to allowed path', async () => {
      const filePath = path.join(allowedDir, 'output.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.write',
          { path: filePath, content: 'Test content' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.written).toBe(true);

      // Verify file was written
      const content = await fs.readFile(filePath, 'utf-8');
      expect(content).toBe('Test content');
    });

    test('should write file atomically by default', async () => {
      const filePath = path.join(allowedDir, 'atomic.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.write',
          { path: filePath, content: 'Atomic write' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);

      const content = await fs.readFile(filePath, 'utf-8');
      expect(content).toBe('Atomic write');

      // Verify temp file was cleaned up
      expect(existsSync(`${filePath}.tmp`)).toBe(false);
    });

    test('should write file non-atomically when specified', async () => {
      const filePath = path.join(allowedDir, 'non-atomic.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.write',
          { path: filePath, content: 'Non-atomic write', atomic: false },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);

      const content = await fs.readFile(filePath, 'utf-8');
      expect(content).toBe('Non-atomic write');
    });

    test('should create parent directories if needed', async () => {
      const filePath = path.join(allowedDir, 'deep', 'nested', 'file.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.write',
          { path: filePath, content: 'Nested content' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);

      const content = await fs.readFile(filePath, 'utf-8');
      expect(content).toBe('Nested content');
    });

    test('should reject write to restricted path', async () => {
      const filePath = path.join(restrictedDir, 'output.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.write',
          { path: filePath, content: 'Test' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
    });

    test('should reject write when operation not permitted', async () => {
      const readOnlyFs = new FileSystemActor('read-only', router, {
        allowedPaths: [allowedDir],
        operations: ['read'], // No write permission
      });

      const filePath = path.join(allowedDir, 'output.txt');

      const response = await readOnlyFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.write',
          { path: filePath, content: 'Test' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Write operation not permitted');
    });

    test('should overwrite existing file', async () => {
      const filePath = path.join(allowedDir, 'overwrite.txt');
      await fs.writeFile(filePath, 'Original content');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.write',
          { path: filePath, content: 'New content' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);

      const content = await fs.readFile(filePath, 'utf-8');
      expect(content).toBe('New content');
    });
  });

  describe('fs.append', () => {
    test('should append to existing file', async () => {
      const filePath = path.join(allowedDir, 'append.txt');
      await fs.writeFile(filePath, 'Line 1\n');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.append',
          { path: filePath, content: 'Line 2\n' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.appended).toBe(true);

      const content = await fs.readFile(filePath, 'utf-8');
      expect(content).toBe('Line 1\nLine 2\n');
    });

    test('should create file if it does not exist', async () => {
      const filePath = path.join(allowedDir, 'new-append.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.append',
          { path: filePath, content: 'First line\n' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);

      const content = await fs.readFile(filePath, 'utf-8');
      expect(content).toBe('First line\n');
    });

    test('should reject append to restricted path', async () => {
      const filePath = path.join(restrictedDir, 'append.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.append',
          { path: filePath, content: 'Test' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
    });
  });

  describe('fs.delete', () => {
    test('should delete file from allowed path', async () => {
      const filePath = path.join(allowedDir, 'delete-me.txt');
      await fs.writeFile(filePath, 'Delete this');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.delete',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.deleted).toBe(true);

      // Verify file was deleted
      expect(existsSync(filePath)).toBe(false);
    });

    test('should reject delete from restricted path', async () => {
      const filePath = path.join(restrictedDir, 'protected.txt');
      await fs.writeFile(filePath, 'Protected');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.delete',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');

      // Verify file was not deleted
      expect(existsSync(filePath)).toBe(true);
    });

    test('should reject delete when operation not permitted', async () => {
      const noDeleteFs = new FileSystemActor('no-delete', router, {
        allowedPaths: [allowedDir],
        operations: ['read', 'write'], // No delete permission
      });

      const filePath = path.join(allowedDir, 'file.txt');
      await fs.writeFile(filePath, 'Test');

      const response = await noDeleteFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.delete',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Delete operation not permitted');
    });

    test('should handle file not found error on delete', async () => {
      const filePath = path.join(allowedDir, 'nonexistent.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.delete',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toBeDefined();
    });
  });

  describe('fs.list', () => {
    test('should list files in directory', async () => {
      await fs.writeFile(path.join(allowedDir, 'file1.txt'), 'Test 1');
      await fs.writeFile(path.join(allowedDir, 'file2.txt'), 'Test 2');
      await fs.mkdir(path.join(allowedDir, 'subdir'));

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.list',
          { path: allowedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.files).toContain('file1.txt');
      expect(response.payload?.files).toContain('file2.txt');
      expect(response.payload?.files).toContain('subdir');
    });

    test('should filter files by pattern', async () => {
      await fs.writeFile(path.join(allowedDir, 'test.txt'), 'Test');
      await fs.writeFile(path.join(allowedDir, 'test.json'), 'JSON');
      await fs.writeFile(path.join(allowedDir, 'data.json'), 'Data');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.list',
          { path: allowedDir, pattern: '*.json' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.files).toContain('test.json');
      expect(response.payload?.files).toContain('data.json');
      expect(response.payload?.files).not.toContain('test.txt');
    });

    test('should list files recursively', async () => {
      const subdir = path.join(allowedDir, 'subdir');
      await fs.mkdir(subdir);
      await fs.writeFile(path.join(allowedDir, 'root.txt'), 'Root');
      await fs.writeFile(path.join(subdir, 'nested.txt'), 'Nested');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.list',
          { path: allowedDir, recursive: true },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.files).toContain('root.txt');
      expect(response.payload?.files).toContain('subdir/nested.txt');
    });

    test('should list recursively with pattern filter', async () => {
      const subdir = path.join(allowedDir, 'subdir');
      await fs.mkdir(subdir);
      await fs.writeFile(path.join(allowedDir, 'root.json'), 'Root');
      await fs.writeFile(path.join(allowedDir, 'root.txt'), 'Text');
      await fs.writeFile(path.join(subdir, 'nested.json'), 'Nested');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.list',
          { path: allowedDir, recursive: true, pattern: '*.json' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.files).toContain('root.json');
      expect(response.payload?.files).toContain('subdir/nested.json');
      expect(response.payload?.files).not.toContain('root.txt');
    });

    test('should reject list for restricted path', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.list',
          { path: restrictedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
    });

    test('should return empty array for empty directory', async () => {
      const emptyDir = path.join(allowedDir, 'empty');
      await fs.mkdir(emptyDir);

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.list',
          { path: emptyDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.files).toEqual([]);
    });
  });

  describe('fs.exists', () => {
    test('should return true for existing file', async () => {
      const filePath = path.join(allowedDir, 'exists.txt');
      await fs.writeFile(filePath, 'Test');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.exists',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.exists).toBe(true);
    });

    test('should return false for nonexistent file', async () => {
      const filePath = path.join(allowedDir, 'nonexistent.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.exists',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.exists).toBe(false);
    });

    test('should return true for existing directory', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.exists',
          { path: allowedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.exists).toBe(true);
    });

    test('should reject exists for restricted path', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.exists',
          { path: restrictedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
    });
  });

  describe('fs.stat', () => {
    test('should return file stats', async () => {
      const filePath = path.join(allowedDir, 'stat-test.txt');
      await fs.writeFile(filePath, 'Test content');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.stat',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.size).toBe(12); // "Test content" length
      expect(response.payload?.isFile).toBe(true);
      expect(response.payload?.isDirectory).toBe(false);
      expect(response.payload?.mtime).toBeGreaterThan(0);
      expect(response.payload?.ctime).toBeGreaterThan(0);
    });

    test('should return directory stats', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.stat',
          { path: allowedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.isFile).toBe(false);
      expect(response.payload?.isDirectory).toBe(true);
    });

    test('should reject stat for restricted path', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.stat',
          { path: restrictedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
    });

    test('should handle file not found error', async () => {
      const filePath = path.join(allowedDir, 'nonexistent.txt');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.stat',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toBeDefined();
    });
  });

  describe('fs.watch', () => {
    test('should create file watcher', async () => {
      const watchFs = new FileSystemActor('watch-fs', router, {
        allowedPaths: [allowedDir],
        operations: ['read', 'write', 'watch'],
      });

      const response = await watchFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.watch',
          { path: allowedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.watchId).toBeDefined();
      expect(response.payload?.watchId).toMatch(/^watch-/);

      await watchFs.shutdown();
    });

    test('should reject watch when operation not permitted', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.watch',
          { path: allowedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Watch operation not permitted');
    });

    test('should reject watch for restricted path', async () => {
      const watchFs = new FileSystemActor('watch-fs', router, {
        allowedPaths: [allowedDir],
        operations: ['read', 'write', 'watch'],
      });

      const response = await watchFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.watch',
          { path: restrictedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');

      await watchFs.shutdown();
    });
  });

  describe('fs.unwatch', () => {
    test('should remove file watcher', async () => {
      const watchFs = new FileSystemActor('watch-fs', router, {
        allowedPaths: [allowedDir],
        operations: ['read', 'write', 'watch'],
      });

      // Create watch
      const watchResponse = await watchFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.watch',
          { path: allowedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      const watchId = watchResponse.payload?.watchId;

      // Remove watch
      const unwatchResponse = await watchFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.unwatch',
          { watchId },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(unwatchResponse.success).toBe(true);
      expect(unwatchResponse.payload?.unwatched).toBe(true);

      await watchFs.shutdown();
    });

    test('should reject unwatch for nonexistent watch', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.unwatch',
          { watchId: 'watch-nonexistent' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Watch not found');
    });
  });

  describe('path validation', () => {
    test('should normalize and validate paths', async () => {
      const filePath = path.join(allowedDir, './subdir/../test.txt');
      await fs.writeFile(path.join(allowedDir, 'test.txt'), 'Test');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: filePath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.content).toBe('Test');
    });

    test('should prevent directory traversal attacks', async () => {
      // Try to escape via ../../../
      const evilPath = path.join(allowedDir, '../../../etc/passwd');

      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: evilPath },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
    });

    test('should handle multiple allowed paths', async () => {
      const allowedDir2 = path.join(testDir, 'allowed2');
      await fs.mkdir(allowedDir2, { recursive: true });

      const multiPathFs = new FileSystemActor('multi-path', router, {
        allowedPaths: [allowedDir, allowedDir2],
        operations: ['read', 'write'],
      });

      const file1 = path.join(allowedDir, 'file1.txt');
      const file2 = path.join(allowedDir2, 'file2.txt');
      await fs.writeFile(file1, 'File 1');
      await fs.writeFile(file2, 'File 2');

      const response1 = await multiPathFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: file1 },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      const response2 = await multiPathFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: file2 },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response1.success).toBe(true);
      expect(response1.payload?.content).toBe('File 1');
      expect(response2.success).toBe(true);
      expect(response2.payload?.content).toBe('File 2');
    });
  });

  describe('error handling', () => {
    test('should handle unknown message type', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.unknown',
          {},
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Unknown message type');
    });

    test('should provide clear error messages for access denial', async () => {
      const response = await fsActor.receive(
        createMessage(
          address('/system/fs'),
          'fs.read',
          { path: restrictedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Path access denied');
      expect(response.error).toContain(restrictedDir);
      expect(response.error).toContain('allowedPaths');
      expect(response.error).toContain(allowedDir);
    });
  });

  describe('integration with Actor base class', () => {
    test('should work with actor messaging', async () => {
      class FileStorageActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          if (message.type === 'save-data') {
            const filePath = path.join(allowedDir, 'data.json');
            const response = await this.ask(
              address('/system/fs'),
              'fs.write',
              {
                path: filePath,
                content: JSON.stringify(message.payload),
              }
            );

            return response;
          }

          return createMessage(address('/caller'), 'unknown', {}, { pattern: 'ask' }) as any;
        }
      }

      const storageActor = new FileStorageActor('storage-actor', router);
      router.registerActor('/storage-actor', storageActor);

      const response = await storageActor.receive(
        createMessage(
          address('/storage-actor'),
          'save-data',
          { key: 'value' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);

      // Verify file was written
      const content = await fs.readFile(path.join(allowedDir, 'data.json'), 'utf-8');
      expect(JSON.parse(content)).toEqual({ key: 'value' });
    });
  });

  describe('shutdown', () => {
    test('should close all watchers on shutdown', async () => {
      const watchFs = new FileSystemActor('watch-fs', router, {
        allowedPaths: [allowedDir],
        operations: ['read', 'write', 'watch'],
      });

      // Create multiple watches
      await watchFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.watch',
          { path: allowedDir },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      await watchFs.receive(
        createMessage(
          address('/system/fs'),
          'fs.watch',
          { path: allowedDir, pattern: '*.txt' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      // Shutdown should close all watchers
      await watchFs.shutdown();

      // No way to directly verify watchers are closed without exposing internal state,
      // but this ensures shutdown doesn't throw
    });
  });
});
