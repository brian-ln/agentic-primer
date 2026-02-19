#!/usr/bin/env bun
/**
 * System Actor Integration Tests
 *
 * Tests routing-based access control across StorageActor and FileSystemActor.
 * Verifies that access control is enforced through routing configuration,
 * not through actor code.
 *
 * Architecture: Pure Actor Model
 * - System actors enforce their own access control internally
 * - Access control happens through routing registration
 * - No helper classes - actors use standard messaging (ask/tell)
 */

import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { StorageActor } from '../storage.ts';
import { FileSystemActor } from '../filesystem.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { Actor } from '../../messaging/actor.ts';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import { address, createMessage, createResponse } from '@agentic-primer/actors';
import type { Message, MessageResponse } from '@agentic-primer/actors';
import { Database } from 'bun:sqlite';
import * as fs from 'node:fs/promises';
import { mkdtemp, rm } from 'node:fs/promises';
import * as path from 'node:path';
import { tmpdir } from 'node:os';
import { unlinkSync, existsSync } from 'fs';

// Test Actor that uses system actors through routing
class WorkflowActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'get-tasks') {
      // Query storage via routing
      const response = await this.ask(
        address('/workflows/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM tasks WHERE status = ?', params: ['open'] }
      );
      return createResponse(message, { tasks: response.payload?.rows || [] });
    }

    if (message.type === 'save-workflow-config') {
      // Write file via routing
      const response = await this.ask(
        address('/workflows/system/fs'),
        'fs.write',
        {
          path: message.payload.path,
          content: message.payload.content
        }
      );
      return createResponse(message, { saved: response.success });
    }

    if (message.type === 'combined-operation') {
      // Use both storage and filesystem in single operation
      const storageResponse = await this.ask(
        address('/workflows/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM tasks' }
      );

      const fsResponse = await this.ask(
        address('/workflows/system/fs'),
        'fs.write',
        {
          path: message.payload.exportPath,
          content: JSON.stringify(storageResponse.payload?.rows)
        }
      );

      return createResponse(message, {
        taskCount: storageResponse.payload?.rows?.length || 0,
        exported: fsResponse.success
      });
    }

    return createResponse(message, {});
  }
}

class DomainActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'get-entities') {
      // Should access different storage instance
      const response = await this.ask(
        address('/domain/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM entities' }
      );
      return createResponse(message, { entities: response.payload?.rows || [] });
    }

    if (message.type === 'read-domain-config') {
      // Should access different filesystem instance
      const response = await this.ask(
        address('/domain/system/fs'),
        'fs.read',
        { path: message.payload.path }
      );
      return createResponse(message, { content: response.payload?.content });
    }

    return createResponse(message, {});
  }
}

describe('Integration: StorageActor Access Control via Routing', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let workflowStorage: StorageActor;
  let domainStorage: StorageActor;
  let workflowActor: WorkflowActor;
  let domainActor: DomainActor;
  let testDbDir: string;
  let workflowDbPath: string;
  let domainDbPath: string;

  beforeEach(async () => {
    testDbDir = await mkdtemp(path.join(tmpdir(), 'ugs-integration-storage-'));
    workflowDbPath = path.join(testDbDir, 'workflow.db');
    domainDbPath = path.join(testDbDir, 'domain.db');

    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    // Create workflow database with tasks and workflows tables
    const workflowDb = new Database(workflowDbPath);
    workflowDb.exec(`
      CREATE TABLE IF NOT EXISTS tasks (
        id TEXT PRIMARY KEY,
        status TEXT,
        title TEXT
      );
      CREATE TABLE IF NOT EXISTS workflows (
        id TEXT PRIMARY KEY,
        name TEXT
      );
      CREATE TABLE IF NOT EXISTS users (
        id TEXT PRIMARY KEY,
        name TEXT
      );
    `);
    workflowDb.close();

    // Create domain database with entities table
    const domainDb = new Database(domainDbPath);
    domainDb.exec(`
      CREATE TABLE IF NOT EXISTS entities (
        id TEXT PRIMARY KEY,
        type TEXT,
        data TEXT
      );
      CREATE TABLE IF NOT EXISTS tasks (
        id TEXT PRIMARY KEY,
        description TEXT
      );
    `);
    domainDb.close();

    // Create storage actors with different configurations
    // WorkflowActor can only access tasks and workflows tables
    workflowStorage = new StorageActor('workflow-storage', router, {
      dbPath: workflowDbPath,
      allowedTables: ['tasks', 'workflows'],
      operations: ['read', 'write', 'delete'],
    });

    // DomainActor can only access entities table
    domainStorage = new StorageActor('domain-storage', router, {
      dbPath: domainDbPath,
      allowedTables: ['entities'],
      operations: ['read', 'write'],
    });

    // Register storage actors at different routes
    router.registerActor('/workflows/system/storage', workflowStorage);
    router.registerActor('/domain/system/storage', domainStorage);

    // Create and register business actors
    workflowActor = new WorkflowActor('workflow-actor', router);
    domainActor = new DomainActor('domain-actor', router);
    router.registerActor('/workflows/workflow-actor', workflowActor);
    router.registerActor('/domain/domain-actor', domainActor);
  });

  afterEach(async () => {
    await rm(testDbDir, { recursive: true, force: true });
  });

  test('WorkflowActor can query allowed table (tasks) via routing', async () => {
    // Insert test data
    const db = new Database(workflowDbPath);
    db.exec("INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Test Task')");
    db.close();

    const response = await workflowActor.receive(
      createMessage(
        address('/workflows/workflow-actor'),
        'get-tasks',
        {},
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(response.success).toBe(true);
    expect(response.payload?.tasks).toHaveLength(1);
    expect(response.payload?.tasks[0].id).toBe('task-1');
  });

  test('WorkflowActor cannot query disallowed table (users) via routing', async () => {
    // Attempt to query users table (not in allowedTables)
    const response = await workflowStorage.receive(
      createMessage(
        address('/workflows/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM users' },
        { pattern: 'ask', from: address('/workflows/workflow-actor') }
      )
    );

    expect(response.success).toBe(false);
    expect(response.error).toContain('Access denied');
    expect(response.error).toContain('users');
    expect(response.error).toContain('allowedTables');
  });

  test('DomainActor can query its allowed table (entities)', async () => {
    // Insert test data
    const db = new Database(domainDbPath);
    db.exec("INSERT INTO entities (id, type, data) VALUES ('entity-1', 'user', '{\"name\":\"Alice\"}')");
    db.close();

    const response = await domainActor.receive(
      createMessage(
        address('/domain/domain-actor'),
        'get-entities',
        {},
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(response.success).toBe(true);
    expect(response.payload?.entities).toHaveLength(1);
    expect(response.payload?.entities[0].type).toBe('user');
  });

  test('DomainActor cannot query disallowed table (tasks)', async () => {
    // Attempt to query tasks table (not in allowedTables for domain storage)
    const response = await domainStorage.receive(
      createMessage(
        address('/domain/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM tasks' },
        { pattern: 'ask', from: address('/domain/domain-actor') }
      )
    );

    expect(response.success).toBe(false);
    expect(response.error).toContain('Access denied');
    expect(response.error).toContain('tasks');
  });

  test('Routing isolates storage namespaces (workflow vs domain)', async () => {
    // Insert data in both databases
    const wfDb = new Database(workflowDbPath);
    wfDb.exec("INSERT INTO tasks (id, status, title) VALUES ('wf-task', 'open', 'Workflow Task')");
    wfDb.close();

    const domDb = new Database(domainDbPath);
    domDb.exec("INSERT INTO entities (id, type, data) VALUES ('domain-entity', 'org', '{}')");
    domDb.close();

    // Workflow actor queries workflow storage
    const wfResponse = await workflowActor.receive(
      createMessage(
        address('/workflows/workflow-actor'),
        'get-tasks',
        {},
        { pattern: 'ask', from: address('/caller') }
      )
    );

    // Domain actor queries domain storage
    const domainResponse = await domainActor.receive(
      createMessage(
        address('/domain/domain-actor'),
        'get-entities',
        {},
        { pattern: 'ask', from: address('/caller') }
      )
    );

    // Each gets their own data
    expect(wfResponse.success).toBe(true);
    expect(wfResponse.payload?.tasks[0].id).toBe('wf-task');

    expect(domainResponse.success).toBe(true);
    expect(domainResponse.payload?.entities[0].id).toBe('domain-entity');
  });

  test('Unregistered route cannot access StorageActor', async () => {
    // Try to access storage from unregistered route
    const response = await router.route(
      createMessage(
        address('/unregistered/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM tasks' },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(response.success).toBe(false);
    expect(response.error).toContain('not found');
  });
});

describe('Integration: FileSystemActor Access Control via Routing', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let workflowFs: FileSystemActor;
  let domainFs: FileSystemActor;
  let workflowActor: WorkflowActor;
  let domainActor: DomainActor;
  let testDir: string;
  let workflowDir: string;
  let domainDir: string;
  let restrictedDir: string;

  beforeEach(async () => {
    testDir = await mkdtemp(path.join(tmpdir(), 'ugs-integration-fs-'));
    workflowDir = path.join(testDir, 'workflows');
    domainDir = path.join(testDir, 'domain');
    restrictedDir = path.join(testDir, 'restricted');

    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    // Create test directories
    await fs.mkdir(workflowDir, { recursive: true });
    await fs.mkdir(domainDir, { recursive: true });
    await fs.mkdir(restrictedDir, { recursive: true });

    // Create filesystem actors with different allowed paths
    workflowFs = new FileSystemActor('workflow-fs', router, {
      allowedPaths: [workflowDir],
      operations: ['read', 'write', 'delete'],
    });

    domainFs = new FileSystemActor('domain-fs', router, {
      allowedPaths: [domainDir],
      operations: ['read', 'write'],
    });

    // Register filesystem actors at different routes
    router.registerActor('/workflows/system/fs', workflowFs);
    router.registerActor('/domain/system/fs', domainFs);

    // Create and register business actors
    workflowActor = new WorkflowActor('workflow-actor', router);
    domainActor = new DomainActor('domain-actor', router);
    router.registerActor('/workflows/workflow-actor', workflowActor);
    router.registerActor('/domain/domain-actor', domainActor);
  });

  afterEach(async () => {
    await workflowFs.shutdown();
    await domainFs.shutdown();

    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch (e) {
      // Ignore if directory doesn't exist
    }
  });

  test('WorkflowActor can write to allowed path via routing', async () => {
    const filePath = path.join(workflowDir, 'config.json');

    const response = await workflowActor.receive(
      createMessage(
        address('/workflows/workflow-actor'),
        'save-workflow-config',
        { path: filePath, content: '{"enabled":true}' },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(response.success).toBe(true);
    expect(response.payload?.saved).toBe(true);

    // Verify file was written
    const content = await fs.readFile(filePath, 'utf-8');
    expect(content).toBe('{"enabled":true}');
  });

  test('WorkflowActor cannot write to restricted path', async () => {
    const filePath = path.join(restrictedDir, 'secret.txt');

    const response = await workflowFs.receive(
      createMessage(
        address('/workflows/system/fs'),
        'fs.write',
        { path: filePath, content: 'Secret data' },
        { pattern: 'ask', from: address('/workflows/workflow-actor') }
      )
    );

    expect(response.success).toBe(false);
    expect(response.error).toContain('Path access denied');
    expect(response.error).toContain(restrictedDir);
    expect(response.error).toContain(workflowDir);
  });

  test('DomainActor can read from its allowed path', async () => {
    const filePath = path.join(domainDir, 'settings.json');
    await fs.writeFile(filePath, '{"theme":"dark"}');

    const response = await domainActor.receive(
      createMessage(
        address('/domain/domain-actor'),
        'read-domain-config',
        { path: filePath },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(response.success).toBe(true);
    expect(response.payload?.content).toBe('{"theme":"dark"}');
  });

  test('DomainActor cannot read from workflow path', async () => {
    const filePath = path.join(workflowDir, 'config.json');
    await fs.writeFile(filePath, 'workflow data');

    const response = await domainFs.receive(
      createMessage(
        address('/domain/system/fs'),
        'fs.read',
        { path: filePath },
        { pattern: 'ask', from: address('/domain/domain-actor') }
      )
    );

    expect(response.success).toBe(false);
    expect(response.error).toContain('Path access denied');
  });

  test('Path traversal attacks are blocked via routing', async () => {
    // Create secret file in restricted directory
    const secretPath = path.join(restrictedDir, 'secret.txt');
    await fs.writeFile(secretPath, 'Secret data');

    // Attempt path traversal from workflow directory
    const traversalPath = path.join(workflowDir, '../restricted/secret.txt');

    const response = await workflowFs.receive(
      createMessage(
        address('/workflows/system/fs'),
        'fs.read',
        { path: traversalPath },
        { pattern: 'ask', from: address('/workflows/workflow-actor') }
      )
    );

    expect(response.success).toBe(false);
    expect(response.error).toContain('Path access denied');
  });

  test('Routing isolates filesystem namespaces', async () => {
    const wfFile = path.join(workflowDir, 'workflow.txt');
    const domainFile = path.join(domainDir, 'domain.txt');

    await fs.writeFile(wfFile, 'Workflow content');
    await fs.writeFile(domainFile, 'Domain content');

    // Workflow actor reads workflow file
    const wfResponse = await workflowFs.receive(
      createMessage(
        address('/workflows/system/fs'),
        'fs.read',
        { path: wfFile },
        { pattern: 'ask', from: address('/workflows/workflow-actor') }
      )
    );

    // Domain actor reads domain file
    const domainResponse = await domainFs.receive(
      createMessage(
        address('/domain/system/fs'),
        'fs.read',
        { path: domainFile },
        { pattern: 'ask', from: address('/domain/domain-actor') }
      )
    );

    expect(wfResponse.success).toBe(true);
    expect(wfResponse.payload?.content).toBe('Workflow content');

    expect(domainResponse.success).toBe(true);
    expect(domainResponse.payload?.content).toBe('Domain content');
  });

  test('Unregistered route cannot access FileSystemActor', async () => {
    const filePath = path.join(workflowDir, 'test.txt');

    const response = await router.route(
      createMessage(
        address('/unregistered/system/fs'),
        'fs.read',
        { path: filePath },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(response.success).toBe(false);
    expect(response.error).toContain('not found');
  });
});

describe('Integration: Cross-System Actor Operations', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let storage: StorageActor;
  let fsActor: FileSystemActor;
  let workflowActor: WorkflowActor;
  let testDir: string;
  let dbPath: string;
  let allowedDir: string;

  beforeEach(async () => {
    testDir = await mkdtemp(path.join(tmpdir(), 'ugs-integration-cross-'));
    dbPath = path.join(testDir, 'cross.db');
    allowedDir = path.join(testDir, 'allowed');

    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    // Create database
    const db = new Database(dbPath);
    db.exec(`
      CREATE TABLE IF NOT EXISTS tasks (
        id TEXT PRIMARY KEY,
        title TEXT,
        status TEXT
      );
    `);
    db.exec("INSERT INTO tasks (id, title, status) VALUES ('task-1', 'First', 'open')");
    db.exec("INSERT INTO tasks (id, title, status) VALUES ('task-2', 'Second', 'closed')");
    db.close();

    // Create filesystem
    await fs.mkdir(allowedDir, { recursive: true });

    // Create system actors
    storage = new StorageActor('storage', router, {
      dbPath,
      allowedTables: ['tasks'],
      operations: ['read', 'write'],
    });

    fsActor = new FileSystemActor('fs', router, {
      allowedPaths: [allowedDir],
      operations: ['read', 'write'],
    });

    // Register system actors
    router.registerActor('/workflows/system/storage', storage);
    router.registerActor('/workflows/system/fs', fsActor);

    // Create business actor
    workflowActor = new WorkflowActor('workflow-actor', router);
    router.registerActor('/workflows/workflow-actor', workflowActor);
  });

  afterEach(async () => {
    await fsActor.shutdown();
    await rm(testDir, { recursive: true, force: true });
  });

  test('Actor uses both StorageActor and FileSystemActor in single workflow', async () => {
    const exportPath = path.join(allowedDir, 'export.json');

    const response = await workflowActor.receive(
      createMessage(
        address('/workflows/workflow-actor'),
        'combined-operation',
        { exportPath },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(response.success).toBe(true);
    expect(response.payload?.taskCount).toBe(2);
    expect(response.payload?.exported).toBe(true);

    // Verify file was created
    const content = await fs.readFile(exportPath, 'utf-8');
    const tasks = JSON.parse(content);
    expect(tasks).toHaveLength(2);
    expect(tasks[0].id).toBe('task-1');
  });

  test('Multiple actors can share same system actor instance', async () => {
    // Create second business actor
    class ReportActor extends Actor {
      async receive(message: Message): Promise<MessageResponse> {
        if (message.type === 'generate-report') {
          const response = await this.ask(
            address('/workflows/system/storage'),
            'storage.query',
            { sql: 'SELECT COUNT(*) as count FROM tasks' }
          );
          return createResponse(message, { count: response.payload?.rows[0].count });
        }
        return createResponse(message, {});
      }
    }

    const reportActor = new ReportActor('report-actor', router);
    router.registerActor('/workflows/report-actor', reportActor);

    // Both actors use same storage instance
    const wfResponse = await workflowActor.receive(
      createMessage(
        address('/workflows/workflow-actor'),
        'get-tasks',
        {},
        { pattern: 'ask', from: address('/caller') }
      )
    );

    const reportResponse = await reportActor.receive(
      createMessage(
        address('/workflows/report-actor'),
        'generate-report',
        {},
        { pattern: 'ask', from: address('/caller') }
      )
    );

    // WorkflowActor filters by status='open', so only 1 task
    expect(wfResponse.success).toBe(true);
    expect(wfResponse.payload?.tasks).toHaveLength(1);
    expect(wfResponse.payload?.tasks[0].status).toBe('open');

    // ReportActor counts all tasks (no filter)
    expect(reportResponse.success).toBe(true);
    expect(reportResponse.payload?.count).toBe(2);
  });

  test('Storage and filesystem operations in transaction-like pattern', async () => {
    // Simulate multi-step workflow with rollback capability
    const exportPath = path.join(allowedDir, 'backup.json');

    // Step 1: Query data
    const queryResponse = await storage.receive(
      createMessage(
        address('/workflows/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM tasks WHERE status = ?', params: ['open'] },
        { pattern: 'ask', from: address('/workflows/workflow-actor') }
      )
    );

    expect(queryResponse.success).toBe(true);
    const openTasks = queryResponse.payload?.rows;

    // Step 2: Export to file
    const writeResponse = await fsActor.receive(
      createMessage(
        address('/workflows/system/fs'),
        'fs.write',
        { path: exportPath, content: JSON.stringify(openTasks) },
        { pattern: 'ask', from: address('/workflows/workflow-actor') }
      )
    );

    expect(writeResponse.success).toBe(true);

    // Step 3: Verify both operations succeeded
    const verifyResponse = await fsActor.receive(
      createMessage(
        address('/workflows/system/fs'),
        'fs.exists',
        { path: exportPath },
        { pattern: 'ask', from: address('/workflows/workflow-actor') }
      )
    );

    expect(verifyResponse.success).toBe(true);
    expect(verifyResponse.payload?.exists).toBe(true);
  });
});

describe('Integration: Operation Permission Enforcement', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let readOnlyStorage: StorageActor;
  let readOnlyFs: FileSystemActor;
  let testDir: string;
  let dbPath: string;
  let allowedDir: string;

  beforeEach(async () => {
    testDir = await mkdtemp(path.join(tmpdir(), 'ugs-integration-readonly-'));
    dbPath = path.join(testDir, 'readonly.db');
    allowedDir = path.join(testDir, 'allowed');

    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    // Create database
    const db = new Database(dbPath);
    db.exec(`
      CREATE TABLE IF NOT EXISTS tasks (id TEXT PRIMARY KEY, title TEXT);
    `);
    db.close();

    // Create filesystem
    await fs.mkdir(allowedDir, { recursive: true });

    // Create read-only system actors
    readOnlyStorage = new StorageActor('readonly-storage', router, {
      dbPath,
      allowedTables: ['tasks'],
      operations: ['read'], // Only read
    });

    readOnlyFs = new FileSystemActor('readonly-fs', router, {
      allowedPaths: [allowedDir],
      operations: ['read'], // Only read
    });

    router.registerActor('/readonly/system/storage', readOnlyStorage);
    router.registerActor('/readonly/system/fs', readOnlyFs);
  });

  afterEach(async () => {
    await readOnlyFs.shutdown();
    await rm(testDir, { recursive: true, force: true });
  });

  test('Read-only storage allows queries but denies writes', async () => {
    // Query should succeed
    const queryResponse = await readOnlyStorage.receive(
      createMessage(
        address('/readonly/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM tasks' },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(queryResponse.success).toBe(true);

    // Write should fail
    const writeResponse = await readOnlyStorage.receive(
      createMessage(
        address('/readonly/system/storage'),
        'storage.execute',
        { sql: 'INSERT INTO tasks (id, title) VALUES (?, ?)', params: ['task-1', 'Test'] },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(writeResponse.success).toBe(false);
    expect(writeResponse.error).toContain("Operation 'write' not permitted");
  });

  test('Read-only filesystem allows reads but denies writes', async () => {
    const filePath = path.join(allowedDir, 'test.txt');
    await fs.writeFile(filePath, 'Test content');

    // Read should succeed
    const readResponse = await readOnlyFs.receive(
      createMessage(
        address('/readonly/system/fs'),
        'fs.read',
        { path: filePath },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(readResponse.success).toBe(true);
    expect(readResponse.payload?.content).toBe('Test content');

    // Write should fail
    const writeResponse = await readOnlyFs.receive(
      createMessage(
        address('/readonly/system/fs'),
        'fs.write',
        { path: filePath, content: 'New content' },
        { pattern: 'ask', from: address('/caller') }
      )
    );

    expect(writeResponse.success).toBe(false);
    expect(writeResponse.error).toContain('Write operation not permitted');
  });
});
