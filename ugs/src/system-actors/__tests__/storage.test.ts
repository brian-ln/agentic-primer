#!/usr/bin/env bun
import { describe, test, expect, beforeEach, afterEach } from 'bun:test';
import { StorageActor } from '../storage.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { Actor } from '../../messaging/actor.ts';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import { address, createMessage, createResponse } from '@agentic-primer/actors';
import type { Message, MessageResponse } from '@agentic-primer/actors';
import { Database } from 'bun:sqlite';
import { unlinkSync } from 'fs';

describe('StorageActor', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let storage: StorageActor;
  const dbPath = './test-storage.db';

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    // Create test database with some tables
    const db = new Database(dbPath);
    db.exec(`
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
    db.close();

    // Create storage actor with limited access
    storage = new StorageActor('storage', router, {
      dbPath,
      allowedTables: ['tasks', 'workflows'],
      operations: ['read', 'write', 'delete'],
    });

    router.registerActor('/system/storage', storage);
  });

  afterEach(() => {
    try {
      unlinkSync(dbPath);
    } catch (e) {
      // Ignore if file doesn't exist
    }
  });

  describe('storage.query', () => {
    test('should execute SELECT query on allowed table', async () => {
      // Insert test data
      const db = new Database(dbPath);
      db.exec("INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Test Task')");
      db.close();

      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          {
            sql: 'SELECT * FROM tasks WHERE status = ?',
            params: ['open'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.rows).toHaveLength(1);
      expect(response.payload?.rows[0].id).toBe('task-1');
      expect(response.payload?.rows[0].title).toBe('Test Task');
    });

    test('should reject query on disallowed table', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          {
            sql: 'SELECT * FROM users',
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Access denied');
      expect(response.error).toContain('users');
      expect(response.error).toContain('allowedTables');
    });

    test('should support parameterized queries', async () => {
      const db = new Database(dbPath);
      db.exec(`
        INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Task 1');
        INSERT INTO tasks (id, status, title) VALUES ('task-2', 'closed', 'Task 2');
        INSERT INTO tasks (id, status, title) VALUES ('task-3', 'open', 'Task 3');
      `);
      db.close();

      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          {
            sql: 'SELECT * FROM tasks WHERE status = ? ORDER BY id',
            params: ['open'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.rows).toHaveLength(2);
      expect(response.payload?.rows[0].id).toBe('task-1');
      expect(response.payload?.rows[1].id).toBe('task-3');
    });

    test('should return empty array for query with no results', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          {
            sql: 'SELECT * FROM tasks WHERE status = ?',
            params: ['nonexistent'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.rows).toEqual([]);
    });
  });

  describe('storage.execute', () => {
    test('should execute INSERT statement', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'INSERT INTO tasks (id, status, title) VALUES (?, ?, ?)',
            params: ['task-1', 'open', 'New Task'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.changes).toBe(1);

      // Verify data was inserted
      const db = new Database(dbPath);
      const result = db.query('SELECT * FROM tasks WHERE id = ?').get('task-1');
      expect(result).toBeDefined();
      expect((result as any).title).toBe('New Task');
      db.close();
    });

    test('should execute UPDATE statement', async () => {
      // Insert test data
      const db = new Database(dbPath);
      db.exec("INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Task')");
      db.close();

      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'UPDATE tasks SET status = ? WHERE id = ?',
            params: ['closed', 'task-1'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.changes).toBe(1);

      // Verify data was updated
      const db2 = new Database(dbPath);
      const result = db2.query('SELECT status FROM tasks WHERE id = ?').get('task-1');
      expect((result as any).status).toBe('closed');
      db2.close();
    });

    test('should execute DELETE statement', async () => {
      // Insert test data
      const db = new Database(dbPath);
      db.exec("INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Task')");
      db.close();

      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'DELETE FROM tasks WHERE id = ?',
            params: ['task-1'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.changes).toBe(1);

      // Verify data was deleted
      const db2 = new Database(dbPath);
      const result = db2.query('SELECT * FROM tasks WHERE id = ?').get('task-1');
      expect(result).toBeNull();
      db2.close();
    });

    test('should reject execute on disallowed table', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'INSERT INTO users (id, name) VALUES (?, ?)',
            params: ['user-1', 'Alice'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Access denied');
      expect(response.error).toContain('users');
    });

    test('should return changes count', async () => {
      // Insert multiple rows
      const db = new Database(dbPath);
      db.exec(`
        INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Task 1');
        INSERT INTO tasks (id, status, title) VALUES ('task-2', 'open', 'Task 2');
      `);
      db.close();

      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'UPDATE tasks SET status = ?',
            params: ['closed'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.changes).toBe(2);
    });
  });

  describe('storage.transaction', () => {
    test('should execute multiple statements in transaction', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.transaction',
          {
            statements: [
              {
                sql: 'INSERT INTO tasks (id, status, title) VALUES (?, ?, ?)',
                params: ['task-1', 'open', 'Task 1'],
              },
              {
                sql: 'INSERT INTO workflows (id, name) VALUES (?, ?)',
                params: ['wf-1', 'Workflow 1'],
              },
            ],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.committed).toBe(true);

      // Verify both inserts succeeded
      const db = new Database(dbPath);
      const task = db.query('SELECT * FROM tasks WHERE id = ?').get('task-1');
      const workflow = db.query('SELECT * FROM workflows WHERE id = ?').get('wf-1');
      expect(task).toBeDefined();
      expect(workflow).toBeDefined();
      db.close();
    });

    test('should rollback transaction on error', async () => {
      const db = new Database(dbPath);
      db.exec("INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Existing')");
      db.close();

      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.transaction',
          {
            statements: [
              {
                sql: 'INSERT INTO tasks (id, status, title) VALUES (?, ?, ?)',
                params: ['task-2', 'open', 'Task 2'],
              },
              {
                // This will fail due to duplicate primary key
                sql: 'INSERT INTO tasks (id, status, title) VALUES (?, ?, ?)',
                params: ['task-1', 'open', 'Duplicate'],
              },
            ],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);

      // Verify neither insert succeeded (transaction rolled back)
      const db2 = new Database(dbPath);
      const task2 = db2.query('SELECT * FROM tasks WHERE id = ?').get('task-2');
      expect(task2).toBeNull();
      db2.close();
    });

    test('should validate all statements before executing', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.transaction',
          {
            statements: [
              {
                sql: 'INSERT INTO tasks (id, status, title) VALUES (?, ?, ?)',
                params: ['task-1', 'open', 'Task 1'],
              },
              {
                // Second statement accesses disallowed table
                sql: 'INSERT INTO users (id, name) VALUES (?, ?)',
                params: ['user-1', 'Alice'],
              },
            ],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Access denied');
      expect(response.error).toContain('users');

      // Verify first insert was NOT executed (validation failed before execution)
      const db = new Database(dbPath);
      const task = db.query('SELECT * FROM tasks WHERE id = ?').get('task-1');
      expect(task).toBeNull();
      db.close();
    });
  });

  describe('operation permissions', () => {
    test('should allow read operation when permitted', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          { sql: 'SELECT * FROM tasks' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });

    test('should reject read operation when not permitted', async () => {
      // Create read-only storage actor
      const readOnlyStorage = new StorageActor('readonly-storage', router, {
        dbPath,
        allowedTables: ['tasks'],
        operations: ['write'], // Only write, no read
      });

      const response = await readOnlyStorage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          { sql: 'SELECT * FROM tasks' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Read operation not permitted');
    });

    test('should reject write operation when not permitted', async () => {
      // Create read-only storage actor
      const readOnlyStorage = new StorageActor('readonly-storage', router, {
        dbPath,
        allowedTables: ['tasks'],
        operations: ['read'], // Only read
      });

      const response = await readOnlyStorage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'INSERT INTO tasks (id, status, title) VALUES (?, ?, ?)',
            params: ['task-1', 'open', 'Task'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain("Operation 'write' not permitted");
    });

    test('should reject delete operation when not permitted', async () => {
      const noDeleteStorage = new StorageActor('no-delete-storage', router, {
        dbPath,
        allowedTables: ['tasks'],
        operations: ['read', 'write'], // No delete
      });

      const response = await noDeleteStorage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'DELETE FROM tasks WHERE id = ?',
            params: ['task-1'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain("Operation 'delete' not permitted");
    });

    test('should reject admin operation when not permitted', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'CREATE TABLE new_table (id TEXT)',
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain("Operation 'admin' not permitted");
    });

    test('should allow admin operations when permitted', async () => {
      const adminStorage = new StorageActor('admin-storage', router, {
        dbPath,
        allowedTables: ['*'],
        operations: ['read', 'write', 'delete', 'admin'],
      });

      const response = await adminStorage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'CREATE TABLE IF NOT EXISTS admin_table (id TEXT)',
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });
  });

  describe('wildcard table access', () => {
    test('should allow access to any table with wildcard', async () => {
      const wildcardStorage = new StorageActor('wildcard-storage', router, {
        dbPath,
        allowedTables: ['*'],
        operations: ['read', 'write'],
      });

      // Should allow access to users table (normally disallowed)
      const response = await wildcardStorage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          { sql: 'SELECT * FROM users' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });

    test('should allow access to tasks table with wildcard', async () => {
      const wildcardStorage = new StorageActor('wildcard-storage', router, {
        dbPath,
        allowedTables: ['*'],
        operations: ['read'],
      });

      const response = await wildcardStorage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          { sql: 'SELECT * FROM tasks' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });
  });

  describe('table extraction', () => {
    test('should extract table from FROM clause', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          { sql: 'SELECT * FROM tasks' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });

    test('should extract table from INSERT INTO clause', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'INSERT INTO tasks (id) VALUES (?)',
            params: ['task-1'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });

    test('should extract table from UPDATE clause', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          {
            sql: 'UPDATE tasks SET status = ?',
            params: ['closed'],
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });

    test('should handle case-insensitive SQL keywords', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          { sql: 'select * from tasks' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
    });
  });

  describe('error handling', () => {
    test('should handle unknown message type', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.unknown',
          {},
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Unknown message type');
    });

    test('should handle SQL syntax errors', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.query',
          { sql: 'INVALID SQL SYNTAX' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toBeDefined();
    });

    test('should handle invalid SQL operation', async () => {
      const response = await storage.receive(
        createMessage(
          address('/system/storage'),
          'storage.execute',
          { sql: 'EXPLAIN QUERY PLAN SELECT * FROM tasks' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Cannot infer operation');
    });
  });

  describe('integration with Actor base class', () => {
    test('should work with actor messaging', async () => {
      class TaskActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          if (message.type === 'get-tasks') {
            const response = await this.ask(
              address('/system/storage'),
              'storage.query',
              {
                sql: 'SELECT * FROM tasks WHERE status = ?',
                params: ['open'],
              }
            );

            return createResponse(message, {
              tasks: response.payload?.rows || [],
            });
          }

          return createResponse(message, {});
        }
      }

      const taskActor = new TaskActor('task-actor', router);
      router.registerActor('/task-actor', taskActor);

      // Insert test data
      const db = new Database(dbPath);
      db.exec("INSERT INTO tasks (id, status, title) VALUES ('task-1', 'open', 'Test')");
      db.close();

      // Query via TaskActor
      const response = await taskActor.receive(
        createMessage(
          address('/task-actor'),
          'get-tasks',
          {},
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.tasks).toHaveLength(1);
    });
  });

  describe('shutdown', () => {
    test('should close database on shutdown', async () => {
      await storage.shutdown();
      // Database should be closed, subsequent operations should fail
      // (We can't easily test this without exposing internal state)
    });
  });
});
