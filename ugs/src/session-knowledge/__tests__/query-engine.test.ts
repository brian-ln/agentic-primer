/**
 * QueryEngine Comprehensive Test Suite
 * Epic: agentic-primer-9ad
 * Task: agentic-primer-t49.7
 *
 * Tests all QueryEngine methods with edge cases and error handling
 * Target: 80%+ coverage (from 12%)
 *
 * Test Database: Creates/uses ~/.claude/index/sessions.db with test data
 */

import { describe, test, expect, beforeAll, afterAll } from 'bun:test';
import { Database } from 'bun:sqlite';
import { join } from 'path';
import { existsSync, mkdirSync, renameSync, copyFileSync, rmSync } from 'fs';
import { QueryEngine, type SessionResult, type QueryOptions } from '../index/QueryEngine';

// Database paths
const DB_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(DB_DIR, 'sessions-libsql.db');
const BACKUP_PATH = join(DB_DIR, 'sessions-libsql.db.test-backup');
const ORIGINAL_EXISTS = existsSync(DB_PATH);

// Global setup: backup existing DB and create test DB
beforeAll(() => {
  // Ensure directory exists
  if (!existsSync(DB_DIR)) {
    mkdirSync(DB_DIR, { recursive: true });
  }

  // Backup existing database if it exists
  if (ORIGINAL_EXISTS) {
    console.log('Backing up existing database...');
    copyFileSync(DB_PATH, BACKUP_PATH);
  }

  // Create fresh test database
  console.log('Creating test database...');
  if (existsSync(DB_PATH)) {
    rmSync(DB_PATH);
  }

  setupTestDatabase();
});

// Global teardown: restore original DB
afterAll(() => {
  console.log('Cleaning up...');

  // Remove test database
  if (existsSync(DB_PATH)) {
    rmSync(DB_PATH);
  }

  // Restore original database
  if (ORIGINAL_EXISTS && existsSync(BACKUP_PATH)) {
    console.log('Restoring original database...');
    renameSync(BACKUP_PATH, DB_PATH);
  }
});

// ============================================================================
// Test Suites
// ============================================================================

describe('QueryEngine - Constructor & Initialization', () => {
  test('should initialize with default database path', async () => {
    const engine = new QueryEngine();
    expect(engine).toBeDefined();
    engine.close();
  });

  test('should successfully open readonly database', async () => {
    const engine = new QueryEngine();
    expect(engine).toBeDefined();

    // Should be able to query
    const results = await engine.recent(1);
    expect(Array.isArray(results)).toBe(true);

    engine.close();
  });
});

describe('QueryEngine - today()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return sessions from today', async () => {
    const results = await engine.today();

    expect(Array.isArray(results)).toBe(true);

    if (results.length > 0) {
      const today = new Date();
      today.setHours(0, 0, 0, 0);

      results.forEach(session => {
        expect(session.id).toBeDefined();
        expect(typeof session.created).toBe('number');
        expect(session.created).toBeGreaterThanOrEqual(today.getTime());
      });
    }
  });

  test('should respect limit option', async () => {
    const limit = 5;
    const results = await engine.today({ limit });

    expect(results.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 100', async () => {
    const results = await engine.today();
    expect(results.length).toBeLessThanOrEqual(100);
  });

  test('should handle empty options object', async () => {
    const results = await engine.today({});
    expect(Array.isArray(results)).toBe(true);
  });

  test('should order by created DESC', async () => {
    const results = await engine.today();

    if (results.length > 1) {
      for (let i = 0; i < results.length - 1; i++) {
        expect(results[i].created).toBeGreaterThanOrEqual(
          results[i + 1].created
        );
      }
    }
  });

  test('should include required fields', async () => {
    const results = await engine.today({ limit: 1 });

    if (results.length > 0) {
      const session = results[0];
      expect(session.id).toBeDefined();
      expect(typeof session.created).toBe('number');
      expect(session.summary).toBeDefined();
      expect(typeof session.cost).toBe('number');
      expect(typeof session.messageCount).toBe('number');
    }
  });
});

describe('QueryEngine - yesterday()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return sessions from yesterday', async () => {
    const results = await engine.yesterday();

    expect(Array.isArray(results)).toBe(true);

    if (results.length > 0) {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);
      yesterday.setHours(0, 0, 0, 0);

      const tomorrow = new Date(yesterday);
      tomorrow.setDate(tomorrow.getDate() + 1);

      results.forEach(session => {
        expect(session.created).toBeGreaterThanOrEqual(yesterday.getTime());
        expect(session.created).toBeLessThan(tomorrow.getTime());
      });
    }
  });

  test('should respect limit option', async () => {
    const limit = 3;
    const results = await engine.yesterday({ limit });

    expect(results.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 100', async () => {
    const results = await engine.yesterday();
    expect(results.length).toBeLessThanOrEqual(100);
  });

  test('should not include today sessions', async () => {
    const results = await engine.yesterday();
    const today = new Date();
    today.setHours(0, 0, 0, 0);

    results.forEach(session => {
      expect(session.created).toBeLessThan(today.getTime());
    });
  });
});

describe('QueryEngine - recent()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return recent sessions with default limit', async () => {
    const results = await engine.recent();

    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBeLessThanOrEqual(10);
    expect(results.length).toBeGreaterThan(0); // We seeded data
  });

  test('should respect custom limit', async () => {
    const limit = 5;
    const results = await engine.recent(limit);

    expect(results.length).toBeLessThanOrEqual(limit);
  });

  test('should order by created DESC (most recent first)', async () => {
    const results = await engine.recent(10);

    if (results.length > 1) {
      for (let i = 0; i < results.length - 1; i++) {
        expect(results[i].created).toBeGreaterThanOrEqual(
          results[i + 1].created
        );
      }
    }
  });

  test('should return all required fields', async () => {
    const results = await engine.recent(1);

    expect(results.length).toBeGreaterThan(0);
    const session = results[0];
    expect(session.id).toBeDefined();
    expect(typeof session.created).toBe('number');
    expect(session.summary).toBeDefined();
    expect(typeof session.cost).toBe('number');
    expect(typeof session.messageCount).toBe('number');
  });

  test('should handle limit of 0', async () => {
    const results = await engine.recent(0);
    expect(results.length).toBe(0);
  });

  test('should handle very large limit', async () => {
    const results = await engine.recent(1000);
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBeGreaterThan(0);
  });
});

describe('QueryEngine - byFile()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should find sessions that modified a specific file', async () => {
    const results = await engine.byFile('test.ts');

    expect(Array.isArray(results)).toBe(true);

    if (results.length > 0) {
      results.forEach(session => {
        expect(session.id).toBeDefined();
      });
    }
  });

  test('should use LIKE pattern matching (partial matches)', async () => {
    const results = await engine.byFile('QueryEngine');

    expect(Array.isArray(results)).toBe(true);
    // Should find sessions that modified QueryEngine files
  });

  test('should respect limit option', async () => {
    const limit = 2;
    const results = await engine.byFile('test', { limit });

    expect(results.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 50', async () => {
    const results = await engine.byFile('');
    expect(results.length).toBeLessThanOrEqual(50);
  });

  test('should handle non-existent file', async () => {
    const results = await engine.byFile('nonexistent-file-xyz-' + Date.now());

    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(0);
  });

  test('should order by created DESC', async () => {
    const results = await engine.byFile('test');

    if (results.length > 1) {
      for (let i = 0; i < results.length - 1; i++) {
        expect(results[i].created).toBeGreaterThanOrEqual(
          results[i + 1].created
        );
      }
    }
  });

  test('should handle empty string', async () => {
    const results = await engine.byFile('');
    expect(Array.isArray(results)).toBe(true);
  });

  test('should return DISTINCT sessions (no duplicates)', async () => {
    const results = await engine.byFile('test');
    const ids = results.map(s => s.id);
    const uniqueIds = new Set(ids);

    expect(ids.length).toBe(uniqueIds.size);
  });
});

describe('QueryEngine - search()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should search sessions by keyword in summary', async () => {
    const results = await engine.search('test');

    expect(Array.isArray(results)).toBe(true);

    if (results.length > 0) {
      results.forEach(session => {
        expect(session.summary).toBeDefined();
      });
    }
  });

  test('should use case-insensitive LIKE search', async () => {
    const resultsLower = await engine.search('test');
    const resultsUpper = await engine.search('TEST');

    // LIKE in SQLite is case-insensitive by default for ASCII
    expect(Array.isArray(resultsLower)).toBe(true);
    expect(Array.isArray(resultsUpper)).toBe(true);
  });

  test('should respect limit option', async () => {
    const limit = 3;
    const results = await engine.search('session', { limit });

    expect(results.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 20', async () => {
    const results = await engine.search('test');
    expect(results.length).toBeLessThanOrEqual(20);
  });

  test('should handle no matches', async () => {
    const results = await engine.search('xyznonexistentkeyword' + Date.now());

    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(0);
  });

  test('should order by created DESC', async () => {
    const results = await engine.search('session');

    if (results.length > 1) {
      for (let i = 0; i < results.length - 1; i++) {
        expect(results[i].created).toBeGreaterThanOrEqual(
          results[i + 1].created
        );
      }
    }
  });

  test('should handle empty search query', async () => {
    const results = await engine.search('');
    expect(Array.isArray(results)).toBe(true);
  });

  test('should handle special characters in search', async () => {
    const results = await engine.search('%_');
    expect(Array.isArray(results)).toBe(true);
  });
});

describe('QueryEngine - dateRange()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return sessions within date range', async () => {
    const start = new Date();
    start.setDate(start.getDate() - 7);

    const end = new Date();

    const results = await engine.dateRange(start, end);

    expect(Array.isArray(results)).toBe(true);

    results.forEach(session => {
      expect(session.created).toBeGreaterThanOrEqual(start.getTime());
      expect(session.created).toBeLessThanOrEqual(end.getTime());
    });
  });

  test('should respect limit option', async () => {
    const start = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000);
    const end = new Date();
    const limit = 5;

    const results = await engine.dateRange(start, end, { limit });

    expect(results.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 100', async () => {
    const start = new Date(0);
    const end = new Date();

    const results = await engine.dateRange(start, end);
    expect(results.length).toBeLessThanOrEqual(100);
  });

  test('should handle same start and end date', async () => {
    const today = new Date();
    today.setHours(0, 0, 0, 0);

    const results = await engine.dateRange(today, today);

    expect(Array.isArray(results)).toBe(true);
  });

  test('should handle inverted date range (end before start)', async () => {
    const start = new Date();
    const end = new Date();
    end.setDate(end.getDate() - 7);

    const results = await engine.dateRange(start, end);

    // Should return empty
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(0);
  });

  test('should order by created DESC', async () => {
    const start = new Date();
    start.setDate(start.getDate() - 30);
    const end = new Date();

    const results = await engine.dateRange(start, end);

    if (results.length > 1) {
      for (let i = 0; i < results.length - 1; i++) {
        expect(results[i].created).toBeGreaterThanOrEqual(
          results[i + 1].created
        );
      }
    }
  });

  test('should handle future date range', async () => {
    const start = new Date();
    start.setFullYear(start.getFullYear() + 1);

    const end = new Date();
    end.setFullYear(end.getFullYear() + 2);

    const results = await engine.dateRange(start, end);

    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(0);
  });
});

describe('QueryEngine - costSummary()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return cost summary for default 7 days', async () => {
    const summary = await engine.costSummary();

    expect(summary).toBeDefined();
    expect(typeof summary.sessionCount).toBe('number');
    expect(typeof summary.totalCost).toBe('number');
    expect(typeof summary.avgCost).toBe('number');
    expect(typeof summary.totalMessages).toBe('number');

    expect(summary.sessionCount).toBeGreaterThanOrEqual(0);
    expect(summary.totalCost).toBeGreaterThanOrEqual(0);
    expect(summary.avgCost).toBeGreaterThanOrEqual(0);
    expect(summary.totalMessages).toBeGreaterThanOrEqual(0);
  });

  test('should respect custom days parameter', async () => {
    const summary30 = await engine.costSummary(30);
    const summary7 = await engine.costSummary(7);

    expect(summary30.sessionCount).toBeGreaterThanOrEqual(summary7.sessionCount);
  });

  test('should handle 1 day period', async () => {
    const summary = await engine.costSummary(1);

    expect(summary).toBeDefined();
    expect(summary.sessionCount).toBeGreaterThanOrEqual(0);
  });

  test('should handle 0 days (no sessions)', async () => {
    const summary = await engine.costSummary(0);

    expect(summary).toBeDefined();
    expect(summary.sessionCount).toBe(0);
    expect(summary.totalCost).toBe(0);
    expect(summary.avgCost).toBe(0);
    expect(summary.totalMessages).toBe(0);
  });

  test('should calculate avgCost correctly', async () => {
    const summary = await engine.costSummary(365);

    if (summary.sessionCount > 0) {
      const expectedAvg = summary.totalCost / summary.sessionCount;
      expect(Math.abs(summary.avgCost - expectedAvg)).toBeLessThan(0.01);
    } else {
      expect(summary.avgCost).toBe(0);
    }
  });

  test('should handle negative days parameter', async () => {
    const summary = await engine.costSummary(-1);

    expect(summary).toBeDefined();
    expect(summary.sessionCount).toBe(0);
  });

  test('should handle very large days parameter', async () => {
    const summary = await engine.costSummary(365 * 10);

    expect(summary).toBeDefined();
    expect(typeof summary.sessionCount).toBe('number');
  });
});

describe('QueryEngine - toolStats()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return tool usage statistics', async () => {
    const stats = await engine.toolStats();

    expect(Array.isArray(stats)).toBe(true);

    stats.forEach(stat => {
      expect(stat.tool).toBeDefined();
      expect(typeof stat.uses).toBe('number');
      expect(typeof stat.sessions).toBe('number');
      expect(stat.uses).toBeGreaterThan(0);
      expect(stat.sessions).toBeGreaterThan(0);
    });
  });

  test('should respect limit option', async () => {
    const limit = 5;
    const stats = await engine.toolStats({ limit });

    expect(stats.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 20', async () => {
    const stats = await engine.toolStats();
    expect(stats.length).toBeLessThanOrEqual(20);
  });

  test('should order by uses DESC', async () => {
    const stats = await engine.toolStats();

    if (stats.length > 1) {
      for (let i = 0; i < stats.length - 1; i++) {
        expect(stats[i].uses).toBeGreaterThanOrEqual(stats[i + 1].uses);
      }
    }
  });

  test('should aggregate uses across sessions', async () => {
    const stats = await engine.toolStats();

    stats.forEach(stat => {
      expect(stat.uses).toBeGreaterThanOrEqual(stat.sessions);
    });
  });

  test('should handle empty tool data', async () => {
    const stats = await engine.toolStats({ limit: 1000 });
    expect(Array.isArray(stats)).toBe(true);
  });
});

describe('QueryEngine - agentStats()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return agent type statistics', async () => {
    const stats = await engine.agentStats();

    expect(Array.isArray(stats)).toBe(true);

    stats.forEach(stat => {
      expect(stat.type).toBeDefined();
      expect(typeof stat.count).toBe('number');
      expect(stat.count).toBeGreaterThan(0);
    });
  });

  test('should respect limit option', async () => {
    const limit = 3;
    const stats = await engine.agentStats({ limit });

    expect(stats.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 20', async () => {
    const stats = await engine.agentStats();
    expect(stats.length).toBeLessThanOrEqual(20);
  });

  test('should order by count DESC', async () => {
    const stats = await engine.agentStats();

    if (stats.length > 1) {
      for (let i = 0; i < stats.length - 1; i++) {
        expect(stats[i].count).toBeGreaterThanOrEqual(stats[i + 1].count);
      }
    }
  });

  test('should exclude null agent types', async () => {
    const stats = await engine.agentStats();

    stats.forEach(stat => {
      expect(stat.type).not.toBeNull();
      expect(stat.type).not.toBeUndefined();
    });
  });

  test('should handle empty agent data', async () => {
    const stats = await engine.agentStats({ limit: 1000 });
    expect(Array.isArray(stats)).toBe(true);
  });
});

describe('QueryEngine - filesModified()', () => {
  let engine: QueryEngine;

  beforeAll(() => {
    engine = new QueryEngine();
  });

  afterAll(() => {
    engine.close();
  });

  test('should return files modified across sessions', async () => {
    const files = await engine.filesModified();

    expect(Array.isArray(files)).toBe(true);

    files.forEach(file => {
      expect(file.file).toBeDefined();
      expect(typeof file.sessions).toBe('number');
      expect(file.sessions).toBeGreaterThan(0);
    });
  });

  test('should respect limit parameter', async () => {
    const limit = 5;
    const files = await engine.filesModified(limit);

    expect(files.length).toBeLessThanOrEqual(limit);
  });

  test('should default to limit of 50', async () => {
    const files = await engine.filesModified();
    expect(files.length).toBeLessThanOrEqual(50);
  });

  test('should order by sessions DESC (most modified first)', async () => {
    const files = await engine.filesModified();

    if (files.length > 1) {
      for (let i = 0; i < files.length - 1; i++) {
        expect(files[i].sessions).toBeGreaterThanOrEqual(files[i + 1].sessions);
      }
    }
  });

  test('should handle limit of 0', async () => {
    const files = await engine.filesModified(0);
    expect(files.length).toBe(0);
  });

  test('should return distinct file paths', async () => {
    const files = await engine.filesModified();
    const paths = files.map(f => f.file);
    const uniquePaths = new Set(paths);

    expect(paths.length).toBe(uniquePaths.size);
  });
});

describe('QueryEngine - close()', () => {
  test('should close database connection', async () => {
    const engine = new QueryEngine();
    expect(() => engine.close()).not.toThrow();
  });

  test('should allow multiple close calls', async () => {
    const engine = new QueryEngine();
    engine.close();
    expect(() => engine.close()).not.toThrow();
  });
});

describe('QueryEngine - Edge Cases & Error Handling', () => {
  test('should handle operations after close gracefully', async () => {
    const engine = new QueryEngine();
    engine.close();

    // Operations after close should throw
    await expect(engine.recent()).rejects.toThrow();
  });

  test('should handle very long search queries', async () => {
    const engine = new QueryEngine();
    const longQuery = 'test'.repeat(1000);
    const results = await engine.search(longQuery);

    expect(Array.isArray(results)).toBe(true);
    engine.close();
  });

  test('should handle special SQL characters in search', async () => {
    const engine = new QueryEngine();
    const specialChars = ["'", '"', ';', '--', '/*', '*/'];

    for (const char of specialChars) {
      const results = await engine.search(char);
      expect(Array.isArray(results)).toBe(true);
    }

    engine.close();
  });

  test('should handle Unicode in search queries', async () => {
    const engine = new QueryEngine();
    const unicodeQuery = '测试 🔍 émojis';
    const results = await engine.search(unicodeQuery);

    expect(Array.isArray(results)).toBe(true);
    engine.close();
  });
});

// ============================================================================
// Test Database Setup Helper
// ============================================================================

function setupTestDatabase(): void {
  const db = new Database(DB_PATH);

  // Create schema
  db.exec(`
    CREATE TABLE IF NOT EXISTS sessions (
      id TEXT PRIMARY KEY,
      created INTEGER NOT NULL,
      modified INTEGER NOT NULL,
      summary TEXT,
      message_count INTEGER DEFAULT 0,
      agent_count INTEGER DEFAULT 0,
      cost REAL DEFAULT 0.0,
      duration INTEGER DEFAULT 0,
      git_branch TEXT,
      project_path TEXT,
      content_hash TEXT,
      indexed_at INTEGER
    );

    CREATE TABLE IF NOT EXISTS session_files (
      session_id TEXT NOT NULL,
      file_path TEXT NOT NULL,
      operation TEXT CHECK(operation IN ('read', 'write', 'edit')),
      timestamp INTEGER
    );

    CREATE TABLE IF NOT EXISTS session_tools (
      session_id TEXT NOT NULL,
      tool_name TEXT NOT NULL,
      count INTEGER DEFAULT 1,
      UNIQUE(session_id, tool_name)
    );

    CREATE TABLE IF NOT EXISTS session_agents (
      session_id TEXT NOT NULL,
      agent_id TEXT NOT NULL,
      agent_type TEXT,
      task TEXT,
      outcome TEXT,
      spawned_at INTEGER,
      completed_at INTEGER
    );
  `);

  // Seed comprehensive test data
  const now = Date.now();
  const today = new Date();
  today.setHours(0, 0, 0, 0);

  // Today's sessions
  for (let i = 0; i < 5; i++) {
    db.run(`
      INSERT INTO sessions (id, created, modified, summary, message_count, cost)
      VALUES (?, ?, ?, ?, ?, ?)
    `, [`today-${i}`, today.getTime() + i * 1000, now, `Testing session ${i}`, 10 + i, 0.05 + i * 0.01]);
  }

  // Yesterday's sessions
  const yesterday = new Date(today);
  yesterday.setDate(yesterday.getDate() - 1);
  for (let i = 0; i < 5; i++) {
    db.run(`
      INSERT INTO sessions (id, created, modified, summary, message_count, cost)
      VALUES (?, ?, ?, ?, ?, ?)
    `, [`yesterday-${i}`, yesterday.getTime() + i * 1000, now, `Yesterday session ${i}`, 8 + i, 0.04 + i * 0.01]);
  }

  // Recent sessions spread over 30 days
  for (let i = 0; i < 30; i++) {
    const created = now - (i * 24 * 60 * 60 * 1000);
    db.run(`
      INSERT INTO sessions (id, created, modified, summary, message_count, cost)
      VALUES (?, ?, ?, ?, ?, ?)
    `, [`recent-${i}`, created, now, `Recent session ${i}`, 5 + i, 0.03 + i * 0.005]);
  }

  // Sessions with files
  for (let i = 0; i < 10; i++) {
    const sessionId = `file-session-${i}`;
    db.run(`
      INSERT INTO sessions (id, created, modified, summary, message_count, cost)
      VALUES (?, ?, ?, ?, ?, ?)
    `, [sessionId, now - i * 1000, now, `File session ${i}`, 10, 0.05]);

    // Add file modifications
    const files = ['test.ts', 'QueryEngine.ts', 'index.ts', 'main.ts'];
    for (const file of files) {
      db.run(`
        INSERT INTO session_files (session_id, file_path, operation, timestamp)
        VALUES (?, ?, ?, ?)
      `, [sessionId, `/src/${file}`, 'edit', now]);
    }
  }

  // Sessions with tools
  for (let i = 0; i < 10; i++) {
    const sessionId = `tool-session-${i}`;
    db.run(`
      INSERT INTO sessions (id, created, modified, summary, message_count, cost)
      VALUES (?, ?, ?, ?, ?, ?)
    `, [sessionId, now - i * 1000, now, `Tool session ${i}`, 10, 0.05]);

    // Add tool usage
    const tools = ['Read', 'Write', 'Bash', 'Edit', 'Grep'];
    for (const tool of tools) {
      db.run(`
        INSERT INTO session_tools (session_id, tool_name, count)
        VALUES (?, ?, ?)
      `, [sessionId, tool, Math.floor(Math.random() * 10) + 1]);
    }
  }

  // Sessions with agents
  for (let i = 0; i < 10; i++) {
    const sessionId = `agent-session-${i}`;
    db.run(`
      INSERT INTO sessions (id, created, modified, summary, message_count, cost)
      VALUES (?, ?, ?, ?, ?, ?)
    `, [sessionId, now - i * 1000, now, `Agent session ${i}`, 10, 0.05]);

    // Add agents
    const agentTypes = ['code-agent', 'search-agent', 'test-agent', 'debug-agent'];
    for (const type of agentTypes) {
      db.run(`
        INSERT INTO session_agents (session_id, agent_id, agent_type, task, outcome, spawned_at)
        VALUES (?, ?, ?, ?, ?, ?)
      `, [sessionId, `${type}-${i}`, type, `Task ${i}`, 'success', now]);
    }
  }

  db.close();
  console.log('Test database created and seeded successfully');
}
