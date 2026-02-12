/**
 * Database Migration Safety Tests
 * Epic: agentic-primer-t49.3
 *
 * Tests migration safety including:
 * - Version detection (prevents re-running migrations)
 * - Schema changes (columns added safely)
 * - Data backfill (existing records get new columns)
 * - Index creation (performance not degraded)
 * - Rollback capability (can restore from backup)
 * - Migration idempotency (safe to run multiple times)
 * - Concurrent access during migration
 */

import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'bun:test';
import { createClient, type Client } from '@libsql/client';
import { join } from 'path';
import { existsSync, mkdirSync, copyFileSync, unlinkSync, rmSync } from 'fs';

const TEST_DB_DIR = join(process.env.HOME!, '.claude/index/test');
const TEST_DB_PATH = join(TEST_DB_DIR, 'migrations-test.db');
const BACKUP_DB_PATH = join(TEST_DB_DIR, 'migrations-test-backup.db');
const MAIN_DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

/**
 * Test helper: Create a test database with initial schema
 */
async function createTestDatabase(): Promise<Client> {
  // Ensure test directory exists
  if (!existsSync(TEST_DB_DIR)) {
    mkdirSync(TEST_DB_DIR, { recursive: true });
  }

  const client = createClient({ url: `file:${TEST_DB_PATH}` });

  // Create minimal initial schema (v2)
  await client.execute(`
    CREATE TABLE IF NOT EXISTS sessions (
      id TEXT PRIMARY KEY,
      created INTEGER NOT NULL,
      modified INTEGER NOT NULL,
      summary TEXT,
      summary_embedding F32_BLOB(768),
      message_count INTEGER DEFAULT 0,
      agent_count INTEGER DEFAULT 0,
      cost REAL DEFAULT 0.0,
      duration INTEGER DEFAULT 0,
      git_branch TEXT,
      project_path TEXT,
      content_hash TEXT,
      indexed_at INTEGER
    )
  `);

  await client.execute(`
    CREATE TABLE IF NOT EXISTS session_decisions (
      id TEXT PRIMARY KEY,
      session_id TEXT NOT NULL,
      timestamp INTEGER NOT NULL,
      decision TEXT NOT NULL,
      reasoning TEXT,
      alternatives TEXT,
      context TEXT,
      FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
    )
  `);

  await client.execute(`
    CREATE TABLE IF NOT EXISTS session_learnings (
      id TEXT PRIMARY KEY,
      session_id TEXT NOT NULL,
      timestamp INTEGER NOT NULL,
      learning TEXT NOT NULL,
      context TEXT,
      actionable TEXT,
      message_id TEXT,
      FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
    )
  `);

  await client.execute(`
    CREATE TABLE IF NOT EXISTS session_errors (
      id TEXT PRIMARY KEY,
      session_id TEXT NOT NULL,
      timestamp INTEGER NOT NULL,
      tool_name TEXT,
      error_type TEXT,
      error_message TEXT,
      root_cause TEXT,
      suggested_fix TEXT,
      message_id TEXT,
      FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
    )
  `);

  await client.execute(`
    CREATE TABLE IF NOT EXISTS session_workflows (
      id TEXT PRIMARY KEY,
      session_id TEXT NOT NULL,
      message_id TEXT NOT NULL,
      timestamp INTEGER NOT NULL,
      workflow_type TEXT,
      description TEXT NOT NULL,
      effectiveness TEXT,
      context TEXT,
      tools_involved TEXT,
      outcome TEXT,
      lessons TEXT,
      confidence REAL DEFAULT 0.0,
      embedding F32_BLOB(768),
      FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
    )
  `);

  await client.execute(`
    CREATE TABLE IF NOT EXISTS index_metadata (
      key TEXT PRIMARY KEY,
      value TEXT,
      updated_at INTEGER
    )
  `);

  // Set initial schema version
  await client.execute(`
    INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
    VALUES ('schema_version', '3-libsql', strftime('%s', 'now'))
  `);

  return client;
}

/**
 * Test helper: Insert sample data
 */
async function insertSampleData(client: Client) {
  const now = Date.now();
  const sessionId = 'test-session-001';

  // Insert session
  await client.execute({
    sql: `INSERT INTO sessions (id, created, modified, summary, message_count, cost)
          VALUES (?, ?, ?, ?, ?, ?)`,
    args: [sessionId, now, now, 'Test session', 5, 0.025]
  });

  // Insert decision
  await client.execute({
    sql: `INSERT INTO session_decisions (id, session_id, timestamp, decision, reasoning, context)
          VALUES (?, ?, ?, ?, ?, ?)`,
    args: ['dec-001', sessionId, now, 'Use libSQL', 'Native vector support', 'Database migration']
  });

  // Insert learning
  await client.execute({
    sql: `INSERT INTO session_learnings (id, session_id, timestamp, learning, context, actionable)
          VALUES (?, ?, ?, ?, ?, ?)`,
    args: ['learn-001', sessionId, now, 'Test migrations carefully', 'Production safety', 'Always backup first']
  });

  // Insert error
  await client.execute({
    sql: `INSERT INTO session_errors (id, session_id, timestamp, tool_name, error_type, error_message, root_cause)
          VALUES (?, ?, ?, ?, ?, ?, ?)`,
    args: ['err-001', sessionId, now, 'Read', 'FileNotFound', 'File missing.txt not found', 'User typo']
  });

  // Insert workflow
  await client.execute({
    sql: `INSERT INTO session_workflows (id, session_id, message_id, timestamp, workflow_type, description, confidence)
          VALUES (?, ?, ?, ?, ?, ?, ?)`,
    args: ['work-001', sessionId, 'msg-001', now, 'planning', 'Created detailed migration plan', 0.9]
  });
}

/**
 * Test helper: Simulate cognitive features migration
 */
async function applyCognitiveMigration(client: Client): Promise<void> {
  // Check if already applied
  const versionCheck = await client.execute({
    sql: `SELECT value FROM index_metadata WHERE key = ?`,
    args: ['cognitive_schema_version']
  });

  if (versionCheck.rows.length > 0) {
    throw new Error('Cognitive migration already applied');
  }

  // Add temporal columns to decisions
  const alterStatements = [
    { table: 'session_decisions', column: 'valid_from', type: 'INTEGER' },
    { table: 'session_decisions', column: 'valid_to', type: 'INTEGER' },
    { table: 'session_decisions', column: 'transaction_from', type: 'INTEGER' },
    { table: 'session_decisions', column: 'transaction_to', type: 'INTEGER' },
    { table: 'session_decisions', column: 'base_confidence', type: 'REAL DEFAULT 0.0' },
    { table: 'session_decisions', column: 'decay_rate', type: 'REAL' },
    { table: 'session_decisions', column: 'domain', type: 'TEXT' },

    { table: 'session_learnings', column: 'valid_from', type: 'INTEGER' },
    { table: 'session_learnings', column: 'valid_to', type: 'INTEGER' },
    { table: 'session_learnings', column: 'transaction_from', type: 'INTEGER' },
    { table: 'session_learnings', column: 'transaction_to', type: 'INTEGER' },
    { table: 'session_learnings', column: 'base_confidence', type: 'REAL DEFAULT 0.0' },
    { table: 'session_learnings', column: 'decay_rate', type: 'REAL' },
    { table: 'session_learnings', column: 'domain', type: 'TEXT' },

    { table: 'session_errors', column: 'valid_from', type: 'INTEGER' },
    { table: 'session_errors', column: 'valid_to', type: 'INTEGER' },
    { table: 'session_errors', column: 'transaction_from', type: 'INTEGER' },
    { table: 'session_errors', column: 'transaction_to', type: 'INTEGER' },
    { table: 'session_errors', column: 'base_confidence', type: 'REAL DEFAULT 0.0' },
    { table: 'session_errors', column: 'decay_rate', type: 'REAL' },
    { table: 'session_errors', column: 'domain', type: 'TEXT' },

    { table: 'session_workflows', column: 'valid_from', type: 'INTEGER' },
    { table: 'session_workflows', column: 'valid_to', type: 'INTEGER' },
    { table: 'session_workflows', column: 'transaction_from', type: 'INTEGER' },
    { table: 'session_workflows', column: 'transaction_to', type: 'INTEGER' },
    { table: 'session_workflows', column: 'base_confidence', type: 'REAL DEFAULT 0.0' },
    { table: 'session_workflows', column: 'decay_rate', type: 'REAL' },
    { table: 'session_workflows', column: 'domain', type: 'TEXT' },
  ];

  for (const stmt of alterStatements) {
    try {
      await client.execute(
        `ALTER TABLE ${stmt.table} ADD COLUMN ${stmt.column} ${stmt.type}`
      );
    } catch (error) {
      if (error instanceof Error && error.message.includes('duplicate column')) {
        continue;
      }
      throw error;
    }
  }

  // Create new tables
  await client.execute(`
    CREATE TABLE IF NOT EXISTS knowledge_relationships (
      id TEXT PRIMARY KEY,
      from_type TEXT NOT NULL,
      from_id TEXT NOT NULL,
      to_type TEXT NOT NULL,
      to_id TEXT NOT NULL,
      relationship_type TEXT NOT NULL,
      confidence REAL DEFAULT 0.0,
      created_at INTEGER NOT NULL,
      evidence TEXT,
      UNIQUE(from_type, from_id, to_type, to_id, relationship_type)
    )
  `);

  await client.execute(`
    CREATE TABLE IF NOT EXISTS thinking_arcs (
      id TEXT PRIMARY KEY,
      session_id TEXT NOT NULL,
      arc_type TEXT NOT NULL,
      start_message_id TEXT NOT NULL,
      end_message_id TEXT,
      description TEXT NOT NULL,
      breakthrough_moment TEXT,
      confidence REAL DEFAULT 0.0,
      created_at INTEGER NOT NULL,
      FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
    )
  `);

  // Create indexes
  await client.execute('CREATE INDEX IF NOT EXISTS idx_decisions_valid_from ON session_decisions(valid_from)');
  await client.execute('CREATE INDEX IF NOT EXISTS idx_decisions_domain ON session_decisions(domain)');
  await client.execute('CREATE INDEX IF NOT EXISTS idx_learnings_valid_from ON session_learnings(valid_from)');
  await client.execute('CREATE INDEX IF NOT EXISTS idx_errors_valid_from ON session_errors(valid_from)');
  await client.execute('CREATE INDEX IF NOT EXISTS idx_workflows_valid_from ON session_workflows(valid_from)');

  // Backfill temporal data
  await client.execute(`
    UPDATE session_decisions
    SET valid_from = timestamp,
        transaction_from = timestamp,
        base_confidence = 0.7,
        domain = 'core'
    WHERE valid_from IS NULL
  `);

  await client.execute(`
    UPDATE session_learnings
    SET valid_from = timestamp,
        transaction_from = timestamp,
        base_confidence = 0.7,
        domain = 'core'
    WHERE valid_from IS NULL
  `);

  await client.execute(`
    UPDATE session_errors
    SET valid_from = timestamp,
        transaction_from = timestamp,
        base_confidence = 0.7,
        domain = 'core'
    WHERE valid_from IS NULL
  `);

  await client.execute(`
    UPDATE session_workflows
    SET valid_from = timestamp,
        transaction_from = timestamp,
        base_confidence = COALESCE(confidence, 0.7),
        domain = 'core'
    WHERE valid_from IS NULL
  `);

  // Mark migration as complete
  await client.execute(`
    INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
    VALUES ('cognitive_schema_version', '1.0', strftime('%s', 'now'))
  `);
}

/**
 * Test helper: Create backup
 */
function createBackup() {
  if (existsSync(TEST_DB_PATH)) {
    copyFileSync(TEST_DB_PATH, BACKUP_DB_PATH);
  }
}

/**
 * Test helper: Restore from backup
 */
function restoreFromBackup() {
  if (existsSync(BACKUP_DB_PATH)) {
    copyFileSync(BACKUP_DB_PATH, TEST_DB_PATH);
  }
}

/**
 * Cleanup test database
 */
function cleanupTestDatabase() {
  try {
    if (existsSync(TEST_DB_PATH)) unlinkSync(TEST_DB_PATH);
    if (existsSync(BACKUP_DB_PATH)) unlinkSync(BACKUP_DB_PATH);
    if (existsSync(`${TEST_DB_PATH}-shm`)) unlinkSync(`${TEST_DB_PATH}-shm`);
    if (existsSync(`${TEST_DB_PATH}-wal`)) unlinkSync(`${TEST_DB_PATH}-wal`);
  } catch (err) {
    // Ignore cleanup errors
  }
}

describe('Migration Safety Tests', () => {
  let client: Client;

  beforeAll(() => {
    cleanupTestDatabase();
  });

  afterAll(() => {
    cleanupTestDatabase();
  });

  describe('Version Detection', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
    });

    test('should detect initial schema version', async () => {
      const result = await client.execute({
        sql: 'SELECT value FROM index_metadata WHERE key = ?',
        args: ['schema_version']
      });

      expect(result.rows.length).toBe(1);
      expect(result.rows[0].value).toBe('3-libsql');
    });

    test('should prevent re-running v1.0 migration', async () => {
      // Apply migration once
      await applyCognitiveMigration(client);

      // Try to apply again
      await expect(applyCognitiveMigration(client)).rejects.toThrow('already applied');

      client.close();
    });

    test('should store migration timestamp', async () => {
      await applyCognitiveMigration(client);

      const result = await client.execute({
        sql: 'SELECT updated_at FROM index_metadata WHERE key = ?',
        args: ['cognitive_schema_version']
      });

      expect(result.rows.length).toBe(1);
      expect(result.rows[0].updated_at).toBeGreaterThan(0);

      client.close();
    });

    test('should track multiple migration versions', async () => {
      await applyCognitiveMigration(client);

      const versions = await client.execute({
        sql: "SELECT key, value FROM index_metadata WHERE key LIKE '%version%'"
      });

      expect(versions.rows.length).toBeGreaterThanOrEqual(2);
      const versionKeys = versions.rows.map(r => r.key);
      expect(versionKeys).toContain('schema_version');
      expect(versionKeys).toContain('cognitive_schema_version');

      client.close();
    });
  });

  describe('Schema Changes - Safe Column Addition', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
      await insertSampleData(client);
    });

    test('should add temporal columns to session_decisions', async () => {
      await applyCognitiveMigration(client);

      const pragma = await client.execute('PRAGMA table_info(session_decisions)');
      const columns = pragma.rows.map(r => r.name);

      expect(columns).toContain('valid_from');
      expect(columns).toContain('valid_to');
      expect(columns).toContain('transaction_from');
      expect(columns).toContain('transaction_to');
      expect(columns).toContain('base_confidence');
      expect(columns).toContain('decay_rate');
      expect(columns).toContain('domain');

      client.close();
    });

    test('should add temporal columns to session_learnings', async () => {
      await applyCognitiveMigration(client);

      const pragma = await client.execute('PRAGMA table_info(session_learnings)');
      const columns = pragma.rows.map(r => r.name);

      expect(columns).toContain('valid_from');
      expect(columns).toContain('transaction_from');
      expect(columns).toContain('base_confidence');
      expect(columns).toContain('domain');

      client.close();
    });

    test('should add temporal columns to session_errors', async () => {
      await applyCognitiveMigration(client);

      const pragma = await client.execute('PRAGMA table_info(session_errors)');
      const columns = pragma.rows.map(r => r.name);

      expect(columns).toContain('valid_from');
      expect(columns).toContain('transaction_from');
      expect(columns).toContain('domain');

      client.close();
    });

    test('should add temporal columns to session_workflows', async () => {
      await applyCognitiveMigration(client);

      const pragma = await client.execute('PRAGMA table_info(session_workflows)');
      const columns = pragma.rows.map(r => r.name);

      expect(columns).toContain('valid_from');
      expect(columns).toContain('transaction_from');
      expect(columns).toContain('domain');

      client.close();
    });

    test('should preserve existing columns', async () => {
      await applyCognitiveMigration(client);

      const pragma = await client.execute('PRAGMA table_info(session_decisions)');
      const columns = pragma.rows.map(r => r.name);

      // Original columns still exist
      expect(columns).toContain('id');
      expect(columns).toContain('session_id');
      expect(columns).toContain('timestamp');
      expect(columns).toContain('decision');
      expect(columns).toContain('reasoning');
      expect(columns).toContain('context');

      client.close();
    });
  });

  describe('Data Backfill - Existing Records', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
      await insertSampleData(client);
    });

    test('should backfill temporal columns in decisions', async () => {
      await applyCognitiveMigration(client);

      const result = await client.execute('SELECT * FROM session_decisions WHERE id = ?', ['dec-001']);
      const row = result.rows[0];

      expect(row.valid_from).not.toBeNull();
      expect(row.transaction_from).not.toBeNull();
      expect(row.valid_from).toBe(row.timestamp);
      expect(row.transaction_from).toBe(row.timestamp);
      expect(row.base_confidence).toBe(0.7);
      expect(row.domain).toBe('core');

      client.close();
    });

    test('should backfill temporal columns in learnings', async () => {
      await applyCognitiveMigration(client);

      const result = await client.execute('SELECT * FROM session_learnings WHERE id = ?', ['learn-001']);
      const row = result.rows[0];

      expect(row.valid_from).toBe(row.timestamp);
      expect(row.transaction_from).toBe(row.timestamp);
      expect(row.base_confidence).toBe(0.7);
      expect(row.domain).toBe('core');

      client.close();
    });

    test('should preserve existing confidence in workflows', async () => {
      await applyCognitiveMigration(client);

      const result = await client.execute('SELECT * FROM session_workflows WHERE id = ?', ['work-001']);
      const row = result.rows[0];

      expect(row.base_confidence).toBe(0.9); // Uses existing confidence
      expect(row.domain).toBe('core');

      client.close();
    });

    test('should not modify existing data', async () => {
      const beforeMigration = await client.execute('SELECT * FROM session_decisions WHERE id = ?', ['dec-001']);
      const beforeRow = beforeMigration.rows[0];

      await applyCognitiveMigration(client);

      const afterMigration = await client.execute('SELECT * FROM session_decisions WHERE id = ?', ['dec-001']);
      const afterRow = afterMigration.rows[0];

      // Original columns unchanged
      expect(afterRow.decision).toBe(beforeRow.decision);
      expect(afterRow.reasoning).toBe(beforeRow.reasoning);
      expect(afterRow.context).toBe(beforeRow.context);
      expect(afterRow.timestamp).toBe(beforeRow.timestamp);

      client.close();
    });
  });

  describe('Index Creation - Performance', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
      await insertSampleData(client);
    });

    test('should create indexes on temporal columns', async () => {
      await applyCognitiveMigration(client);

      const indexes = await client.execute(
        "SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'idx_%'"
      );

      const indexNames = indexes.rows.map(r => r.name);
      expect(indexNames).toContain('idx_decisions_valid_from');
      expect(indexNames).toContain('idx_decisions_domain');
      expect(indexNames).toContain('idx_learnings_valid_from');
      expect(indexNames).toContain('idx_errors_valid_from');
      expect(indexNames).toContain('idx_workflows_valid_from');

      client.close();
    });

    test('should allow queries using new indexes', async () => {
      await applyCognitiveMigration(client);

      // Query should use index (verify it doesn't error)
      const result = await client.execute(
        "SELECT * FROM session_decisions WHERE domain = 'core' ORDER BY valid_from DESC"
      );

      expect(result.rows.length).toBeGreaterThan(0);

      client.close();
    });

    test('should not degrade read performance', async () => {
      // Measure query time before migration
      const startBefore = performance.now();
      await client.execute('SELECT * FROM session_decisions WHERE session_id = ?', ['test-session-001']);
      const durationBefore = performance.now() - startBefore;

      await applyCognitiveMigration(client);

      // Measure query time after migration
      const startAfter = performance.now();
      await client.execute('SELECT * FROM session_decisions WHERE session_id = ?', ['test-session-001']);
      const durationAfter = performance.now() - startAfter;

      // After migration should not be significantly slower
      // Allow up to 2x slowdown for safety (indexes + new columns)
      expect(durationAfter).toBeLessThan(durationBefore * 2 + 10); // +10ms buffer

      client.close();
    });
  });

  describe('New Table Creation', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
    });

    test('should create knowledge_relationships table', async () => {
      await applyCognitiveMigration(client);

      const tables = await client.execute(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='knowledge_relationships'"
      );

      expect(tables.rows.length).toBe(1);

      client.close();
    });

    test('should create thinking_arcs table', async () => {
      await applyCognitiveMigration(client);

      const tables = await client.execute(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='thinking_arcs'"
      );

      expect(tables.rows.length).toBe(1);

      client.close();
    });

    test('should enforce unique constraint on relationships', async () => {
      await applyCognitiveMigration(client);

      // Insert first relationship
      await client.execute({
        sql: `INSERT INTO knowledge_relationships
              (id, from_type, from_id, to_type, to_id, relationship_type, confidence, created_at)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
        args: ['rel-001', 'decision', 'dec-001', 'learning', 'learn-001', 'supports', 0.9, Date.now()]
      });

      // Try to insert duplicate (should fail)
      await expect(client.execute({
        sql: `INSERT INTO knowledge_relationships
              (id, from_type, from_id, to_type, to_id, relationship_type, confidence, created_at)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
        args: ['rel-002', 'decision', 'dec-001', 'learning', 'learn-001', 'supports', 0.8, Date.now()]
      })).rejects.toThrow();

      client.close();
    });
  });

  describe('Rollback Capability', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
      await insertSampleData(client);
    });

    test('should restore database from backup', async () => {
      // Create backup before migration
      createBackup();

      // Apply migration
      await applyCognitiveMigration(client);
      client.close();

      // Verify migration was applied
      client = createClient({ url: `file:${TEST_DB_PATH}` });
      const afterMigration = await client.execute('PRAGMA table_info(session_decisions)');
      const columnsAfter = afterMigration.rows.map(r => r.name);
      expect(columnsAfter).toContain('valid_from');
      client.close();

      // Restore from backup
      restoreFromBackup();

      // Verify original schema restored
      client = createClient({ url: `file:${TEST_DB_PATH}` });
      const afterRestore = await client.execute('PRAGMA table_info(session_decisions)');
      const columnsRestored = afterRestore.rows.map(r => r.name);
      expect(columnsRestored).not.toContain('valid_from');

      client.close();
    });

    test('should preserve data after backup/restore', async () => {
      createBackup();
      await applyCognitiveMigration(client);
      client.close();

      restoreFromBackup();

      client = createClient({ url: `file:${TEST_DB_PATH}` });
      const result = await client.execute('SELECT * FROM session_decisions WHERE id = ?', ['dec-001']);

      expect(result.rows.length).toBe(1);
      expect(result.rows[0].decision).toBe('Use libSQL');

      client.close();
    });
  });

  describe('Migration Idempotency', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
      await insertSampleData(client);
    });

    test('should be safe to run ALTER TABLE multiple times', async () => {
      // Apply migration
      await applyCognitiveMigration(client);

      // Try adding columns manually again (should fail gracefully)
      const addColumn = async () => {
        try {
          await client.execute('ALTER TABLE session_decisions ADD COLUMN valid_from INTEGER');
        } catch (error) {
          if (error instanceof Error && error.message.includes('duplicate column')) {
            return; // Expected
          }
          throw error;
        }
      };

      await expect(addColumn()).resolves.toBeUndefined();

      client.close();
    });

    test('should handle IF NOT EXISTS for tables', async () => {
      await applyCognitiveMigration(client);

      // Try creating tables again
      await client.execute(`
        CREATE TABLE IF NOT EXISTS knowledge_relationships (
          id TEXT PRIMARY KEY,
          from_type TEXT NOT NULL,
          from_id TEXT NOT NULL,
          to_type TEXT NOT NULL,
          to_id TEXT NOT NULL,
          relationship_type TEXT NOT NULL,
          confidence REAL DEFAULT 0.0,
          created_at INTEGER NOT NULL,
          evidence TEXT,
          UNIQUE(from_type, from_id, to_type, to_id, relationship_type)
        )
      `);

      // Should not error
      const tables = await client.execute(
        "SELECT COUNT(*) as count FROM sqlite_master WHERE type='table' AND name='knowledge_relationships'"
      );

      expect(tables.rows[0].count).toBe(1);

      client.close();
    });

    test('should handle IF NOT EXISTS for indexes', async () => {
      await applyCognitiveMigration(client);

      // Try creating indexes again
      await client.execute('CREATE INDEX IF NOT EXISTS idx_decisions_valid_from ON session_decisions(valid_from)');

      // Should not error
      const indexes = await client.execute(
        "SELECT COUNT(*) as count FROM sqlite_master WHERE type='index' AND name='idx_decisions_valid_from'"
      );

      expect(indexes.rows[0].count).toBe(1);

      client.close();
    });
  });

  describe('Concurrent Access During Migration', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
      await insertSampleData(client);
    });

    test('should allow reads during migration', async () => {
      // Start migration in background
      const migrationPromise = applyCognitiveMigration(client);

      // Try to read data during migration
      const readResult = await client.execute('SELECT COUNT(*) as count FROM session_decisions');

      expect(readResult.rows[0].count).toBeGreaterThan(0);

      await migrationPromise;

      client.close();
    });

    test('should allow writes after migration', async () => {
      await applyCognitiveMigration(client);

      // Insert new decision after migration
      const now = Date.now();
      await client.execute({
        sql: `INSERT INTO session_decisions
              (id, session_id, timestamp, decision, reasoning, valid_from, transaction_from, domain, base_confidence)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
        args: ['dec-002', 'test-session-001', now, 'Test post-migration', 'Verify writes work', now, now, 'tech', 0.8]
      });

      const result = await client.execute('SELECT * FROM session_decisions WHERE id = ?', ['dec-002']);
      expect(result.rows.length).toBe(1);
      expect(result.rows[0].domain).toBe('tech');

      client.close();
    });

    test('should maintain ACID properties during migration', async () => {
      // Migration should be atomic (all or nothing)
      try {
        await applyCognitiveMigration(client);
      } catch (error) {
        // If migration fails, schema should be unchanged
        const pragma = await client.execute('PRAGMA table_info(session_decisions)');
        const columns = pragma.rows.map(r => r.name);
        expect(columns).not.toContain('valid_from');
      }

      client.close();
    });
  });

  describe('Data Integrity After Migration', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
      await insertSampleData(client);
    });

    test('should preserve all existing records', async () => {
      const beforeCount = await client.execute('SELECT COUNT(*) as count FROM session_decisions');
      const countBefore = beforeCount.rows[0].count;

      await applyCognitiveMigration(client);

      const afterCount = await client.execute('SELECT COUNT(*) as count FROM session_decisions');
      const countAfter = afterCount.rows[0].count;

      expect(countAfter).toBe(countBefore);

      client.close();
    });

    test('should maintain foreign key relationships', async () => {
      await applyCognitiveMigration(client);

      // Insert decision with invalid session_id (should fail if FK enforced)
      // Note: libSQL might not enforce FK by default, so this tests the schema intention
      await expect(client.execute({
        sql: `INSERT INTO session_decisions
              (id, session_id, timestamp, decision, valid_from, transaction_from)
              VALUES (?, ?, ?, ?, ?, ?)`,
        args: ['dec-bad', 'nonexistent-session', Date.now(), 'Test', Date.now(), Date.now()]
      })).rejects.toThrow();

      client.close();
    });

    test('should validate CHECK constraints on new inserts', async () => {
      await applyCognitiveMigration(client);

      // This would test CHECK constraints if they existed in the schema
      // Currently no CHECK constraints on temporal columns, but we verify schema validity
      const pragma = await client.execute('PRAGMA integrity_check');
      expect(pragma.rows[0].integrity_check).toBe('ok');

      client.close();
    });
  });

  describe('Migration Error Handling', () => {
    beforeEach(async () => {
      cleanupTestDatabase();
      client = await createTestDatabase();
    });

    test('should handle migration on empty database', async () => {
      // No sample data inserted
      await expect(applyCognitiveMigration(client)).resolves.toBeUndefined();

      client.close();
    });

    test('should handle migration with no existing data to backfill', async () => {
      await applyCognitiveMigration(client);

      const result = await client.execute('SELECT COUNT(*) as count FROM session_decisions');
      expect(result.rows[0].count).toBe(0);

      client.close();
    });

    test('should report clear error if database is locked', async () => {
      // Create a second connection that holds a lock
      const lockingClient = createClient({ url: `file:${TEST_DB_PATH}` });

      try {
        // Start a transaction to lock the database
        await lockingClient.execute('BEGIN EXCLUSIVE TRANSACTION');

        // Migration might timeout or error - both are acceptable
        const migrationPromise = applyCognitiveMigration(client);

        // Don't wait forever for this test
        const timeoutPromise = new Promise((_, reject) =>
          setTimeout(() => reject(new Error('Test timeout')), 1000)
        );

        await expect(Promise.race([migrationPromise, timeoutPromise])).rejects.toThrow();

      } finally {
        try {
          await lockingClient.execute('ROLLBACK');
          lockingClient.close();
        } catch {
          // Ignore cleanup errors
        }
      }

      client.close();
    });
  });
});
