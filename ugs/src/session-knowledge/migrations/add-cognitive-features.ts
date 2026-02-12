#!/usr/bin/env bun
/**
 * Migration: Add Cognitive Features
 * Epic: agentic-primer-9ad
 * Phase: Cognitive Integration v1
 *
 * Adds bi-temporal tracking, confidence decay, thinking arcs, and relationships
 * to the existing session knowledge database.
 *
 * SAFETY: All changes are ADDITIVE. No data is deleted or modified.
 */

import { createClient, type Client } from '@libsql/client';
import { join } from 'path';
import { readFileSync } from 'fs';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

class CognitiveMigration {
  private db: Client;

  constructor() {
    this.db = createClient({ url: `file:${DB_PATH}` });
  }

  async migrate() {
    console.log('🧠 Cognitive Integration Migration v1.0\n');
    console.log('Adding:');
    console.log('  ✓ Bi-temporal tracking (valid_time + transaction_time)');
    console.log('  ✓ Confidence decay (domain-specific aging)');
    console.log('  ✓ Knowledge relationships (supports, contradicts, etc.)');
    console.log('  ✓ Thinking arcs (conceptual evolution tracking)\n');

    try {
      // Check if migration already applied
      const versionCheck = await this.db.execute({
        sql: `SELECT value FROM index_metadata WHERE key = ?`,
        args: ['cognitive_schema_version']
      });

      if (versionCheck.rows.length > 0) {
        const version = versionCheck.rows[0].value;
        console.log(`⚠️  Cognitive features already applied (version ${version})`);
        console.log('    Migration skipped to prevent data corruption.\n');
        return;
      }

      // Execute migration in phases to handle table creation order
      console.log('Phase 1: Enhancing existing tables...');
      await this.enhanceExistingTables();

      console.log('\nPhase 2: Creating new tables...');
      await this.createNewTables();

      console.log('\nPhase 3: Creating indexes...');
      await this.createIndexes();

      console.log('\nPhase 4: Creating views...');
      await this.createViews();

      console.log('\n📊 Backfilling temporal data...');
      await this.backfillTemporalData();

      console.log('\n✅ Migration complete!\n');
      await this.showStats();

    } catch (error) {
      console.error('\n❌ Migration failed:', error);
      throw error;
    }
  }

  /**
   * Phase 1: Add columns to existing tables
   */
  private async enhanceExistingTables() {
    const alterStatements = [
      // session_decisions
      { table: 'session_decisions', column: 'valid_from', type: 'INTEGER' },
      { table: 'session_decisions', column: 'valid_to', type: 'INTEGER' },
      { table: 'session_decisions', column: 'transaction_from', type: 'INTEGER' },
      { table: 'session_decisions', column: 'transaction_to', type: 'INTEGER' },
      { table: 'session_decisions', column: 'base_confidence', type: 'REAL DEFAULT 0.0' },
      { table: 'session_decisions', column: 'decay_rate', type: 'REAL' },
      { table: 'session_decisions', column: 'domain', type: 'TEXT' },
      // message_id already exists in schema but let's ensure it

      // session_learnings
      { table: 'session_learnings', column: 'valid_from', type: 'INTEGER' },
      { table: 'session_learnings', column: 'valid_to', type: 'INTEGER' },
      { table: 'session_learnings', column: 'transaction_from', type: 'INTEGER' },
      { table: 'session_learnings', column: 'transaction_to', type: 'INTEGER' },
      { table: 'session_learnings', column: 'base_confidence', type: 'REAL DEFAULT 0.0' },
      { table: 'session_learnings', column: 'decay_rate', type: 'REAL' },
      { table: 'session_learnings', column: 'domain', type: 'TEXT' },
      { table: 'session_learnings', column: 'category', type: 'TEXT' },
      { table: 'session_learnings', column: 'evidence', type: 'TEXT' },
      { table: 'session_learnings', column: 'application', type: 'TEXT' },

      // session_errors
      { table: 'session_errors', column: 'valid_from', type: 'INTEGER' },
      { table: 'session_errors', column: 'valid_to', type: 'INTEGER' },
      { table: 'session_errors', column: 'transaction_from', type: 'INTEGER' },
      { table: 'session_errors', column: 'transaction_to', type: 'INTEGER' },
      { table: 'session_errors', column: 'base_confidence', type: 'REAL DEFAULT 0.0' },
      { table: 'session_errors', column: 'decay_rate', type: 'REAL' },
      { table: 'session_errors', column: 'domain', type: 'TEXT' },
      { table: 'session_errors', column: 'resolution', type: 'TEXT' },
      { table: 'session_errors', column: 'prevention', type: 'TEXT' },

      // session_workflows
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
        await this.db.execute(
          `ALTER TABLE ${stmt.table} ADD COLUMN ${stmt.column} ${stmt.type}`
        );
        console.log(`  ✓ Added ${stmt.table}.${stmt.column}`);
      } catch (error) {
        // Ignore duplicate column errors
        if (error instanceof Error && error.message.includes('duplicate column')) {
          // Column already exists, skip silently
          continue;
        }
        console.warn(`  ⚠️  Warning: Could not add ${stmt.table}.${stmt.column}`);
      }
    }
  }

  /**
   * Phase 2: Create new tables
   */
  private async createNewTables() {
    // Create knowledge_relationships table
    await this.db.execute(`
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
    console.log('  ✓ Created knowledge_relationships');

    // Create thinking_arcs table
    await this.db.execute(`
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
    console.log('  ✓ Created thinking_arcs');
  }

  /**
   * Phase 3: Create indexes
   */
  private async createIndexes() {
    const indexes = [
      // Relationship indexes
      'CREATE INDEX IF NOT EXISTS idx_relationships_from ON knowledge_relationships(from_type, from_id)',
      'CREATE INDEX IF NOT EXISTS idx_relationships_to ON knowledge_relationships(to_type, to_id)',
      'CREATE INDEX IF NOT EXISTS idx_relationships_type ON knowledge_relationships(relationship_type)',

      // Thinking arc indexes
      'CREATE INDEX IF NOT EXISTS idx_thinking_arcs_session ON thinking_arcs(session_id)',
      'CREATE INDEX IF NOT EXISTS idx_thinking_arcs_type ON thinking_arcs(arc_type)',
      'CREATE INDEX IF NOT EXISTS idx_thinking_arcs_created ON thinking_arcs(created_at)',

      // Temporal indexes for decisions
      'CREATE INDEX IF NOT EXISTS idx_decisions_valid_from ON session_decisions(valid_from)',
      'CREATE INDEX IF NOT EXISTS idx_decisions_valid_to ON session_decisions(valid_to)',
      'CREATE INDEX IF NOT EXISTS idx_decisions_transaction_from ON session_decisions(transaction_from)',
      'CREATE INDEX IF NOT EXISTS idx_decisions_domain ON session_decisions(domain)',

      // Temporal indexes for learnings
      'CREATE INDEX IF NOT EXISTS idx_learnings_valid_from ON session_learnings(valid_from)',
      'CREATE INDEX IF NOT EXISTS idx_learnings_valid_to ON session_learnings(valid_to)',
      'CREATE INDEX IF NOT EXISTS idx_learnings_transaction_from ON session_learnings(transaction_from)',
      'CREATE INDEX IF NOT EXISTS idx_learnings_domain ON session_learnings(domain)',
      'CREATE INDEX IF NOT EXISTS idx_learnings_category ON session_learnings(category)',

      // Temporal indexes for errors
      'CREATE INDEX IF NOT EXISTS idx_errors_valid_from ON session_errors(valid_from)',
      'CREATE INDEX IF NOT EXISTS idx_errors_valid_to ON session_errors(valid_to)',
      'CREATE INDEX IF NOT EXISTS idx_errors_transaction_from ON session_errors(transaction_from)',
      'CREATE INDEX IF NOT EXISTS idx_errors_domain ON session_errors(domain)',

      // Temporal indexes for workflows
      'CREATE INDEX IF NOT EXISTS idx_workflows_valid_from ON session_workflows(valid_from)',
      'CREATE INDEX IF NOT EXISTS idx_workflows_valid_to ON session_workflows(valid_to)',
      'CREATE INDEX IF NOT EXISTS idx_workflows_transaction_from ON session_workflows(transaction_from)',
      'CREATE INDEX IF NOT EXISTS idx_workflows_domain ON session_workflows(domain)',
    ];

    for (const indexSql of indexes) {
      await this.db.execute(indexSql);
      const match = indexSql.match(/CREATE INDEX IF NOT EXISTS (\w+)/);
      if (match) {
        console.log(`  ✓ Created index ${match[1]}`);
      }
    }
  }

  /**
   * Phase 4: Create views
   */
  private async createViews() {
    await this.db.execute(`
      CREATE VIEW IF NOT EXISTS current_decisions AS
      SELECT * FROM session_decisions
      WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
        AND (transaction_to IS NULL)
    `);
    console.log('  ✓ Created view current_decisions');

    await this.db.execute(`
      CREATE VIEW IF NOT EXISTS current_learnings AS
      SELECT * FROM session_learnings
      WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
        AND (transaction_to IS NULL)
    `);
    console.log('  ✓ Created view current_learnings');

    await this.db.execute(`
      CREATE VIEW IF NOT EXISTS current_errors AS
      SELECT * FROM session_errors
      WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
        AND (transaction_to IS NULL)
    `);
    console.log('  ✓ Created view current_errors');

    await this.db.execute(`
      CREATE VIEW IF NOT EXISTS current_workflows AS
      SELECT * FROM session_workflows
      WHERE (valid_to IS NULL OR valid_to > strftime('%s', 'now') * 1000)
        AND (transaction_to IS NULL)
    `);
    console.log('  ✓ Created view current_workflows');

    // Update metadata
    await this.db.execute(`
      INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
      VALUES ('cognitive_schema_version', '1.0', strftime('%s', 'now'))
    `);

    await this.db.execute(`
      INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
      VALUES ('cognitive_features', 'bi-temporal,confidence-decay,thinking-arcs,relationships', strftime('%s', 'now'))
    `);
    console.log('  ✓ Updated metadata');
  }

  /**
   * Backfill temporal columns with default values from existing data
   */
  private async backfillTemporalData() {
    // For all existing records:
    // - valid_from = timestamp (when it was created)
    // - valid_to = NULL (still valid)
    // - transaction_from = timestamp (when we learned about it)
    // - transaction_to = NULL (current version)
    // - base_confidence = confidence (if exists) or 0.0
    // - domain = 'core' (default for existing data)

    const tables = [
      { name: 'session_decisions', hasConfidence: false },
      { name: 'session_learnings', hasConfidence: false },
      { name: 'session_errors', hasConfidence: false },
      { name: 'session_workflows', hasConfidence: true } // workflows already has confidence
    ];

    for (const table of tables) {
      try {
        // Set temporal columns
        await this.db.execute(`
          UPDATE ${table.name}
          SET valid_from = timestamp,
              transaction_from = timestamp,
              domain = 'core'
          WHERE valid_from IS NULL
        `);

        // Set base_confidence
        if (table.hasConfidence) {
          await this.db.execute(`
            UPDATE ${table.name}
            SET base_confidence = COALESCE(confidence, 0.0)
            WHERE base_confidence IS NULL OR base_confidence = 0.0
          `);
        } else {
          await this.db.execute(`
            UPDATE ${table.name}
            SET base_confidence = 0.7
            WHERE base_confidence IS NULL OR base_confidence = 0.0
          `);
        }

        const result = await this.db.execute(`SELECT COUNT(*) as count FROM ${table.name}`);
        const count = result.rows[0].count;
        console.log(`  ✓ Backfilled ${count} records in ${table.name}`);
      } catch (error) {
        console.error(`  ⚠️  Warning: Could not backfill ${table.name}:`, error);
      }
    }
  }

  /**
   * Show post-migration statistics
   */
  private async showStats() {
    console.log('Database Statistics:');

    // Count knowledge items
    const decisions = await this.db.execute('SELECT COUNT(*) as count FROM session_decisions');
    const learnings = await this.db.execute('SELECT COUNT(*) as count FROM session_learnings');
    const errors = await this.db.execute('SELECT COUNT(*) as count FROM session_errors');
    const workflows = await this.db.execute('SELECT COUNT(*) as count FROM session_workflows');
    const relationships = await this.db.execute('SELECT COUNT(*) as count FROM knowledge_relationships');
    const arcs = await this.db.execute('SELECT COUNT(*) as count FROM thinking_arcs');

    console.log(`  Decisions:      ${decisions.rows[0].count}`);
    console.log(`  Learnings:      ${learnings.rows[0].count}`);
    console.log(`  Errors:         ${errors.rows[0].count}`);
    console.log(`  Workflows:      ${workflows.rows[0].count}`);
    console.log(`  Relationships:  ${relationships.rows[0].count}`);
    console.log(`  Thinking Arcs:  ${arcs.rows[0].count}`);

    // Check cognitive features
    const features = await this.db.execute({
      sql: `SELECT value FROM index_metadata WHERE key = ?`,
      args: ['cognitive_features']
    });

    if (features.rows.length > 0) {
      console.log(`\nEnabled Features: ${features.rows[0].value}`);
    }

    console.log('\nNew Capabilities:');
    console.log('  ./know temporal "<query>" --as-of="2026-01-15"');
    console.log('  ./know decay --domain=tech --show-rate');
    console.log('  ./know arcs <session-id>');
    console.log('  ./know relationships <knowledge-id>');
  }

  async close() {
    this.db.close();
  }
}

// Run migration
const migration = new CognitiveMigration();
try {
  await migration.migrate();
} catch (error) {
  console.error('Migration failed:', error);
  process.exit(1);
} finally {
  await migration.close();
}
