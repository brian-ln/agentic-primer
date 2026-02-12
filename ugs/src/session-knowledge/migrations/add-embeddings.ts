#!/usr/bin/env bun
/**
 * Migration: Add embedding columns to existing database
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { Database } from 'bun:sqlite';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions.db');

console.log('🔄 Migrating database to add embedding support...');

const db = new Database(DB_PATH);

try {
  // Check current schema version
  const version = db.query(
    "SELECT value FROM index_metadata WHERE key = 'schema_version'"
  ).get() as any;

  console.log(`Current schema version: ${version?.value || '1'}`);

  // Add summary_embedding column if it doesn't exist
  try {
    db.run('ALTER TABLE sessions ADD COLUMN summary_embedding BLOB');
    console.log('  ✓ Added summary_embedding column to sessions table');
  } catch (err) {
    if (err.message.includes('duplicate column')) {
      console.log('  ℹ summary_embedding column already exists');
    } else {
      throw err;
    }
  }

  // Create message_embeddings table
  db.run(`
    CREATE TABLE IF NOT EXISTS message_embeddings (
      message_id TEXT PRIMARY KEY,
      session_id TEXT NOT NULL,
      content TEXT NOT NULL,
      embedding BLOB NOT NULL,
      timestamp INTEGER NOT NULL,
      FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
    )
  `);
  console.log('  ✓ Created message_embeddings table');

  db.run(
    'CREATE INDEX IF NOT EXISTS idx_message_embeddings_session ON message_embeddings(session_id)'
  );
  db.run(
    'CREATE INDEX IF NOT EXISTS idx_message_embeddings_timestamp ON message_embeddings(timestamp)'
  );
  console.log('  ✓ Created indexes on message_embeddings');

  // Update schema version
  db.run(
    `INSERT OR REPLACE INTO index_metadata (key, value, updated_at)
     VALUES ('schema_version', '2', strftime('%s', 'now'))`
  );
  console.log('  ✓ Updated schema version to 2');

  console.log('\n✓ Migration complete!');
} catch (err) {
  console.error(`\n✗ Migration failed: ${err.message}`);
  process.exit(1);
} finally {
  db.close();
}
