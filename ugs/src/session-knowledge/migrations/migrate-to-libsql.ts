#!/usr/bin/env bun
/**
 * Migration: Switch from SQLite to libSQL with native vectors
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { createClient } from '@libsql/client';
import { join } from 'path';
import { Database } from 'bun:sqlite';

const OLD_DB_PATH = join(process.env.HOME!, '.claude/index/sessions.db');
const NEW_DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

console.log('🔄 Migrating from SQLite to libSQL with native vectors...\n');

// Read schema and data from old SQLite database
const oldDb = new Database(OLD_DB_PATH, { readonly: true });

// Create new libSQL database
const newClient = createClient({
  url: `file:${NEW_DB_PATH}`
});

async function migrate() {
  console.log('📋 Creating libSQL schema with native vector support...');

  // Read and execute schema, replacing BLOB with F32_BLOB for embeddings
  const schemaPath = join(import.meta.dir, '../index/schema.sql');
  let schema = await Bun.file(schemaPath).text();

  // Modify schema for libSQL
  schema = schema.replace(
    'summary_embedding BLOB',
    'summary_embedding F32_BLOB(768)'
  );
  schema = schema.replace(
    'embedding BLOB NOT NULL',
    'embedding F32_BLOB(768) NOT NULL'
  );

  // Execute schema statements one by one
  const statements = schema
    .split(';')
    .map(s => s.trim())
    .filter(s => s.length > 0 && !s.startsWith('--'));

  for (const stmt of statements) {
    try {
      await newClient.execute(stmt);
    } catch (err) {
      // Ignore errors for triggers/indexes that might not apply
      if (!err.message.includes('already exists')) {
        console.log(`  ⚠ Skipped: ${stmt.slice(0, 50)}...`);
      }
    }
  }

  console.log('  ✓ Schema created\n');

  // Copy sessions data
  console.log('📊 Copying sessions...');
  const sessions = oldDb.query(`
    SELECT id, created, modified, summary, message_count, agent_count,
           cost, duration, git_branch, project_path, content_hash, indexed_at
    FROM sessions
  `).all() as any[];

  for (const session of sessions) {
    await newClient.execute({
      sql: `INSERT INTO sessions (id, created, modified, summary, message_count, agent_count,
                                  cost, duration, git_branch, project_path, content_hash, indexed_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      args: [
        session.id, session.created, session.modified, session.summary,
        session.message_count, session.agent_count, session.cost, session.duration,
        session.git_branch, session.project_path, session.content_hash, session.indexed_at
      ]
    });
  }
  console.log(`  ✓ Copied ${sessions.length} sessions\n`);

  // Copy session files
  console.log('📁 Copying session files...');
  const files = oldDb.query('SELECT * FROM session_files').all() as any[];
  for (const file of files) {
    await newClient.execute({
      sql: 'INSERT INTO session_files (session_id, file_path, operation, timestamp) VALUES (?, ?, ?, ?)',
      args: [file.session_id, file.file_path, file.operation, file.timestamp]
    });
  }
  console.log(`  ✓ Copied ${files.length} file entries\n`);

  // Copy session tools
  console.log('🔧 Copying session tools...');
  const tools = oldDb.query('SELECT * FROM session_tools').all() as any[];
  for (const tool of tools) {
    await newClient.execute({
      sql: 'INSERT INTO session_tools (session_id, tool_name, count) VALUES (?, ?, ?)',
      args: [tool.session_id, tool.tool_name, tool.count]
    });
  }
  console.log(`  ✓ Copied ${tools.length} tool entries\n`);

  // Copy session agents
  console.log('🤖 Copying session agents...');
  const agents = oldDb.query('SELECT * FROM session_agents').all() as any[];
  for (const agent of agents) {
    await newClient.execute({
      sql: 'INSERT INTO session_agents (session_id, agent_id, agent_type, task, outcome, spawned_at, completed_at) VALUES (?, ?, ?, ?, ?, ?, ?)',
      args: [agent.session_id, agent.agent_id, agent.agent_type, agent.task, agent.outcome, agent.spawned_at, agent.completed_at]
    });
  }
  console.log(`  ✓ Copied ${agents.length} agent entries\n`);

  // Create vector index for sessions (DiskANN)
  console.log('🔮 Creating vector indexes...');
  await newClient.execute(
    "CREATE INDEX IF NOT EXISTS sessions_vector_idx ON sessions (libsql_vector_idx(summary_embedding, 'metric=cosine'))"
  );
  await newClient.execute(
    "CREATE INDEX IF NOT EXISTS messages_vector_idx ON message_embeddings (libsql_vector_idx(embedding, 'metric=cosine'))"
  );
  console.log('  ✓ Vector indexes created (DiskANN)\n');

  oldDb.close();
  await newClient.close();

  console.log(`✓ Migration complete!

Old database: ${OLD_DB_PATH}
New database: ${NEW_DB_PATH}

Next steps:
1. Update DB_PATH in your code to use: ${NEW_DB_PATH}
2. Re-run embeddings: bun run src/session-knowledge/embeddings/SessionEmbeddingIndexer.ts sessions
3. Test semantic search
`);
}

migrate().catch(err => {
  console.error(`\n✗ Migration failed: ${err.message}`);
  process.exit(1);
});
