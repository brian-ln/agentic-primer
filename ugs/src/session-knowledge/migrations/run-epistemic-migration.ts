#!/usr/bin/env bun
/**
 * Migration Runner: Add Epistemic Gradients
 * Epic: Knowledge Architecture Phase 1 MVP
 * Date: 2026-02-03
 *
 * Applies migration 004-add-epistemic-gradients.sql to add epistemic
 * gradient fields to existing session knowledge tables.
 */

import { createClient } from '@libsql/client';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');
const MIGRATION_PATH = join(import.meta.dir, '004-add-epistemic-gradients.sql');

console.log('🔄 Running epistemic gradients migration...\n');
console.log(`Database: ${DB_PATH}`);
console.log(`Migration: ${MIGRATION_PATH}\n`);

async function runMigration() {
  // Create libSQL client
  const client = createClient({
    url: `file:${DB_PATH}`
  });

  try {
    // Read migration SQL
    const migrationSql = await Bun.file(MIGRATION_PATH).text();

    // Split into individual statements
    const statements = migrationSql
      .split(';')
      .map(s => s.trim())
      .filter(s => s.length > 0 && !s.startsWith('--'));

    console.log(`📋 Executing ${statements.length} migration statements...\n`);

    // Execute each statement
    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i];
      const preview = stmt.slice(0, 80).replace(/\s+/g, ' ');

      try {
        await client.execute(stmt);
        console.log(`  ✓ [${i + 1}/${statements.length}] ${preview}...`);
      } catch (err: any) {
        // Handle "column already exists" errors gracefully
        if (err.message?.includes('duplicate column') || err.message?.includes('already exists')) {
          console.log(`  ⚠ [${i + 1}/${statements.length}] Already applied: ${preview}...`);
        } else {
          console.error(`  ✗ [${i + 1}/${statements.length}] Failed: ${preview}...`);
          throw err;
        }
      }
    }

    console.log('\n✓ Migration completed successfully!\n');

    // Verify schema changes
    console.log('🔍 Verifying schema changes...\n');

    const tables = ['session_decisions', 'session_learnings', 'session_errors'];
    for (const table of tables) {
      const result = await client.execute(`PRAGMA table_info(${table})`);
      const columns = (result.rows as any[]).map((r: any) => r.name);

      const hasEpistemicLevel = columns.includes('epistemic_level');
      const hasConfidence = columns.includes('confidence');
      const hasEvidence = columns.includes('evidence');
      const hasLastValidated = columns.includes('last_validated');

      console.log(`  ${table}:`);
      console.log(`    ${hasEpistemicLevel ? '✓' : '✗'} epistemic_level`);
      console.log(`    ${hasConfidence ? '✓' : '✗'} confidence`);
      console.log(`    ${hasEvidence ? '✓' : '✗'} evidence`);
      console.log(`    ${hasLastValidated ? '✓' : '✗'} last_validated`);

      if (!hasEpistemicLevel || !hasConfidence || !hasEvidence || !hasLastValidated) {
        throw new Error(`Schema verification failed for ${table}`);
      }
    }

    console.log('\n✓ Schema verification passed!\n');

    // Check indexes
    console.log('🔍 Verifying indexes...\n');

    const indexQuery = await client.execute(`
      SELECT name FROM sqlite_master
      WHERE type = 'index'
      AND name LIKE 'idx_%_epistemic'
      OR name LIKE 'idx_%_confidence'
    `);

    const indexes = (indexQuery.rows as any[]).map((r: any) => r.name);
    console.log(`  Found ${indexes.length} epistemic indexes:`);
    indexes.forEach(idx => console.log(`    ✓ ${idx}`));

    console.log('\n✅ Migration complete and verified!\n');
    console.log('Next steps:');
    console.log('  1. Update CLI commands to use epistemic fields');
    console.log('  2. Implement /suspect, /believe, /know commands');
    console.log('  3. Test with sample data\n');

  } catch (err: any) {
    console.error(`\n✗ Migration failed: ${err.message}\n`);
    console.error(err.stack);
    process.exit(1);
  } finally {
    await client.close();
  }
}

runMigration();
