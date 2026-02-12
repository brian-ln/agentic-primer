#!/usr/bin/env bun
/**
 * Run database migration
 */
import { createClient } from '@libsql/client';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');
const migrationFile = process.argv[2];

if (!migrationFile) {
  console.error('Usage: bun migrations/run-migration.ts <migration-file>');
  process.exit(1);
}

const migrationPath = join(import.meta.dir, migrationFile);

async function runMigration() {
  const client = createClient({ url: `file:${DB_PATH}` });

  try {
    const sql = await Bun.file(migrationPath).text();
    const statements = sql
      .split(';')
      .map(s => s.trim())
      .filter(s => s.length > 0 && !s.startsWith('--'));

    console.log(`Running migration: ${migrationFile}`);
    console.log(`Statements: ${statements.length}\n`);

    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i];
      try {
        await client.execute(stmt);
        console.log(`✓ [${i + 1}/${statements.length}] ${stmt.slice(0, 60).replace(/\s+/g, ' ')}...`);
      } catch (err: any) {
        if (err.message?.includes('already exists') || err.message?.includes('duplicate')) {
          console.log(`⚠ [${i + 1}/${statements.length}] Already applied`);
        } else {
          throw err;
        }
      }
    }

    console.log('\n✓ Migration complete!');
  } catch (err: any) {
    console.error('\n✗ Migration failed:', err.message);
    process.exit(1);
  } finally {
    await client.close();
  }
}

runMigration();
