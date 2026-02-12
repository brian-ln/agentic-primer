#!/usr/bin/env bun
/**
 * Semantic search CLI
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { Database } from 'bun:sqlite';
import { join } from 'path';
import { EmbeddingGenerator } from '../embeddings/EmbeddingGenerator';
import { VectorStore } from '../embeddings/VectorStore';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions.db');

// Set custom SQLite for extension support on macOS
try {
  Database.setCustomSQLite("/opt/homebrew/opt/sqlite/lib/libsqlite3.dylib");
} catch {
  try {
    Database.setCustomSQLite("/usr/local/opt/sqlite/lib/libsqlite3.dylib");
  } catch {}
}

async function main() {
  const query = process.argv.slice(2).join(' ');

  if (!query) {
    console.error('Usage: search-semantic <query>');
    console.error('\nExample: search-semantic "how did we handle authentication?"');
    process.exit(1);
  }

  const db = new Database(DB_PATH, { readonly: true });
  const generator = new EmbeddingGenerator();
  const vectorStore = new VectorStore(db);

  await vectorStore.load();

  console.log(`\n🔍 Semantic search: "${query}"\n`);

  // Generate query embedding
  const queryEmbedding = await generator.embed(query);

  // Search sessions
  const sessionResults = vectorStore.searchSessions(queryEmbedding, 5);

  if (sessionResults.length === 0) {
    console.log('No sessions with embeddings found. Run embeddings first:\n');
    console.log('  bun run src/session-knowledge/embeddings/SessionEmbeddingIndexer.ts sessions\n');
    db.close();
    return;
  }

  console.log('📊 Similar Sessions:\n');
  for (const result of sessionResults) {
    const similarity = ((1 - result.distance) * 100).toFixed(1);
    const date = new Date(result.timestamp).toLocaleString();
    console.log(`  ${similarity}% | ${date} | ${result.id.slice(0, 8)}`);
    console.log(`       ${result.content.slice(0, 100)}...`);
    console.log();
  }

  // Search messages
  const messageResults = vectorStore.searchMessages(queryEmbedding, 5);

  if (messageResults.length > 0) {
    console.log('\n💬 Similar Messages:\n');
    for (const result of messageResults) {
      const similarity = ((1 - result.distance) * 100).toFixed(1);
      const date = new Date(result.timestamp!).toLocaleString();
      console.log(`  ${similarity}% | ${date} | ${result.sessionId?.slice(0, 8)}`);
      console.log(`       ${result.content.slice(0, 150)}...`);
      console.log();
    }
  }

  db.close();
}

main();
