#!/usr/bin/env bun
/**
 * Semantic search CLI with libSQL
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { join } from 'path';
import { EmbeddingGenerator } from '../embeddings/EmbeddingGenerator';
import { VectorStoreLibSQL } from '../embeddings/VectorStoreLibSQL';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

async function main() {
  const query = process.argv.slice(2).join(' ');

  if (!query) {
    console.error('Usage: search-semantic-libsql <query>');
    console.error('\nExample: search-semantic-libsql "how did we handle authentication?"');
    process.exit(1);
  }

  const generator = new EmbeddingGenerator();
  const vectorStore = new VectorStoreLibSQL(DB_PATH);

  console.log(`\n🔍 Semantic search: "${query}"\n`);

  // Generate query embedding
  const queryEmbedding = await generator.embed(query);

  // Search sessions
  const sessionResults = await vectorStore.searchSessions(queryEmbedding, 5);

  if (sessionResults.length === 0) {
    console.log('No sessions with embeddings found. Run embeddings first:\n');
    console.log('  bun run src/session-knowledge/embeddings/SessionEmbeddingIndexerLibSQL.ts sessions\n');
    await vectorStore.close();
    return;
  }

  console.log('📊 Similar Sessions:\n');
  for (const result of sessionResults) {
    const similarity = ((1 - result.distance) * 100).toFixed(1);
    const date = new Date(result.timestamp!).toLocaleString();
    console.log(`  ${similarity}% | ${date} | ${result.id.slice(0, 8)}`);
    console.log(`       ${result.content.slice(0, 100)}...`);
    console.log();
  }

  // Search messages
  const messageResults = await vectorStore.searchMessages(queryEmbedding, 5);

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

  await vectorStore.close();
}

main();
