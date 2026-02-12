#!/usr/bin/env bun
/**
 * Performance comparison: Database-side vs In-memory similarity calculation
 */
import { MessageCandidateDetector } from './MessageCandidateDetector';
import { createClient } from '@libsql/client';
import { join } from 'path';

const INDEX_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(INDEX_DIR, 'sessions-libsql.db');

const detector = new MessageCandidateDetector();
const client = createClient({ url: `file:${DB_PATH}` });

// Get test embeddings from database
const msgResult = await client.execute(
  'SELECT message_id, embedding FROM message_embeddings LIMIT 20'
);

if (msgResult.rows.length === 0) {
  console.log('No message embeddings found. Run SessionEmbeddingIndexerLibSQL first.');
  process.exit(1);
}

const testEmbeddings = msgResult.rows.map(row => {
  const embRaw = row.embedding;
  return embRaw instanceof ArrayBuffer
    ? new Float32Array(embRaw)
    : embRaw as Float32Array;
});

console.log(`\n⚡ Performance Comparison\n`);
console.log(`Testing with ${testEmbeddings.length} message embeddings\n`);

// Test 1: Database-side (current implementation)
console.log('Test 1: Database-side vector similarity (NEW)');
const dbStart = performance.now();
for (const embedding of testEmbeddings) {
  await detector.detectCandidates(embedding);
}
const dbTime = performance.now() - dbStart;
console.log(`  Time: ${dbTime.toFixed(2)}ms`);
console.log(`  Avg per message: ${(dbTime / testEmbeddings.length).toFixed(2)}ms\n`);

// Test 2: In-memory calculation (old implementation simulation)
console.log('Test 2: In-memory cosine similarity (OLD)');
const protoResult = await client.execute('SELECT category, embedding FROM prototype_embeddings');
const prototypes = protoResult.rows.map(row => ({
  category: row.category as string,
  embedding: row.embedding instanceof ArrayBuffer
    ? new Float32Array(row.embedding)
    : row.embedding as Float32Array
}));

const memStart = performance.now();
for (const embedding of testEmbeddings) {
  const candidates: Array<{category: string, similarity: number}> = [];
  for (const proto of prototypes) {
    const similarity = detector.calculateCosineSimilarity(embedding, proto.embedding);
    if (similarity >= 0.65) {
      candidates.push({ category: proto.category, similarity });
    }
  }
  candidates.sort((a, b) => b.similarity - a.similarity);
}
const memTime = performance.now() - memStart;
console.log(`  Time: ${memTime.toFixed(2)}ms`);
console.log(`  Avg per message: ${(memTime / testEmbeddings.length).toFixed(2)}ms\n`);

// Summary
console.log('📊 Summary');
console.log(`  Database-side: ${dbTime.toFixed(2)}ms`);
console.log(`  In-memory:     ${memTime.toFixed(2)}ms`);
console.log(`  Speedup:       ${(memTime / dbTime).toFixed(2)}x ${dbTime < memTime ? '(faster ✓)' : '(slower)'}`);
console.log(`  Difference:    ${Math.abs(memTime - dbTime).toFixed(2)}ms\n`);

client.close();
await detector.close();
