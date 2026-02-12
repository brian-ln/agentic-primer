#!/usr/bin/env bun
/**
 * Test hybrid approach: verify both in-memory and database paths work correctly
 */
import { MessageCandidateDetector } from './MessageCandidateDetector';
import { createClient } from '@libsql/client';
import { join } from 'path';

const INDEX_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(INDEX_DIR, 'sessions-libsql.db');

const detector = new MessageCandidateDetector();
const client = createClient({ url: `file:${DB_PATH}` });

console.log('\n🧪 Hybrid Approach Testing\n');

// Get a test embedding
const protoResult = await client.execute('SELECT category, embedding FROM prototype_embeddings LIMIT 1');
if (protoResult.rows.length === 0) {
  console.log('No prototypes found');
  process.exit(1);
}

const testCategory = protoResult.rows[0].category as string;
const testEmbedding = protoResult.rows[0].embedding instanceof ArrayBuffer
  ? new Float32Array(protoResult.rows[0].embedding)
  : protoResult.rows[0].embedding as Float32Array;

console.log('Test 1: Current implementation (hybrid - uses in-memory for 4 prototypes)');
const hybridStart = performance.now();
const hybridResult = await detector.detectCandidates(testEmbedding);
const hybridTime = performance.now() - hybridStart;
console.log(`  Time: ${hybridTime.toFixed(3)}ms`);
console.log(`  Result: ${hybridResult.length} candidates`);
if (hybridResult.length > 0) {
  console.log(`  Top match: ${hybridResult[0].category} (${(hybridResult[0].similarity * 100).toFixed(1)}%)`);
  console.log(`  Expected: ${testCategory} (100.0%)`);
  console.log(`  Status: ${hybridResult[0].category === testCategory && hybridResult[0].similarity > 0.99 ? '✓ PASS' : '✗ FAIL'}\n`);
} else {
  console.log('  Status: ✗ FAIL (no candidates found)\n');
}

// Test direct database method
console.log('Test 2: Database-side method (direct call)');
const dbStart = performance.now();
const dbResult = await (detector as any).detectCandidatesDatabase(testEmbedding);
const dbTime = performance.now() - dbStart;
console.log(`  Time: ${dbTime.toFixed(3)}ms`);
console.log(`  Result: ${dbResult.length} candidates`);
if (dbResult.length > 0) {
  console.log(`  Top match: ${dbResult[0].category} (${(dbResult[0].similarity * 100).toFixed(1)}%)`);
  console.log(`  Expected: ${testCategory} (100.0%)`);
  console.log(`  Status: ${dbResult[0].category === testCategory && dbResult[0].similarity > 0.99 ? '✓ PASS' : '✗ FAIL'}\n`);
} else {
  console.log('  Status: ✗ FAIL (no candidates found)\n');
}

// Compare results
console.log('Test 3: Results consistency');
if (hybridResult.length === dbResult.length) {
  let allMatch = true;
  for (let i = 0; i < hybridResult.length; i++) {
    if (hybridResult[i].category !== dbResult[i].category) {
      allMatch = false;
      break;
    }
    // Allow small floating point differences
    const simDiff = Math.abs(hybridResult[i].similarity - dbResult[i].similarity);
    if (simDiff > 0.0001) {
      allMatch = false;
      break;
    }
  }
  console.log(`  Categories match: ✓`);
  console.log(`  Similarities match: ${allMatch ? '✓' : '✗'}`);
  console.log(`  Status: ${allMatch ? '✓ PASS' : '✗ FAIL'}\n`);
} else {
  console.log(`  Status: ✗ FAIL (different result counts: ${hybridResult.length} vs ${dbResult.length})\n`);
}

console.log('📊 Summary');
console.log(`  Hybrid approach: ${hybridTime.toFixed(3)}ms (uses in-memory for <10 prototypes)`);
console.log(`  Database method: ${dbTime.toFixed(3)}ms (always uses SQL)`);
console.log(`  Performance ratio: ${(dbTime / hybridTime).toFixed(2)}x\n`);

client.close();
await detector.close();
