#!/usr/bin/env bun
/**
 * Smoke test for LM Studio provider
 * Epic: agentic-primer-t49.9
 *
 * Tests:
 * - Connection availability
 * - Chat completion
 * - JSON completion
 * - Embeddings
 *
 * Exit codes:
 * 0 - All tests passed
 * 1 - Tests failed
 * 2 - Provider unavailable
 */

import { LocalLLMClient } from '../src/session-knowledge/classification/LocalLLMClient';
import { EmbeddingGenerator } from '../src/session-knowledge/embeddings/EmbeddingGenerator';

const PORT = 1234;
const BASE_URL = `http://localhost:${PORT}/v1`;
const CHAT_MODEL = 'llama-3.2-3b-instruct';
const EMBEDDING_MODEL = 'text-embedding-nomic-embed-text-v1.5';

// Color output helpers
const green = (s: string) => `\x1b[32m${s}\x1b[0m`;
const red = (s: string) => `\x1b[31m${s}\x1b[0m`;
const yellow = (s: string) => `\x1b[33m${s}\x1b[0m`;
const blue = (s: string) => `\x1b[34m${s}\x1b[0m`;

async function main() {
  console.log(blue('\n🧪 LM Studio Smoke Test'));
  console.log(`   Provider: LM Studio (port ${PORT})`);
  console.log(`   Chat Model: ${CHAT_MODEL}`);
  console.log(`   Embedding Model: ${EMBEDDING_MODEL}\n`);

  let testsPassed = 0;
  let testsFailed = 0;

  // Test 1: Check availability
  process.stdout.write('   Checking availability... ');
  const client = new LocalLLMClient({
    baseUrl: BASE_URL,
    model: CHAT_MODEL
  });

  const isAvailable = await client.isAvailable();
  if (!isAvailable) {
    console.log(yellow('⏭️  SKIP'));
    console.log(yellow('\n   LM Studio is not running at localhost:1234'));
    console.log('   Start LM Studio and load a model to run this test.\n');
    process.exit(2);
  }
  console.log(green('✅ PASS'));
  testsPassed++;

  // Test 2: Chat completion
  process.stdout.write('   Testing chat completion... ');
  try {
    const response = await client.chat([
      { role: 'system', content: 'You are a helpful assistant.' },
      { role: 'user', content: 'Say "hello" and nothing else.' }
    ]);

    if (!response.content || response.content.trim().length === 0) {
      throw new Error('Empty response');
    }

    console.log(green('✅ PASS'));
    testsPassed++;
  } catch (error) {
    console.log(red('❌ FAIL'));
    console.log(red(`      Error: ${error instanceof Error ? error.message : String(error)}`));
    testsFailed++;
  }

  // Test 3: JSON completion
  process.stdout.write('   Testing JSON completion... ');
  try {
    const result = await client.chatJSON<{ answer: string }>([
      {
        role: 'system',
        content: 'You are a helpful assistant that returns JSON.'
      },
      {
        role: 'user',
        content: 'Return a JSON object with an "answer" field containing "test".'
      }
    ]);

    if (typeof result !== 'object' || !result.answer) {
      throw new Error('Invalid JSON structure');
    }

    console.log(green('✅ PASS'));
    testsPassed++;
  } catch (error) {
    console.log(red('❌ FAIL'));
    console.log(red(`      Error: ${error instanceof Error ? error.message : String(error)}`));
    testsFailed++;
  }

  // Test 4: Embeddings
  process.stdout.write('   Testing embeddings... ');
  try {
    const embedder = new EmbeddingGenerator({
      baseUrl: BASE_URL,
      model: EMBEDDING_MODEL,
      dimensions: 768
    });

    const embedding = await embedder.embed('test text');

    if (!(embedding instanceof Float32Array)) {
      throw new Error('Invalid embedding type');
    }

    if (embedding.length !== 768) {
      throw new Error(`Expected 768 dimensions, got ${embedding.length}`);
    }

    console.log(green('✅ PASS'));
    testsPassed++;
  } catch (error) {
    console.log(red('❌ FAIL'));
    console.log(red(`      Error: ${error instanceof Error ? error.message : String(error)}`));
    testsFailed++;
  }

  // Summary
  console.log();
  if (testsFailed === 0) {
    console.log(green(`   ✅ All tests passed (${testsPassed}/${testsPassed})\n`));
    process.exit(0);
  } else {
    console.log(red(`   ❌ ${testsFailed} test(s) failed (${testsPassed}/${testsPassed + testsFailed} passed)\n`));
    process.exit(1);
  }
}

main().catch(error => {
  console.error(red('\n   ❌ Unexpected error:'));
  console.error(red(`      ${error instanceof Error ? error.message : String(error)}\n`));
  process.exit(1);
});
