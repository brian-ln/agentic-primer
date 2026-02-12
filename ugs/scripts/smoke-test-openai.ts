#!/usr/bin/env bun
/**
 * Smoke test for OpenAI provider
 * Epic: agentic-primer-t49.9
 *
 * Tests:
 * - API key availability
 * - Chat completion
 * - JSON completion
 * - Embeddings
 *
 * Exit codes:
 * 0 - All tests passed
 * 1 - Tests failed
 * 2 - Provider unavailable (no API key)
 */

import { LocalLLMClient } from '../src/session-knowledge/classification/LocalLLMClient';
import { EmbeddingGenerator } from '../src/session-knowledge/embeddings/EmbeddingGenerator';

const BASE_URL = 'https://api.openai.com/v1';
const CHAT_MODEL = 'gpt-4o-mini';
const EMBEDDING_MODEL = 'text-embedding-3-small';

// Color output helpers
const green = (s: string) => `\x1b[32m${s}\x1b[0m`;
const red = (s: string) => `\x1b[31m${s}\x1b[0m`;
const yellow = (s: string) => `\x1b[33m${s}\x1b[0m`;
const blue = (s: string) => `\x1b[34m${s}\x1b[0m`;

async function main() {
  console.log(blue('\n🧪 OpenAI Smoke Test'));
  console.log(`   Provider: OpenAI API`);
  console.log(`   Chat Model: ${CHAT_MODEL}`);
  console.log(`   Embedding Model: ${EMBEDDING_MODEL}`);
  console.log(yellow('   ⚠️  Warning: This test will incur small API costs (~$0.001)\n'));

  let testsPassed = 0;
  let testsFailed = 0;

  // Test 1: Check API key
  process.stdout.write('   Checking API key... ');
  const apiKey = process.env.OPENAI_API_KEY;
  if (!apiKey) {
    console.log(yellow('⏭️  SKIP'));
    console.log(yellow('\n   OPENAI_API_KEY environment variable not set'));
    console.log('   Set your API key to run this test:');
    console.log('   export OPENAI_API_KEY="sk-..."\n');
    process.exit(2);
  }
  console.log(green('✅ PASS'));
  testsPassed++;

  // Test 2: Chat completion
  process.stdout.write('   Testing chat completion... ');
  try {
    const client = new LocalLLMClient({
      baseUrl: BASE_URL,
      model: CHAT_MODEL
    });

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
    const client = new LocalLLMClient({
      baseUrl: BASE_URL,
      model: CHAT_MODEL
    });

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
      apiKey: apiKey,
      baseUrl: BASE_URL,
      model: EMBEDDING_MODEL,
      dimensions: 1536
    });

    const embedding = await embedder.embed('test text');

    if (!(embedding instanceof Float32Array)) {
      throw new Error('Invalid embedding type');
    }

    if (embedding.length !== 1536) {
      throw new Error(`Expected 1536 dimensions, got ${embedding.length}`);
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
    console.log(green(`   ✅ All tests passed (${testsPassed}/${testsPassed})`));
    console.log(yellow('   💰 Estimated cost: ~$0.001\n'));
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
