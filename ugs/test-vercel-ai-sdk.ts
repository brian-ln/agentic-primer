#!/usr/bin/env bun
/**
 * Test Vercel AI SDK + Cloudflare AI Gateway Integration
 *
 * Verifies the pattern documented in cloudflare skill works with real credentials.
 */

import { generateText, streamText } from 'ai';
import { createAiGateway } from 'ai-gateway-provider';
import { createUnified } from 'ai-gateway-provider/providers/unified';

// Cloudflare credentials from ~/.config/cloudflare/
const ACCOUNT_ID = '8d78f1135e2ebd70b5c8f5dee9d519ff';
const GATEWAY_NAME = 'bln_ai';
const CF_TOKEN = 'uWOcIQa-nyvsU2ZaFaPuxIgqIM5Z_Nzqd7nlKiSx';

console.log('🧪 Testing Vercel AI SDK + Cloudflare AI Gateway\n');

async function testBatchInference() {
  console.log('📝 Test 1: Batch Inference (generateText)');

  try {
    // Create AI Gateway client (Step 1)
    const aigateway = createAiGateway({
      accountId: ACCOUNT_ID,
      gateway: GATEWAY_NAME,
      apiKey: CF_TOKEN,
    });

    // Create unified provider (Step 2)
    const unified = createUnified();

    // Compose them (Step 3)
    const aiModel = aigateway(unified('anthropic/claude-sonnet-4-5'));

    // Use with Vercel AI SDK (Step 4)
    const startTime = Date.now();
    const result = await generateText({
      model: aiModel,
      messages: [
        { role: 'system', content: 'You are a helpful assistant. Be concise.' },
        { role: 'user', content: 'What is 2+2? Answer in one sentence.' }
      ],
      temperature: 0.7,
      maxTokens: 50,
    });

    const duration = Date.now() - startTime;

    console.log('✅ Batch inference successful!');
    console.log(`   Response: ${result.text}`);
    console.log(`   Usage: ${result.usage?.promptTokens} prompt + ${result.usage?.completionTokens} completion = ${result.usage?.totalTokens} total tokens`);
    console.log(`   Duration: ${duration}ms\n`);

    return true;
  } catch (error: any) {
    console.error('❌ Batch inference failed:', error.message);
    console.error('   Stack:', error.stack?.split('\n').slice(0, 3).join('\n'));
    return false;
  }
}

async function testStreamingInference() {
  console.log('🌊 Test 2: Streaming Inference (streamText)');

  try {
    const aigateway = createAiGateway({
      accountId: ACCOUNT_ID,
      gateway: GATEWAY_NAME,
      apiKey: CF_TOKEN,
    });

    const unified = createUnified();
    const aiModel = aigateway(unified('anthropic/claude-sonnet-4-5'));

    const startTime = Date.now();
    const stream = streamText({
      model: aiModel,
      messages: [
        { role: 'user', content: 'Count from 1 to 5, one number per line.' }
      ],
      temperature: 0.5,
      maxTokens: 50,
    });

    console.log('   Streaming output:');
    process.stdout.write('   ');

    let fullText = '';
    let tokenCount = 0;
    for await (const textPart of stream.textStream) {
      process.stdout.write(textPart);
      fullText += textPart;
      tokenCount++;
    }

    const usage = await stream.usage;
    const duration = Date.now() - startTime;

    console.log('\n');
    console.log('✅ Streaming inference successful!');
    console.log(`   Tokens received: ${tokenCount}`);
    console.log(`   Full text length: ${fullText.length} chars`);
    console.log(`   Usage: ${usage?.promptTokens} prompt + ${usage?.completionTokens} completion = ${usage?.totalTokens} total`);
    console.log(`   Duration: ${duration}ms\n`);

    return true;
  } catch (error: any) {
    console.error('❌ Streaming inference failed:', error.message);
    console.error('   Stack:', error.stack?.split('\n').slice(0, 3).join('\n'));
    return false;
  }
}

async function testModelFormatting() {
  console.log('🔧 Test 3: Model Name Formatting');

  const testCases = [
    { input: 'claude-sonnet-4-5', expected: 'anthropic/claude-sonnet-4-5' },
    { input: 'gpt-4o', expected: 'openai/gpt-4o' },
    { input: 'anthropic/claude-sonnet-4-5', expected: 'anthropic/claude-sonnet-4-5' },
  ];

  function formatModelName(modelName: string): string {
    if (modelName.includes('/')) return modelName;

    if (modelName.startsWith('claude')) return `anthropic/${modelName}`;
    if (modelName.startsWith('gpt') || modelName.startsWith('o1')) return `openai/${modelName}`;
    if (modelName.includes('gemini')) return `google-ai-studio/${modelName}`;
    return `openai/${modelName}`;
  }

  let allPassed = true;
  for (const test of testCases) {
    const result = formatModelName(test.input);
    const passed = result === test.expected;
    allPassed = allPassed && passed;

    console.log(`   ${passed ? '✅' : '❌'} "${test.input}" -> "${result}" (expected: "${test.expected}")`);
  }

  console.log();
  return allPassed;
}

async function main() {
  const results = {
    batch: await testBatchInference(),
    streaming: await testStreamingInference(),
    formatting: await testModelFormatting(),
  };

  console.log('📊 Test Results Summary:');
  console.log(`   Batch inference:      ${results.batch ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`   Streaming inference:  ${results.streaming ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`   Model formatting:     ${results.formatting ? '✅ PASS' : '❌ FAIL'}`);

  const allPassed = results.batch && results.streaming && results.formatting;

  if (allPassed) {
    console.log('\n🎉 All tests passed! Pattern is [VERIFIED] with real LLM calls.');
  } else {
    console.log('\n⚠️  Some tests failed. Pattern needs investigation.');
  }

  process.exit(allPassed ? 0 : 1);
}

main().catch((error) => {
  console.error('💥 Unhandled error:', error);
  process.exit(1);
});
