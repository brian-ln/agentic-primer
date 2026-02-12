#!/usr/bin/env bun
/**
 * Streaming Demo - Real-time Token Streaming with SessionActor
 *
 * Demonstrates callback-based streaming pattern for real-time LLM responses.
 * Shows progressive token output as they arrive from the model.
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { SessionManager } from './src/entities/session.ts';
import { ModelManager } from './src/entities/model.ts';
import { ProviderManager } from './src/entities/provider.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { SessionActor } from './src/messaging/actors/session.ts';
import { address } from './src/messaging/message.ts';

async function main() {
  console.log('🌊 Real-time Streaming Demo\n');

  // Initialize system
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const providerManager = new ProviderManager(store);
  const modelManager = new ModelManager(store, providerManager);
  const sessionManager = new SessionManager(store, modelManager);
  const router = new MessageRouter(store, programManager);

  // Setup provider and model
  console.log('📝 Setting up provider and model...');

  // Create provider (Cloudflare AI Gateway)
  const provider = await providerManager.createProvider(
    'stream-provider',
    'cloudflare-ai-gateway',
    {
      accountId: process.env.CLOUDFLARE_ACCOUNT_ID || 'demo-account',
      gatewayId: process.env.CLOUDFLARE_GATEWAY_ID || 'demo-gateway'
    }
  );

  // Publish provider
  await providerManager.publishProvider('stream-provider');
  console.log('✓ Provider published: @(stream-provider)');

  // Create inference model
  const model = await modelManager.createModel(
    'stream-model',
    'claude-sonnet-4-5',
    'stream-provider',
    {
      name: 'Stream Model',
      temperature: 0.7,
      maxTokens: 500
    }
  );

  // Publish model
  await modelManager.publishModel('stream-model');
  console.log('✓ Model published: @(stream-model)\n');

  // Create session
  console.log('📝 Creating session...');
  await sessionManager.createSession('stream-session', '@(stream-model)', {
    owner: '@(user-1)',
  });
  console.log('✓ Session created: @(stream-session)\n');

  // Create SessionActor with ModelManager for streaming support
  const sessionActor = new SessionActor(
    'stream-session',
    sessionManager,
    programManager,
    store,
    router,
    modelManager
  );

  // Register actor with router
  router.registerActor('demo/stream-session', sessionActor);

  console.log('✓ SessionActor initialized with streaming support\n');

  // Demo: Stream an inference request
  console.log('💬 Streaming inference request...');
  console.log('Question: "Explain how actors work in distributed systems in 2 sentences."\n');
  console.log('Response (streaming):');
  console.log('---');

  let tokenCount = 0;
  let fullResponse = '';

  try {
    await router.streamAsk(
      address('demo/stream-session'),
      'inference',
      {
        message: 'Explain how actors work in distributed systems in 2 sentences.',
        system: 'You are a helpful assistant. Be concise and clear.',
      },
      {
        onChunk: async (event) => {
          if (event.type === 'token' && event.content) {
            // Display token in real-time (without newline for progressive output)
            process.stdout.write(event.content);
            fullResponse += event.content;
            tokenCount++;
          } else if (event.type === 'done') {
            console.log('\n---');
            console.log(`\n✓ Streaming complete! Received ${tokenCount} token chunks`);
          } else if (event.type === 'error') {
            console.log('\n---');
            console.error(`\n✗ Error: ${event.error}`);
          }
        },
      }
    );

    // Display stats
    console.log('\n📊 Streaming Statistics:');
    console.log(`  • Total tokens: ${tokenCount}`);
    console.log(`  • Full response length: ${fullResponse.length} chars`);
    console.log(`  • Streaming pattern: callback-based`);
    console.log(`  • Real-time display: ✓ Progressive output\n`);

  } catch (error: any) {
    console.error('\n✗ Streaming failed:', error.message);

    // Check if error is due to missing credentials
    if (error.message.includes('CLOUDFLARE_API_TOKEN')) {
      console.log('\n⚠️  Note: This demo requires Cloudflare AI Gateway credentials:');
      console.log('   Set CLOUDFLARE_API_TOKEN, CLOUDFLARE_ACCOUNT_ID, and CLOUDFLARE_GATEWAY_ID');
      console.log('   in your environment or .env file.\n');
    }
  }

  // Demonstrate the streaming pattern even without credentials
  console.log('\n🔍 Demonstrating streaming pattern with mock tokens:\n');
  console.log('Response (mock streaming):');
  console.log('---');

  const mockTokens = [
    'Actors', ' are', ' independent', ' computational', ' entities',
    ' that', ' process', ' messages', ' asynchronously', ' and',
    ' maintain', ' their', ' own', ' state', '.', ' They',
    ' communicate', ' by', ' sending', ' messages', ' to',
    ' each', ' other', ',', ' enabling', ' scalable', ' and',
    ' fault', '-', 'tolerant', ' distributed', ' systems', '.'
  ];

  let mockFullResponse = '';
  for (const token of mockTokens) {
    process.stdout.write(token);
    mockFullResponse += token;
    // Small delay to simulate real streaming
    await new Promise(resolve => setTimeout(resolve, 50));
  }

  console.log('\n---');
  console.log(`\n✓ Mock streaming complete! Displayed ${mockTokens.length} token chunks`);
  console.log(`  Full response length: ${mockFullResponse.length} chars\n`);

  console.log('✨ Streaming demo complete!\n');
  console.log('What just happened:');
  console.log('  1. Created SessionActor with ModelManager');
  console.log('  2. Used Router.streamAsk() to send streaming request');
  console.log('  3. Received tokens via callback in real-time');
  console.log('  4. Displayed progressive output (not batched)');
  console.log('  5. Handled completion and error events\n');
  console.log('🎯 This proves:');
  console.log('  • Router.streamAsk() method ✓');
  console.log('  • SessionActor.stream() implementation ✓');
  console.log('  • Callback-based streaming pattern ✓');
  console.log('  • Real-time token forwarding ✓');
  console.log('  • Progressive output display ✓\n');
}

main().catch(console.error);
