#!/usr/bin/env bun
/**
 * Integration test for streaming functionality
 * Tests all components of the streaming pattern
 */

import GraphStore from './src/graph.ts';
import { ProgramManager } from './src/entities/program.ts';
import { SessionManager } from './src/entities/session.ts';
import { ModelManager } from './src/entities/model.ts';
import { ProviderManager } from './src/entities/provider.ts';
import { MessageRouter } from './src/messaging/router.ts';
import { SessionActor } from './src/messaging/actors/session.ts';
import { Actor } from './src/messaging/actor.ts';
import {
  address,
  type StreamCallback,
  type TokenStreamEvent,
} from './src/messaging/message.ts';

// Custom actor with streaming support for testing
class MockStreamingActor extends Actor {
  async receive(message: any) {
    return {
      id: 'resp',
      correlationId: message.id,
      from: this.address,
      to: message.from || this.address,
      success: true,
      payload: { message: 'Received' },
      timestamp: Date.now(),
    };
  }

  async stream(payload: any, onChunk: StreamCallback<TokenStreamEvent>) {
    // Simulate streaming 3 tokens
    const tokens = ['Hello', ' ', 'World'];
    for (const token of tokens) {
      await onChunk({
        type: 'token',
        content: token,
        timestamp: Date.now(),
      });
      await new Promise(resolve => setTimeout(resolve, 10));
    }
    await onChunk({ type: 'done', timestamp: Date.now() });
  }
}

async function test() {
  console.log('🧪 Streaming Integration Test\n');

  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);

  // Test 1: Router.streamAsk() exists
  console.log('✓ Test 1: Router.streamAsk() method exists');
  if (typeof router.streamAsk !== 'function') {
    throw new Error('Router.streamAsk() method not found!');
  }

  // Test 2: Actor.stream() interface exists
  console.log('✓ Test 2: Actor.stream() optional method interface exists');
  const testActor = new MockStreamingActor('test-actor', router);
  if (typeof testActor.stream !== 'function') {
    throw new Error('Actor.stream() method not found!');
  }

  // Test 3: Custom actor streaming works
  console.log('✓ Test 3: Custom actor streaming works');
  router.registerActor('test/actor', testActor);

  let receivedTokens: string[] = [];
  let completed = false;

  await router.streamAsk(
    address('test/actor'),
    'test',
    { message: 'Test' },
    {
      onChunk: async (event) => {
        if (event.type === 'token' && event.content) {
          receivedTokens.push(event.content);
        } else if (event.type === 'done') {
          completed = true;
        }
      },
    }
  );

  if (receivedTokens.length !== 3 || !completed) {
    throw new Error('Streaming did not work correctly!');
  }
  console.log(`  Received tokens: ${receivedTokens.join('')}`);

  // Test 4: SessionActor has stream method
  console.log('✓ Test 4: SessionActor.stream() method exists');
  const providerManager = new ProviderManager(store);
  const modelManager = new ModelManager(store, providerManager);
  const sessionManager = new SessionManager(store, modelManager);

  await providerManager.createProvider('test-provider', 'cloudflare-ai-gateway', {
    accountId: 'test',
    gatewayId: 'test',
  });
  await providerManager.publishProvider('test-provider');

  await modelManager.createModel('test-model', 'claude-sonnet-4-5', 'test-provider', {
    name: 'Test Model',
  });
  await modelManager.publishModel('test-model');

  await sessionManager.createSession('test-session', '@(test-model)', {});

  const sessionActor = new SessionActor(
    'test-session',
    sessionManager,
    programManager,
    store,
    router,
    modelManager
  );

  if (typeof sessionActor.stream !== 'function') {
    throw new Error('SessionActor.stream() method not found!');
  }

  // Test 5: SessionActor.stream() handles errors gracefully
  console.log('✓ Test 5: SessionActor.stream() handles errors gracefully');
  let errorReceived = false;

  await sessionActor.stream(
    { message: 'Test message' },
    async (event) => {
      if (event.type === 'error') {
        errorReceived = true;
        console.log(`  Expected error: ${event.error}`);
      }
    }
  );

  if (!errorReceived) {
    throw new Error('Expected error event from SessionActor.stream()');
  }

  // Test 6: Router rejects non-streaming actors
  console.log('✓ Test 6: Router rejects actors without stream() method');
  class NonStreamingActor extends Actor {
    async receive(message: any) {
      return {
        id: 'resp',
        correlationId: message.id,
        from: this.address,
        to: message.from || this.address,
        success: true,
        payload: {},
        timestamp: Date.now(),
      };
    }
  }

  const nonStreamingActor = new NonStreamingActor('non-streaming', router);
  router.registerActor('test/non-streaming', nonStreamingActor);

  try {
    await router.streamAsk(
      address('test/non-streaming'),
      'test',
      {},
      {
        onChunk: async () => {},
      }
    );
    throw new Error('Should have thrown error for non-streaming actor!');
  } catch (error: any) {
    if (!error.message.includes('does not support streaming')) {
      throw new Error('Wrong error message!');
    }
    console.log('  Correctly rejected non-streaming actor');
  }

  console.log('\n🎉 All streaming integration tests passed!\n');
  console.log('Summary:');
  console.log('  ✓ Router.streamAsk() method implemented');
  console.log('  ✓ Actor.stream() interface defined');
  console.log('  ✓ Custom actor streaming works');
  console.log('  ✓ SessionActor.stream() implemented');
  console.log('  ✓ Error handling works correctly');
  console.log('  ✓ Non-streaming actors properly rejected\n');
}

test().catch((error) => {
  console.error('❌ Test failed:', error.message);
  process.exit(1);
});
