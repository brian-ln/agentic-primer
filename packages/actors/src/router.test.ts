/**
 * Tests for MessageRouter virtual actor factory routing
 *
 * Covers:
 * - Factory invoked on address miss
 * - Created actor cached (factory not called twice)
 * - Hierarchical prefix fallback
 * - Dead letter when no factory matches
 * - ActorSystem.registerFactory delegates to router
 */

import { describe, it, expect, beforeEach } from 'bun:test';
import { MessageRouter } from './router.ts';
import { ActorSystem } from './actor-system.ts';
import { address, createMessage, createResponse } from './message.ts';
import type { Message, MessageResponse, MessageHandler } from './message.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Create a minimal MessageHandler that records calls and returns success. */
function makeHandler(label: string, calls: string[]): MessageHandler {
  return {
    receive: async (message: Message): Promise<MessageResponse> => {
      calls.push(label);
      return createResponse(message, { handler: label });
    },
  };
}

/** Send a tell message to the router and return the response. */
function sendMsg(router: MessageRouter, targetPath: string): Promise<MessageResponse> {
  const msg = createMessage(
    address(targetPath),
    'test',
    {},
    { pattern: 'tell', from: address('sender') }
  );
  // Use the internal route path via tell — but tell swallows the response,
  // so we use ask() with a correlationId to get the response back.
  const askMsg = createMessage(
    address(targetPath),
    'test',
    {},
    { pattern: 'ask', from: address('sender'), correlationId: crypto.randomUUID() }
  );
  return router.ask(askMsg, 1000);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('MessageRouter — virtual actor factory', () => {
  let router: MessageRouter;

  beforeEach(() => {
    router = new MessageRouter();
  });

  it('factory is called on address miss, actor registered and message delivered', async () => {
    const factoryCalls: string[] = [];
    const handlerCalls: string[] = [];

    router.registerFactory({
      factory: async (addr) => {
        factoryCalls.push(addr);
        return makeHandler('created', handlerCalls);
      },
    });

    const response = await sendMsg(router, 'ai/stt/some-model');

    expect(factoryCalls).toEqual(['ai/stt/some-model']);
    expect(handlerCalls).toEqual(['created']);
    expect(response.success).toBe(true);
  });

  it('created actor is cached — factory is NOT called on the second message', async () => {
    const factoryCalls: string[] = [];
    const handlerCalls: string[] = [];

    router.registerFactory({
      factory: async (addr) => {
        factoryCalls.push(addr);
        return makeHandler('cached-actor', handlerCalls);
      },
    });

    await sendMsg(router, 'ai/tts/model-x');
    await sendMsg(router, 'ai/tts/model-x');

    // Factory called only once; handler called twice (both messages delivered)
    expect(factoryCalls.length).toBe(1);
    expect(handlerCalls.length).toBe(2);
  });

  it('hierarchical prefix fallback: factory at ai/stt matches ai/stt/ns/deepgram/nova-3', async () => {
    const factoryCalls: string[] = [];
    const handlerCalls: string[] = [];

    router.registerFactory({
      prefix: 'ai/stt',
      factory: async (addr) => {
        factoryCalls.push(addr);
        // Only respond when the candidate equals the prefix exactly
        if (addr === 'ai/stt') {
          return makeHandler('stt-factory', handlerCalls);
        }
        return null;
      },
    });

    const response = await sendMsg(router, 'ai/stt/ns/deepgram/nova-3');

    // Factory must have been called with candidates including 'ai/stt'
    expect(factoryCalls).toContain('ai/stt');
    // Actor should have been created and message delivered
    expect(handlerCalls).toEqual(['stt-factory']);
    expect(response.success).toBe(true);

    // Verify the actor is now cached at the FULL original address
    const cached = router.getActor('ai/stt/ns/deepgram/nova-3');
    expect(cached).toBeDefined();
  });

  it('hierarchical fallback tries addresses in order from full to root', async () => {
    const triedAddresses: string[] = [];

    // Factory returns null for everything — we just record what was tried
    router.registerFactory({
      factory: async (addr) => {
        triedAddresses.push(addr);
        return null;
      },
    });

    await sendMsg(router, 'a/b/c/d');

    expect(triedAddresses).toEqual(['a/b/c/d', 'a/b/c', 'a/b', 'a']);
  });

  it('dead letter when no factory matches any prefix', async () => {
    router.registerFactory({
      prefix: 'ai/tts',
      factory: async (_addr) => null,
    });

    const response = await sendMsg(router, 'ai/stt/some-address');

    // No factory matched — should end up in DLQ and return error response
    expect(response.success).toBe(false);

    const dlq = router.getDeadLetterQueue();
    expect(dlq.length).toBeGreaterThan(0);
    const entry = dlq.find(e => String(e.address).includes('ai/stt/some-address'));
    expect(entry).toBeDefined();
  });

  it('prefix-filtered factory is not tried when no candidate prefix matches', async () => {
    const factoryCalls: string[] = [];

    router.registerFactory({
      prefix: 'ai/vision',
      factory: async (addr) => {
        factoryCalls.push(addr);
        return makeHandler('vision-factory', []);
      },
    });

    // Address ai/stt/... — no candidate starts with 'ai/vision'
    await sendMsg(router, 'ai/stt/some-path');

    expect(factoryCalls).toHaveLength(0);
  });

  it('actor is cached at the FULL address when a parent-prefix factory matches', async () => {
    const handlerCalls: string[] = [];

    router.registerFactory({
      // factory responds only to 'ai/stt' (the parent prefix)
      factory: async (addr) => {
        if (addr === 'ai/stt') {
          return makeHandler('parent-matched', handlerCalls);
        }
        return null;
      },
    });

    const fullPath = 'ai/stt/bln_ai/deepgram/nova-3';
    await sendMsg(router, fullPath);

    // Registry should have the actor under the full address
    expect(router.getActor(fullPath)).toBeDefined();
    // Should NOT be registered under the short prefix that triggered it
    expect(router.getActor('ai/stt')).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// ActorSystem delegation
// ---------------------------------------------------------------------------

describe('ActorSystem.registerFactory — delegates to router', () => {
  it('registerFactory on ActorSystem provisions actors on demand', async () => {
    const system = new ActorSystem({ name: 'factory-test' });
    const handlerCalls: string[] = [];

    system.registerFactory({
      prefix: 'ai/inference',
      factory: async (addr) => {
        return makeHandler(`inference:${addr}`, handlerCalls);
      },
    });

    // Ask through the system to an unregistered actor
    const result = await system.ask(
      address('ai/inference/llm/gpt-4'),
      'generate',
      { prompt: 'hello' },
      1000
    );

    // The factory-created actor returned a response
    expect(handlerCalls).toHaveLength(1);
    expect(result).toBeDefined();
  });

  it('registerFactory delegation: factory called once, cached for second message', async () => {
    const system = new ActorSystem({ name: 'cache-test' });
    const factoryCalls: string[] = [];

    system.registerFactory({
      factory: async (addr) => {
        factoryCalls.push(addr);
        return makeHandler('dynamic', []);
      },
    });

    await system.ask(address('dynamic/actor/path'), 'ping', {}, 1000);
    await system.ask(address('dynamic/actor/path'), 'ping', {}, 1000);

    expect(factoryCalls.length).toBe(1);
  });
});
