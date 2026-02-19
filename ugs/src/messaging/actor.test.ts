#!/usr/bin/env bun
/**
 * Actor System Tests
 * Tests for src/messaging/actor.ts
 * Target: >90% coverage
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { Actor, ProgramActor, DocumentActor } from './actor.ts';
import { MessageRouter } from './router.ts';
import {
  type Message,
  type MessageResponse,
  address,
  createMessage,
  generateCorrelationId,
} from '@agentic-primer/actors';
import type GraphStore from '@src/graph.ts';
import type { ProgramManager } from '@src/entities/program.ts';

// Mock implementations
function createMockGraphStore(): GraphStore {
  const nodes = new Map();
  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
  } as any as GraphStore;
}

function createMockProgramManager(): ProgramManager {
  return {} as any as ProgramManager;
}

// Concrete Actor implementation for testing
class TestActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    return {
      id: 'resp_123',
      correlationId: message.correlationId || message.id,
      from: message.to,
      to: message.from!,
      success: true,
      payload: { received: message.type, originalPayload: message.payload },
      timestamp: Date.now(),
    };
  }
}

describe('Actor - Construction', () => {
  let router: MessageRouter;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
  });

  test('creates actor with id and router', () => {
    const actor = new TestActor('test-actor', router);
    expect(actor).toBeDefined();
    expect(actor.address).toBe('@(test-actor)');
  });

  test('actor address is @(id) format', () => {
    const actor = new TestActor('my-actor', router);
    expect(actor.address).toBe('@(my-actor)');
  });

  test('stores router reference', () => {
    const actor = new TestActor('test', router);
    expect((actor as any).router).toBe(router);
  });
});

describe('Actor - Base Class receive()', () => {
  let router: MessageRouter;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
  });

  test('base Actor.receive() returns error for unknown messages', async () => {
    const actor = new Actor('base-actor', router);
    const msg = createMessage(address('base-actor'), 'unknown-type', {});

    const response = await actor.receive(msg);
    expect(response.success).toBe(false);
    expect(response.error).toContain('Unknown message type');
  });

  test('subclass can override receive()', async () => {
    const actor = new TestActor('test-actor', router);
    const msg = createMessage(address('test-actor'), 'test', { data: 'hello' });

    const response = await actor.receive(msg);
    expect(response.success).toBe(true);
    expect(response.payload.originalPayload).toEqual({ data: 'hello' });
  });
});

describe('Actor - tell() Method', () => {
  let router: MessageRouter;
  let targetActor: TestActor;
  let senderActor: Actor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    targetActor = new TestActor('target', router);
    senderActor = new TestActor('sender', router);
    router.registerActor('target', targetActor);
  });

  test('tell() sends message without waiting', async () => {
    await senderActor.tell(address('target'), 'notify', { event: 'update' });
    // Should not throw, fire-and-forget
  });

  test('tell() includes from address', async () => {
    // We can't directly observe the message, but we test it doesn't throw
    await senderActor.tell(address('target'), 'test', {});
  });

  test('tell() handles non-existent target gracefully', async () => {
    await senderActor.tell(address('non-existent'), 'test', {});
    // Should not throw (router handles gracefully)
  });
});

describe('Actor - ask() Method', () => {
  let router: MessageRouter;
  let targetActor: TestActor;
  let senderActor: Actor;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
    targetActor = new TestActor('target', router);
    senderActor = new TestActor('sender', router);
    router.registerActor('target', targetActor);
  });

  test('ask() sends message and waits for response', async () => {
    const response = await senderActor.ask(address('target'), 'query', { question: 'foo' });
    expect(response.success).toBe(true);
    expect(response.payload.received).toBe('query');
    expect(response.payload.originalPayload).toEqual({ question: 'foo' });
  });

  test('ask() includes from address', async () => {
    const response = await senderActor.ask(address('target'), 'test', {});
    expect(response.to).toBe(senderActor.address);
  });

  test('ask() generates correlationId automatically', async () => {
    const response = await senderActor.ask(address('target'), 'test', {});
    expect(response.correlationId).toBeDefined();
    expect(response.correlationId).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i);
  });

  test('ask() returns response from target actor', async () => {
    const response = await senderActor.ask(address('target'), 'custom-type', { data: 42 });
    expect(response.from).toBe(address('target'));
    expect(response.payload.received).toBe('custom-type');
  });
});

describe('ProgramActor', () => {
  let router: MessageRouter;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
  });

  test('creates ProgramActor with programId', () => {
    const actor = new ProgramActor('my-program', router);
    expect(actor).toBeDefined();
    expect(actor.address).toBe('@(my-program)');
  });

  test('ProgramActor stores programId', () => {
    const actor = new ProgramActor('test-program', router);
    expect((actor as any).programId).toBe('test-program');
  });

  test('ProgramActor.receive() delegates to router', async () => {
    // Register a mock program in store
    const store = createMockGraphStore();
    const router = new MessageRouter(store, createMockProgramManager());

    const mockRespActor = new TestActor('test-program', router);
    router.registerActor('test-program', mockRespActor);

    const actor = new ProgramActor('test-program', router);
    const msg = createMessage(address('test-program'), 'execute', { code: 'return 42;' }, {
      from: address('caller'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response).toBeDefined();
  });
});

describe('DocumentActor', () => {
  let router: MessageRouter;
  let store: GraphStore;

  beforeEach(() => {
    store = createMockGraphStore();
    router = new MessageRouter(store, createMockProgramManager());
  });

  test('creates DocumentActor with documentId', () => {
    const actor = new DocumentActor('my-document', router);
    expect(actor).toBeDefined();
    expect(actor.address).toBe('@(my-document)');
  });

  test('DocumentActor stores documentId', () => {
    const actor = new DocumentActor('test-doc', router);
    expect((actor as any).documentId).toBe('test-doc');
  });

  test('DocumentActor.receive() handles read requests', async () => {
    store.set('test-doc', { type: 'information', content: 'Document content' });

    const mockRespActor = new TestActor('test-doc', router);
    router.registerActor('test-doc', mockRespActor);

    const actor = new DocumentActor('test-doc', router);
    const msg = createMessage(address('test-doc'), 'read', {}, {
      from: address('reader'),
      correlationId: generateCorrelationId(),
    });

    const response = await actor.receive(msg);
    expect(response).toBeDefined();
  });
});

describe('Actor - Message Handler Interface', () => {
  let router: MessageRouter;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
  });

  test('Actor implements MessageHandler interface', () => {
    const actor = new TestActor('test', router);
    expect(typeof actor.receive).toBe('function');
  });

  test('receive() returns Promise<MessageResponse>', async () => {
    const actor = new TestActor('test', router);
    const msg = createMessage(address('test'), 'test', {}, {
      from: address('sender'),
    });
    const response = await actor.receive(msg);

    expect(response).toBeDefined();
    expect(response.id).toBeDefined();
    expect(response.correlationId).toBeDefined();
    expect(response.from).toBeDefined();
    expect(response.to).toBeDefined();
    expect(response.success).toBeDefined();
    expect(response.timestamp).toBeDefined();
  });
});

describe('Actor - Addressing Consistency', () => {
  let router: MessageRouter;

  beforeEach(() => {
    router = new MessageRouter(createMockGraphStore(), createMockProgramManager());
  });

  test('actor address matches construction ID', () => {
    const id = 'consistent-actor';
    const actor = new TestActor(id, router);
    expect(actor.address).toBe(`@(${id})`);
  });

  test('tell() uses correct from/to addresses', async () => {
    const sender = new TestActor('sender', router);
    const target = new TestActor('target', router);
    router.registerActor('target', target);

    // tell() uses sender's address as from
    await sender.tell(address('target'), 'test', {});
    // Can't directly observe, but test it doesn't throw
  });

  test('ask() response swaps from/to correctly', async () => {
    const sender = new TestActor('sender', router);
    const target = new TestActor('target', router);
    router.registerActor('target', target);

    const response = await sender.ask(address('target'), 'test', {});
    expect(response.from).toBe(address('target'));
    expect(response.to).toBe(address('sender'));
  });
});
