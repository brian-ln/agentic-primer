#!/usr/bin/env bun
/**
 * Message Router Tests
 * Tests for src/messaging/router.ts
 * Target: >90% coverage
 */

import { test, expect, describe, beforeEach, mock } from 'bun:test';
import { MessageRouter } from '../router.ts';
import {
  type Message,
  type MessageResponse,
  type Address,
  address,
  createMessage,
  generateCorrelationId,
} from '@agentic-primer/actors';
import type GraphStore from '../../graph.ts';
import type { ProgramManager } from '../../entities/program.ts';

// Mock actor for testing
class MockActor {
  constructor(public id: string) {}

  async receive(message: Message): Promise<MessageResponse> {
    return {
      id: 'resp_' + message.id,
      correlationId: message.correlationId || message.id,
      from: message.to,
      to: message.from!,
      success: true,
      payload: { received: message.type, data: message.payload },
      timestamp: Date.now(),
    };
  }
}

// Mock actor that throws errors
class ErrorActor {
  async receive(message: Message): Promise<MessageResponse> {
    throw new Error('Actor processing failed');
  }
}

// Create mock GraphStore
function createMockGraphStore(): GraphStore {
  const nodes = new Map();

  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
    has: (id: string) => nodes.has(id),
    delete: (id: string) => nodes.delete(id),
    clear: () => nodes.clear(),
  } as any as GraphStore;
}

// Create mock ProgramManager
function createMockProgramManager(): ProgramManager {
  return {
    invokeProgram: mock(async (id: string, params: any) => {
      return {
        success: true,
        output: `Program ${id} executed with ${JSON.stringify(params)}`,
      };
    }),
  } as any as ProgramManager;
}

describe('MessageRouter - Construction', () => {
  test('creates router with store and program manager', () => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);
    expect(router).toBeDefined();
  });
});

describe('MessageRouter - Actor Registration', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('registerActor() adds actor to registry', () => {
    const actor = new MockActor('test-actor');
    router.registerActor('test-actor', actor);
    // Registry is private, but we can test by routing to it
  });

  test('unregisterActor() removes actor from registry', () => {
    const actor = new MockActor('test-actor');
    router.registerActor('test-actor', actor);
    router.unregisterActor('test-actor');
    // Registry is private, tested indirectly via routing
  });

  test('can re-register same actor ID', () => {
    const actor1 = new MockActor('test-actor');
    const actor2 = new MockActor('test-actor');
    router.registerActor('test-actor', actor1);
    router.registerActor('test-actor', actor2);
    // Should not throw, last registration wins
  });
});

describe('MessageRouter - tell() Pattern', () => {
  let router: MessageRouter;
  let actor: MockActor;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
    actor = new MockActor('test-actor');
    router.registerActor('test-actor', actor);
  });

  test('tell() sends message without waiting for response', async () => {
    const msg = createMessage(address('test-actor'), 'test', { foo: 'bar' });
    await router.tell(msg);
    // Should not throw, fire-and-forget
  });

  test('tell() does not throw on actor error', async () => {
    const errorActor = new ErrorActor();
    router.registerActor('error-actor', errorActor);

    const msg = createMessage(address('error-actor'), 'test', {});
    await router.tell(msg);
    // Should log error but not throw
  });

  test('tell() handles non-existent actor gracefully', async () => {
    const msg = createMessage(address('non-existent'), 'test', {});
    await router.tell(msg);
    // Should not throw
  });
});

describe('MessageRouter - ask() Pattern', () => {
  let router: MessageRouter;
  let actor: MockActor;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
    actor = new MockActor('test-actor');
    router.registerActor('test-actor', actor);
  });

  test('ask() requires correlationId', async () => {
    const msg = createMessage(address('test-actor'), 'test', {});
    await expect(router.ask(msg)).rejects.toThrow('ask() requires message with correlationId');
  });

  test('ask() returns response from actor', async () => {
    const msg = createMessage(address('test-actor'), 'test', { data: 'hello' }, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);
    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ received: 'test', data: { data: 'hello' } });
  });

  test('ask() correlates response with request', async () => {
    const correlationId = generateCorrelationId();
    const msg = createMessage(address('test-actor'), 'test', {}, {
      from: address('sender'),
      correlationId,
    });

    const response = await router.ask(msg);
    expect(response.correlationId).toBe(correlationId);
  });

  test('ask() times out after default 30s', async () => {
    // Create actor that never responds
    const slowActor = {
      async receive(msg: Message): Promise<MessageResponse> {
        await new Promise((resolve) => setTimeout(resolve, 35000));
        return { id: '', correlationId: '', from: msg.to, to: msg.from!, success: true, timestamp: Date.now() };
      },
    };
    router.registerActor('slow-actor', slowActor);

    const msg = createMessage(address('slow-actor'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    // Use short timeout for testing
    await expect(router.ask(msg, 100)).rejects.toThrow(/timed out after 100ms/);
  }, 1000);

  test('ask() respects custom timeout', async () => {
    const slowActor = {
      async receive(msg: Message): Promise<MessageResponse> {
        await new Promise((resolve) => setTimeout(resolve, 200));
        return { id: '', correlationId: '', from: msg.to, to: msg.from!, success: true, timestamp: Date.now() };
      },
    };
    router.registerActor('slow-actor', slowActor);

    const msg = createMessage(address('slow-actor'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    await expect(router.ask(msg, 50)).rejects.toThrow(/timed out after 50ms/);
  }, 500);

  test('ask() cleans up pending requests after response', async () => {
    const msg = createMessage(address('test-actor'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    await router.ask(msg);
    // Pending request should be cleaned up (can't directly test private map)
  });

  test('ask() cleans up pending requests after timeout', async () => {
    const slowActor = {
      async receive(msg: Message): Promise<MessageResponse> {
        await new Promise((resolve) => setTimeout(resolve, 200));
        return { id: '', correlationId: '', from: msg.to, to: msg.from!, success: true, timestamp: Date.now() };
      },
    };
    router.registerActor('slow-actor', slowActor);

    const msg = createMessage(address('slow-actor'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    try {
      await router.ask(msg, 50);
    } catch (e) {
      // Expected timeout
    }
    // Pending request should be cleaned up
  }, 500);

  test('ask() propagates actor errors', async () => {
    const errorActor = new ErrorActor();
    router.registerActor('error-actor', errorActor);

    const msg = createMessage(address('error-actor'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    await expect(router.ask(msg)).rejects.toThrow('Actor processing failed');
  });
});

describe('MessageRouter - Routing Logic', () => {
  let router: MessageRouter;
  let store: GraphStore;

  beforeEach(() => {
    store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('routes to registered actor first (priority over graph)', async () => {
    const actor = new MockActor('priority-node');
    router.registerActor('priority-node', actor);

    // Also add node to graph
    store.set('priority-node', { type: 'program', data: 'test' });

    const msg = createMessage(address('priority-node'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);
    expect(response.payload).toEqual({ received: 'test', data: {} });
  });

  test('handles system messages without sender', async () => {
    const actor = new MockActor('system-actor');
    router.registerActor('system-actor', actor);

    const msg = createMessage(address('system-actor'), 'system-message', {});
    // System message has no from address
    msg.from = undefined;

    // Should handle gracefully (tell pattern)
    await router.tell(msg);
  });
});

describe('MessageRouter - Error Handling', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('handles non-existent node in ask()', async () => {
    const msg = createMessage(address('non-existent'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);
    expect(response.success).toBe(false);
    expect(response.error).toContain('not found');
  });

  test('handles actor receive() throwing error', async () => {
    const errorActor = new ErrorActor();
    router.registerActor('error-actor', errorActor);

    const msg = createMessage(address('error-actor'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    await expect(router.ask(msg)).rejects.toThrow('Actor processing failed');
  });
});

describe('MessageRouter - Routing to Graph Nodes', () => {
  let router: MessageRouter;
  let store: GraphStore;
  let programManager: ProgramManager;

  beforeEach(() => {
    store = createMockGraphStore();
    programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('routes to program node in graph', async () => {
    store.set('test-program', { type: 'program', code: 'return 42;' });

    const msg = createMessage(address('test-program'), 'execute', { params: {} }, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);
    // Router should return a response (success or failure depending on implementation)
    expect(response).toBeDefined();
    expect(response.correlationId).toBe(msg.correlationId);
    expect(response.from).toBe(msg.to);
    expect(response.to).toBe(msg.from);
  });

  test('routes to document/information node', async () => {
    store.set('test-doc', { type: 'information', content: 'Document data' });

    const msg = createMessage(address('test-doc'), 'read', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);
    // Response structure depends on router implementation for documents
    expect(response).toBeDefined();
    expect(response.correlationId).toBe(msg.correlationId);
  });

  test('routes to session node', async () => {
    store.set('test-session', { type: 'session', messages: [] });

    const msg = createMessage(address('test-session'), 'query', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);
    // Response structure depends on router implementation for sessions
    expect(response).toBeDefined();
    expect(response.correlationId).toBe(msg.correlationId);
  });

  test('handles node not found with sender', async () => {
    const msg = createMessage(address('non-existent'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);
    expect(response.success).toBe(false);
    expect(response.error).toMatch(/not found|Node not found/i);
  });

  test('handles node not found without sender (system message)', async () => {
    const msg = createMessage(address('non-existent'), 'test', {}, {
      correlationId: generateCorrelationId(),
    });
    msg.from = undefined;

    const response = await router.ask(msg, 100);
    expect(response.success).toBe(false);
  });
});

describe('MessageRouter - Stream Support', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('streamAsk() throws if actor missing', async () => {
    await expect(
      router.streamAsk(address('non-existent'), 'test', {}, {
        onChunk: () => {},
      })
    ).rejects.toThrow(/Node not found/);
  });

  test('streamAsk() throws if actor has no stream method', async () => {
    const actorWithoutStream = new MockActor('no-stream');
    router.registerActor('no-stream', actorWithoutStream);

    await expect(
      router.streamAsk(address('no-stream'), 'test', {}, {
        onChunk: () => {},
      })
    ).rejects.toThrow(/does not support streaming/);
  });

  test('streamAsk() calls actor stream method', async () => {
    const chunks: any[] = [];
    const streamActor = {
      async stream(payload: any, onChunk: (chunk: any) => void) {
        await onChunk({ type: 'data', content: 'chunk1' });
        await onChunk({ type: 'data', content: 'chunk2' });
        await onChunk({ type: 'done' });
      },
    };
    router.registerActor('stream-actor', streamActor);

    await router.streamAsk(address('stream-actor'), 'test', {}, {
      onChunk: (chunk) => chunks.push(chunk),
    });

    expect(chunks).toHaveLength(3);
    expect(chunks[0].content).toBe('chunk1');
    expect(chunks[1].content).toBe('chunk2');
    expect(chunks[2].type).toBe('done');
  });

  test('streamAsk() throws if node exists but no stream support', async () => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    const router = new MessageRouter(store, programManager);

    store.set('test-node', { type: 'information', data: 'test' });

    await expect(
      router.streamAsk(address('test-node'), 'test', {}, {
        onChunk: () => {},
      })
    ).rejects.toThrow(/does not support streaming/);
  });
});

describe('MessageRouter - Concurrent Requests', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  test('handles multiple concurrent ask() requests', async () => {
    const actor = new MockActor('concurrent-actor');
    router.registerActor('concurrent-actor', actor);

    const requests = Array.from({ length: 10 }, (_, i) =>
      createMessage(address('concurrent-actor'), `test-${i}`, { index: i }, {
        from: address('sender'),
        correlationId: generateCorrelationId(),
      })
    );

    const responses = await Promise.all(requests.map((msg) => router.ask(msg)));

    expect(responses).toHaveLength(10);
    responses.forEach((resp, i) => {
      expect(resp.success).toBe(true);
      expect(resp.payload.data.index).toBe(i);
    });
  });

  test('independent timeouts for concurrent requests', async () => {
    const fastActor = {
      async receive(msg: Message): Promise<MessageResponse> {
        await new Promise((resolve) => setTimeout(resolve, 10));
        return { id: '', correlationId: msg.correlationId!, from: msg.to, to: msg.from!, success: true, payload: { fast: true }, timestamp: Date.now() };
      },
    };
    const slowActor = {
      async receive(msg: Message): Promise<MessageResponse> {
        await new Promise((resolve) => setTimeout(resolve, 200));
        return { id: '', correlationId: msg.correlationId!, from: msg.to, to: msg.from!, success: true, payload: { slow: true }, timestamp: Date.now() };
      },
    };

    router.registerActor('fast', fastActor);
    router.registerActor('slow', slowActor);

    const fastMsg = createMessage(address('fast'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });
    const slowMsg = createMessage(address('slow'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const [fastResult, slowResult] = await Promise.allSettled([
      router.ask(fastMsg, 100),
      router.ask(slowMsg, 50),
    ]);

    expect(fastResult.status).toBe('fulfilled');
    expect(slowResult.status).toBe('rejected');
  }, 500);
});

// ---------------------------------------------------------------------------
// BridgeRoute Tests
// ---------------------------------------------------------------------------

describe('MessageRouter - BridgeRoute', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);
  });

  function createMockTransport() {
    const sent: Array<{ recipient: string; data: unknown }> = [];
    let receiveHandler: ((sender: string, message: unknown) => void) | null = null;

    return {
      transport: {
        state: 'connected' as const,
        async connect() {},
        async disconnect() {},
        async send(recipient: string, message: unknown) {
          sent.push({ recipient, data: message });
        },
        onReceive(handler: (sender: string, message: unknown) => void) {
          receiveHandler = handler;
        },
      },
      sent,
      simulateInbound(sender: string, message: unknown) {
        if (receiveHandler) receiveHandler(sender, message);
      },
    };
  }

  function createMockSerde() {
    return {
      serialize(value: unknown): Uint8Array {
        return new TextEncoder().encode(JSON.stringify(value));
      },
      deserialize(data: Uint8Array): unknown {
        return JSON.parse(new TextDecoder().decode(data));
      },
      get contentType() {
        return 'application/json';
      },
    };
  }

  test('registerBridge adds a bridge prefix', () => {
    const { transport } = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport, serde });

    expect(router.listBridges()).toEqual(['remote/']);
  });

  test('unregisterBridge removes a bridge prefix', () => {
    const { transport } = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport, serde });
    router.unregisterBridge('remote/');

    expect(router.listBridges()).toEqual([]);
  });

  test('tell() dispatches to bridge when address matches prefix', async () => {
    const { transport, sent } = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport, serde });

    const msg = createMessage(address('remote/signal-hub'), 'ping', { hello: 'world' }, {
      from: address('local/sender'),
    });

    await router.tell(msg);

    // Verify the message was dispatched through the transport
    expect(sent).toHaveLength(1);
    expect(sent[0].recipient).toBe('remote/signal-hub');

    // Verify the serialized payload is a Uint8Array (serialized SharedMessage)
    expect(sent[0].data).toBeInstanceOf(Uint8Array);

    // Deserialize and verify SharedMessage structure
    const decoded = JSON.parse(new TextDecoder().decode(sent[0].data as Uint8Array));
    expect(decoded.to).toBe('@(remote/signal-hub)');
    expect(decoded.from).toBe('@(local/sender)');
    expect(decoded.type).toBe('ping');
    expect(decoded.payload).toEqual({ hello: 'world' });
  });

  test('ask() dispatches to bridge and returns bridged response', async () => {
    const { transport, sent } = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport, serde });

    const msg = createMessage(address('remote/signal-hub'), 'ping', {}, {
      from: address('local/sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);

    expect(response.success).toBe(true);
    expect(response.payload).toEqual({ bridged: true, prefix: 'remote/' });
    expect(sent).toHaveLength(1);
  });

  test('bridge route takes priority over hierarchical routing', async () => {
    const { transport, sent } = createMockTransport();
    const serde = createMockSerde();

    // Register a bridge with prefix 'external/'
    router.registerBridge({ prefix: 'external/', transport, serde });

    // This should go through the bridge, not hierarchical routing
    const msg = createMessage(address('external/some-actor'), 'test', {}, {
      from: address('local/sender'),
    });

    await router.tell(msg);

    expect(sent).toHaveLength(1);
  });

  test('registered local actor takes priority over bridge', async () => {
    const { transport, sent } = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport, serde });

    // Register a local actor with the exact bridge-prefixed path
    const actor = new MockActor('remote/local-override');
    router.registerActor('remote/local-override', actor);

    const msg = createMessage(address('remote/local-override'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);

    // Should be handled by the local actor, NOT the bridge
    expect(response.success).toBe(true);
    expect(response.payload.received).toBe('test');
    expect(sent).toHaveLength(0); // Nothing sent through transport
  });

  test('non-matching address is not bridged', async () => {
    const { transport, sent } = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport, serde });

    // Register a local actor for a non-bridge address
    const actor = new MockActor('local/actor');
    router.registerActor('local/actor', actor);

    const msg = createMessage(address('local/actor'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);

    expect(response.success).toBe(true);
    expect(sent).toHaveLength(0); // Nothing sent to bridge
  });

  test('bridge handles transport send failure gracefully', async () => {
    const failTransport = {
      state: 'connected' as const,
      async connect() {},
      async disconnect() {},
      async send() {
        throw new Error('Transport connection lost');
      },
      onReceive() {},
    };
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport: failTransport, serde });

    const msg = createMessage(address('remote/target'), 'test', {}, {
      from: address('sender'),
      correlationId: generateCorrelationId(),
    });

    const response = await router.ask(msg);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Bridge dispatch failed');
    expect(response.error).toContain('Transport connection lost');
  });

  test('multiple bridges with different prefixes', async () => {
    const ws = createMockTransport();
    const http = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'ws/', transport: ws.transport, serde });
    router.registerBridge({ prefix: 'http/', transport: http.transport, serde });

    expect(router.listBridges()).toEqual(['ws/', 'http/']);

    // Send to ws bridge
    await router.tell(createMessage(address('ws/actor'), 'test', {}, {
      from: address('sender'),
    }));

    // Send to http bridge
    await router.tell(createMessage(address('http/actor'), 'test', {}, {
      from: address('sender'),
    }));

    expect(ws.sent).toHaveLength(1);
    expect(http.sent).toHaveLength(1);
  });

  test('inbound message from bridge transport is routed locally', async () => {
    const { transport, simulateInbound } = createMockTransport();
    const serde = createMockSerde();

    // Register a local actor that will receive the bridged message
    let receivedMessage: Message | null = null;
    const localActor = {
      async receive(msg: Message): Promise<MessageResponse> {
        receivedMessage = msg;
        return {
          id: 'resp',
          correlationId: msg.correlationId || msg.id,
          from: msg.to,
          to: msg.from!,
          success: true,
          payload: { handled: true },
          timestamp: Date.now(),
        };
      },
    };
    router.registerActor('local/handler', localActor);

    // Register the bridge
    router.registerBridge({ prefix: 'remote/', transport, serde });

    // Simulate an inbound SharedMessage arriving from the remote side
    const sharedMsg = {
      id: crypto.randomUUID(),
      from: '@(remote/sender)',
      to: '@(local/handler)',
      type: 'notification',
      payload: { data: 'hello from remote' },
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      metadata: {},
      ttl: null,
      signature: null,
    };

    simulateInbound('remote/sender', sharedMsg);

    // Give the async routing a tick to complete
    await new Promise((resolve) => setTimeout(resolve, 20));

    expect(receivedMessage).not.toBeNull();
    expect(receivedMessage!.type).toBe('notification');
    expect(receivedMessage!.payload).toEqual({ data: 'hello from remote' });
  });

  test('getRoutingStats includes bridges', () => {
    const { transport } = createMockTransport();
    const serde = createMockSerde();

    router.registerBridge({ prefix: 'remote/', transport, serde });
    router.registerBridge({ prefix: 'ws/', transport, serde });

    const stats = router.getRoutingStats();
    expect(stats.bridges).toEqual(['remote/', 'ws/']);
  });
});
