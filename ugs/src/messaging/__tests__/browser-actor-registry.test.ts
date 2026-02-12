/**
 * Tests for Browser Actor Registry
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import {
  BrowserActorRegistry,
  ActorNotFoundError,
  DuplicateActorError,
} from '../browser/actor-registry.ts';
import type { Actor } from '../actor.ts';
import type { Address, Message, MessageResponse } from '@agentic-primer/actors';
import { address } from '@agentic-primer/actors';

// Mock actor for testing
class MockActor implements Actor {
  constructor(
    public readonly address: Address,
    private responseGenerator?: (msg: Message) => MessageResponse
  ) {}

  async receive(msg: Message): Promise<MessageResponse> {
    if (this.responseGenerator) {
      return this.responseGenerator(msg);
    }
    return { success: true, data: `Received: ${msg.type}` };
  }
}

describe('BrowserActorRegistry', () => {
  let registry: BrowserActorRegistry;

  beforeEach(() => {
    registry = new BrowserActorRegistry();
  });

  describe('register()', () => {
    test('registers an actor successfully', () => {
      const actor = new MockActor(address('test-actor'));
      registry.register(address('test-actor'), actor);

      expect(registry.has(address('test-actor'))).toBe(true);
      expect(registry.size).toBe(1);
    });

    test('throws on duplicate registration', () => {
      const actor1 = new MockActor(address('test-actor'));
      const actor2 = new MockActor(address('test-actor'));

      registry.register(address('test-actor'), actor1);

      expect(() => {
        registry.register(address('test-actor'), actor2);
      }).toThrow(DuplicateActorError);
    });

    test('allows multiple actors with different addresses', () => {
      const actor1 = new MockActor(address('actor-1'));
      const actor2 = new MockActor(address('actor-2'));
      const actor3 = new MockActor(address('actor-3'));

      registry.register(address('actor-1'), actor1);
      registry.register(address('actor-2'), actor2);
      registry.register(address('actor-3'), actor3);

      expect(registry.size).toBe(3);
      expect(registry.has(address('actor-1'))).toBe(true);
      expect(registry.has(address('actor-2'))).toBe(true);
      expect(registry.has(address('actor-3'))).toBe(true);
    });
  });

  describe('registerOrReplace()', () => {
    test('registers new actor', () => {
      const actor = new MockActor(address('test-actor'));
      const previous = registry.registerOrReplace(address('test-actor'), actor);

      expect(previous).toBeUndefined();
      expect(registry.has(address('test-actor'))).toBe(true);
    });

    test('replaces existing actor', () => {
      const actor1 = new MockActor(address('test-actor'));
      const actor2 = new MockActor(address('test-actor'));

      registry.register(address('test-actor'), actor1);
      const previous = registry.registerOrReplace(address('test-actor'), actor2);

      expect(previous).toBe(actor1);
      expect(registry.lookup(address('test-actor'))).toBe(actor2);
    });
  });

  describe('unregister()', () => {
    test('removes registered actor', () => {
      const actor = new MockActor(address('test-actor'));
      registry.register(address('test-actor'), actor);

      const removed = registry.unregister(address('test-actor'));

      expect(removed).toBe(true);
      expect(registry.has(address('test-actor'))).toBe(false);
      expect(registry.size).toBe(0);
    });

    test('returns false for non-existent actor', () => {
      const removed = registry.unregister(address('non-existent'));

      expect(removed).toBe(false);
    });

    test('is idempotent (safe to call multiple times)', () => {
      const actor = new MockActor(address('test-actor'));
      registry.register(address('test-actor'), actor);

      registry.unregister(address('test-actor'));
      const secondRemove = registry.unregister(address('test-actor'));

      expect(secondRemove).toBe(false);
    });
  });

  describe('lookup()', () => {
    test('returns registered actor', () => {
      const actor = new MockActor(address('test-actor'));
      registry.register(address('test-actor'), actor);

      const found = registry.lookup(address('test-actor'));

      expect(found).toBe(actor);
    });

    test('returns undefined for non-existent actor', () => {
      const found = registry.lookup(address('non-existent'));

      expect(found).toBeUndefined();
    });
  });

  describe('has()', () => {
    test('returns true for registered actor', () => {
      const actor = new MockActor(address('test-actor'));
      registry.register(address('test-actor'), actor);

      expect(registry.has(address('test-actor'))).toBe(true);
    });

    test('returns false for non-existent actor', () => {
      expect(registry.has(address('non-existent'))).toBe(false);
    });

    test('returns false after unregister', () => {
      const actor = new MockActor(address('test-actor'));
      registry.register(address('test-actor'), actor);
      registry.unregister(address('test-actor'));

      expect(registry.has(address('test-actor'))).toBe(false);
    });
  });

  describe('send()', () => {
    test('sends message to registered actor', async () => {
      const actor = new MockActor(address('test-actor'), (msg) => ({
        success: true,
        data: `Echo: ${msg.type}`,
      }));
      registry.register(address('test-actor'), actor);

      const response = await registry.send(address('test-actor'), {
        id: 'msg-1',
        from: address('sender'),
        to: address('test-actor'),
        type: 'test-message',
        payload: { value: 42 },
        timestamp: Date.now(),
      });

      expect(response.success).toBe(true);
      expect(response.data).toBe('Echo: test-message');
    });

    test('throws ActorNotFoundError for non-existent actor', async () => {
      await expect(
        registry.send(address('non-existent'), {
          id: 'msg-1',
          from: address('sender'),
          to: address('non-existent'),
          type: 'test',
          payload: {},
          timestamp: Date.now(),
        })
      ).rejects.toThrow(ActorNotFoundError);
    });

    test('propagates actor errors', async () => {
      const actor = new MockActor(address('test-actor'), () => {
        throw new Error('Actor processing failed');
      });
      registry.register(address('test-actor'), actor);

      await expect(
        registry.send(address('test-actor'), {
          id: 'msg-1',
          from: address('sender'),
          to: address('test-actor'),
          type: 'test',
          payload: {},
          timestamp: Date.now(),
        })
      ).rejects.toThrow('Actor processing failed');
    });
  });

  describe('list()', () => {
    test('returns empty array when no actors', () => {
      const addresses = registry.list();

      expect(addresses).toEqual([]);
    });

    test('returns all registered addresses', () => {
      registry.register(address('actor-1'), new MockActor(address('actor-1')));
      registry.register(address('actor-2'), new MockActor(address('actor-2')));
      registry.register(address('actor-3'), new MockActor(address('actor-3')));

      const addresses = registry.list();

      expect(addresses.length).toBe(3);
      expect(addresses).toContain(address('actor-1'));
      expect(addresses).toContain(address('actor-2'));
      expect(addresses).toContain(address('actor-3'));
    });
  });

  describe('size', () => {
    test('returns 0 for empty registry', () => {
      expect(registry.size).toBe(0);
    });

    test('tracks actor count', () => {
      registry.register(address('actor-1'), new MockActor(address('actor-1')));
      expect(registry.size).toBe(1);

      registry.register(address('actor-2'), new MockActor(address('actor-2')));
      expect(registry.size).toBe(2);

      registry.unregister(address('actor-1'));
      expect(registry.size).toBe(1);
    });
  });

  describe('clear()', () => {
    test('removes all actors', () => {
      registry.register(address('actor-1'), new MockActor(address('actor-1')));
      registry.register(address('actor-2'), new MockActor(address('actor-2')));
      registry.register(address('actor-3'), new MockActor(address('actor-3')));

      registry.clear();

      expect(registry.size).toBe(0);
      expect(registry.list()).toEqual([]);
    });

    test('is safe to call on empty registry', () => {
      registry.clear();

      expect(registry.size).toBe(0);
    });
  });

  describe('snapshot()', () => {
    test('returns copy of registry state', () => {
      const actor1 = new MockActor(address('actor-1'));
      const actor2 = new MockActor(address('actor-2'));

      registry.register(address('actor-1'), actor1);
      registry.register(address('actor-2'), actor2);

      const snapshot = registry.snapshot();

      expect(snapshot.size).toBe(2);
      expect(snapshot.get(address('actor-1'))).toBe(actor1);
      expect(snapshot.get(address('actor-2'))).toBe(actor2);
    });

    test('snapshot is independent of registry changes', () => {
      const actor = new MockActor(address('test-actor'));
      registry.register(address('test-actor'), actor);

      const snapshot = registry.snapshot();

      registry.unregister(address('test-actor'));

      // Snapshot should still have the actor
      expect(snapshot.size).toBe(1);
      expect(registry.size).toBe(0);
    });
  });

  describe('Widget Actor lifecycle simulation', () => {
    test('mimics connectedCallback/disconnectedCallback pattern', () => {
      // Simulate web component lifecycle
      class SimulatedWidget {
        private widgetAddress = address(`widget-${Math.random()}`);

        connectedCallback() {
          const actor = new MockActor(this.widgetAddress);
          registry.register(this.widgetAddress, actor);
        }

        disconnectedCallback() {
          registry.unregister(this.widgetAddress);
        }

        getAddress() {
          return this.widgetAddress;
        }
      }

      const widget = new SimulatedWidget();

      // Component mounts
      widget.connectedCallback();
      expect(registry.has(widget.getAddress())).toBe(true);

      // Component unmounts
      widget.disconnectedCallback();
      expect(registry.has(widget.getAddress())).toBe(false);
    });

    test('handles multiple widget instances', () => {
      const widgets = Array.from({ length: 5 }, (_, i) => ({
        addr: address(`widget-${i}`),
        actor: new MockActor(address(`widget-${i}`)),
      }));

      // Mount all widgets
      widgets.forEach(({ addr, actor }) => {
        registry.register(addr, actor);
      });

      expect(registry.size).toBe(5);

      // Unmount widgets 0, 2, 4
      [0, 2, 4].forEach((i) => {
        registry.unregister(widgets[i].addr);
      });

      expect(registry.size).toBe(2);
      expect(registry.has(widgets[1].addr)).toBe(true);
      expect(registry.has(widgets[3].addr)).toBe(true);
    });
  });

  describe('memory safety', () => {
    test('no leaks after clear()', () => {
      const actors = Array.from({ length: 100 }, (_, i) => {
        const actor = new MockActor(address(`actor-${i}`));
        registry.register(address(`actor-${i}`), actor);
        return actor;
      });

      expect(registry.size).toBe(100);

      registry.clear();

      expect(registry.size).toBe(0);
      // Actors should be GC-able after clear (weak reference test would go here)
    });
  });
});
