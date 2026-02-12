/**
 * Tests for Actor Reactive Ports pattern
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import { Actor } from '../actor.ts';
import { MessageRouter } from '../router.ts';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import type { Message, MessageResponse, Address } from '@agentic-primer/actors';
import { address } from '@agentic-primer/actors';
import type { Channel } from '../channel.ts';

// Test actor with reactive ports
class ReactiveActor extends Actor {
  private statusChanges = this.createPort<{ status: string; timestamp: number}>('status');
  private errorEvents = this.createPort<{ error: string; timestamp: number }>('errors');

  async receive(msg: Message): Promise<MessageResponse> {
    switch (msg.type) {
      case 'update-status':
        await this.statusChanges.send({
          status: msg.payload.status,
          timestamp: Date.now(),
        });
        return { success: true };

      case 'trigger-error':
        await this.errorEvents.send({
          error: msg.payload.error,
          timestamp: Date.now(),
        });
        return { success: true };

      default:
        return { success: false, error: 'Unknown message type' };
    }
  }

  port<T>(name: string): Channel<T> {
    if (name === 'status') return this.statusChanges as Channel<T>;
    if (name === 'errors') return this.errorEvents as Channel<T>;
    throw new Error(`Unknown port: ${name}`);
  }

  // Expose for testing
  async broadcastStatus(status: string) {
    await this.getPort('status').send({ status, timestamp: Date.now() });
  }

  async broadcastError(error: string) {
    await this.getPort('errors').send({ error, timestamp: Date.now() });
  }

  shutdown() {
    this.closePorts();
  }
}

describe('Actor Reactive Ports', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let actor: ReactiveActor;

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);
    actor = new ReactiveActor('test-actor', router);
  });

  describe('port() method', () => {
    test('returns port channel', () => {
      const statusPort = actor.port('status');

      expect(statusPort).toBeDefined();
      expect(typeof statusPort[Symbol.asyncIterator]).toBe('function');
    });

    test('throws for unknown port', () => {
      expect(() => {
        actor.port('unknown');
      }).toThrow('Unknown port: unknown');
    });

    test('returns same port instance on repeated calls', () => {
      const port1 = actor.port('status');
      const port2 = actor.port('status');

      expect(port1).toBe(port2);
    });
  });

  describe('broadcasting to subscribers', () => {
    test('broadcasts to single subscriber', async () => {
      const statusPort = actor.port<{ status: string; timestamp: number }>('status');
      const events: any[] = [];

      const consumer = (async () => {
        for await (const event of statusPort.subscribe()) {
          events.push(event);
          if (events.length >= 3) break;
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      await actor.broadcastStatus('ready');
      await actor.broadcastStatus('processing');
      await actor.broadcastStatus('complete');

      await consumer;

      expect(events.length).toBe(3);
      expect(events[0].status).toBe('ready');
      expect(events[1].status).toBe('processing');
      expect(events[2].status).toBe('complete');
    });

    test('broadcasts to multiple subscribers', async () => {
      const statusPort = actor.port<{ status: string; timestamp: number }>('status');

      const results1: any[] = [];
      const results2: any[] = [];
      const results3: any[] = [];

      const consumer1 = (async () => {
        for await (const event of statusPort.subscribe()) {
          results1.push(event);
          if (results1.length >= 2) break;
        }
      })();

      const consumer2 = (async () => {
        for await (const event of statusPort.subscribe()) {
          results2.push(event);
          if (results2.length >= 2) break;
        }
      })();

      const consumer3 = (async () => {
        for await (const event of statusPort.subscribe()) {
          results3.push(event);
          if (results3.length >= 2) break;
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      await actor.broadcastStatus('start');
      await actor.broadcastStatus('end');

      await Promise.all([consumer1, consumer2, consumer3]);

      expect(results1.length).toBe(2);
      expect(results2.length).toBe(2);
      expect(results3.length).toBe(2);

      // All subscribers got same events
      expect(results1[0].status).toBe('start');
      expect(results2[0].status).toBe('start');
      expect(results3[0].status).toBe('start');
    });

    test('handles slow consumer with backpressure', async () => {
      const statusPort = actor.port<{ status: string; timestamp: number }>('status');

      let backpressureTriggered = false;
      const events: any[] = [];

      const slowConsumer = (async () => {
        for await (const event of statusPort.subscribe({
          bufferSize: 3,
          onBackpressure: (isPaused) => {
            if (isPaused) backpressureTriggered = true;
          },
        })) {
          events.push(event);
          await new Promise((resolve) => setTimeout(resolve, 20)); // Slow
          if (events.length >= 5) break;
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      // Send rapidly (should trigger backpressure)
      for (let i = 0; i < 5; i++) {
        await actor.broadcastStatus(`status-${i}`);
      }

      await slowConsumer;

      expect(events.length).toBe(5);
      expect(backpressureTriggered).toBe(true);
    });
  });

  describe('multiple ports', () => {
    test('different ports are independent', async () => {
      const statusPort = actor.port<{ status: string; timestamp: number }>('status');
      const errorPort = actor.port<{ error: string; timestamp: number }>('errors');

      const statusEvents: any[] = [];
      const errorEvents: any[] = [];

      const statusConsumer = (async () => {
        for await (const event of statusPort.subscribe()) {
          statusEvents.push(event);
          if (statusEvents.length >= 2) break;
        }
      })();

      const errorConsumer = (async () => {
        for await (const event of errorPort.subscribe()) {
          errorEvents.push(event);
          if (errorEvents.length >= 1) break;
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      await actor.broadcastStatus('working');
      await actor.broadcastError('timeout');
      await actor.broadcastStatus('recovered');

      await Promise.all([statusConsumer, errorConsumer]);

      expect(statusEvents.length).toBe(2);
      expect(errorEvents.length).toBe(1);
      expect(statusEvents[0].status).toBe('working');
      expect(errorEvents[0].error).toBe('timeout');
    });
  });

  describe('via message protocol', () => {
    test('message triggers port broadcast', async () => {
      router.registerActor('test-actor', actor);

      const statusPort = actor.port<{ status: string; timestamp: number }>('status');
      const events: any[] = [];

      const consumer = (async () => {
        for await (const event of statusPort.subscribe()) {
          events.push(event);
          if (events.length >= 1) break;
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      // Send message to actor
      await router.tell({
        id: 'msg-1',
        from: address('sender'),
        to: address('test-actor'),
        type: 'update-status',
        payload: { status: 'online' },
        timestamp: Date.now(),
      });

      await consumer;

      expect(events.length).toBe(1);
      expect(events[0].status).toBe('online');
    });
  });

  describe('lifecycle', () => {
    test('closePorts() stops all ports', async () => {
      const statusPort = actor.port<{ status: string; timestamp: number }>('status');

      const events: any[] = [];
      const consumer = (async () => {
        for await (const event of statusPort.subscribe()) {
          events.push(event);
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      await actor.broadcastStatus('start');

      actor.shutdown(); // Calls closePorts()

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      // Should only have received events before close
      expect(events.length).toBe(1);
    });

    test('unsubscribe stops receiving events', async () => {
      const statusPort = actor.port<{ status: string; timestamp: number }>('status');

      const events: any[] = [];
      const subscription = statusPort.subscribe();

      const consumer = (async () => {
        for await (const event of subscription) {
          events.push(event);
          if (events.length >= 2) {
            subscription.close(); // Unsubscribe
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      await actor.broadcastStatus('event-1');
      await actor.broadcastStatus('event-2');
      await actor.broadcastStatus('event-3'); // Should not be received

      await consumer;

      expect(events.length).toBe(2);
    });
  });

  describe('error handling', () => {
    test('getPort() throws for non-existent port', () => {
      expect(() => {
        // @ts-expect-error - testing private method
        actor.getPort('nonexistent');
      }).toThrow('Port not found: nonexistent');
    });

    test('createPort() creates port on first call', () => {
      // @ts-expect-error - testing private method
      const port1 = actor.createPort('new-port');
      // @ts-expect-error - testing private method
      const port2 = actor.createPort('new-port');

      expect(port1).toBe(port2); // Same instance
    });
  });

  describe('subscriber count', () => {
    test('tracks active subscribers', async () => {
      const statusPort = actor.port<{ status: string; timestamp: number }>('status');

      const sub1 = statusPort.subscribe();
      const sub2 = statusPort.subscribe();
      const sub3 = statusPort.subscribe();

      expect(statusPort.subscriberCount).toBe(3);

      sub1.close();
      expect(statusPort.subscriberCount).toBe(2);

      sub2.close();
      sub3.close();
      expect(statusPort.subscriberCount).toBe(0);
    });
  });
});
