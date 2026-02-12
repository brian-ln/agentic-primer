#!/usr/bin/env bun
import { describe, test, expect, beforeEach } from 'bun:test';
import { SchedulerActor, VirtualClock, RealClock } from '../scheduler.ts';
import { MessageRouter } from '../../messaging/router.ts';
import { Actor } from '../../messaging/actor.ts';
import GraphStore from '../../graph.ts';
import { ProgramManager } from '../../entities/program.ts';
import { address, createMessage, createResponse } from '@agentic-primer/actors';
import type { Message, MessageResponse } from '@agentic-primer/actors';

describe('SchedulerActor', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);

    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });

    // Register scheduler
    router.registerActor('/system/scheduler', scheduler);
  });

  describe('VirtualClock', () => {
    test('should start at time 0', () => {
      const clock = new VirtualClock();
      expect(clock.now()).toBe(0);
    });

    test('should advance time', async () => {
      const clock = new VirtualClock();
      await clock.advance(1000);
      expect(clock.now()).toBe(1000);
    });

    test('should execute setTimeout callbacks when advancing', async () => {
      const clock = new VirtualClock();
      let executed = false;

      clock.setTimeout(() => {
        executed = true;
      }, 500);

      await clock.advance(500);
      expect(executed).toBe(true);
    });

    test('should execute multiple timers in order', async () => {
      const clock = new VirtualClock();
      const results: number[] = [];

      clock.setTimeout(() => results.push(1), 100);
      clock.setTimeout(() => results.push(2), 200);
      clock.setTimeout(() => results.push(3), 50);

      await clock.advance(300);

      expect(results).toEqual([3, 1, 2]);
    });

    test('should execute setInterval callbacks', async () => {
      const clock = new VirtualClock();
      const results: number[] = [];

      clock.setInterval(() => results.push(clock.now()), 100);

      await clock.advance(350);

      expect(results).toEqual([100, 200, 300]);
    });

    test('should cancel setTimeout', async () => {
      const clock = new VirtualClock();
      let executed = false;

      const id = clock.setTimeout(() => {
        executed = true;
      }, 100);

      clock.clearTimeout(id);
      await clock.advance(200);

      expect(executed).toBe(false);
    });

    test('should cancel setInterval', async () => {
      const clock = new VirtualClock();
      const results: number[] = [];

      const id = clock.setInterval(() => results.push(clock.now()), 100);

      await clock.advance(250);
      clock.clearInterval(id);
      await clock.advance(200);

      expect(results).toEqual([100, 200]);
    });

    test('runNext should execute earliest timer', async () => {
      const clock = new VirtualClock();
      const results: number[] = [];

      clock.setTimeout(() => results.push(1), 100);
      clock.setTimeout(() => results.push(2), 50);

      await clock.runNext();
      expect(results).toEqual([2]);
      expect(clock.now()).toBe(50);
    });

    test('runAll should execute all pending timers', async () => {
      const clock = new VirtualClock();
      const results: number[] = [];

      clock.setTimeout(() => results.push(1), 100);
      clock.setTimeout(() => results.push(2), 200);
      clock.setTimeout(() => results.push(3), 50);

      await clock.runAll();
      expect(results).toEqual([3, 1, 2]);
    });

    test('reset should clear all timers', async () => {
      const clock = new VirtualClock();
      let executed = false;

      clock.setTimeout(() => {
        executed = true;
      }, 100);

      clock.reset();
      expect(clock.now()).toBe(0);

      await clock.advance(200);
      expect(executed).toBe(false);
    });
  });

  describe('RealClock', () => {
    test('should use real setTimeout', async () => {
      const clock = new RealClock();
      let executed = false;

      clock.setTimeout(() => {
        executed = true;
      }, 10);

      await new Promise((resolve) => setTimeout(resolve, 20));
      expect(executed).toBe(true);
    });
  });

  describe('scheduler.schedule', () => {
    test('should schedule a message', async () => {
      const messages: Message[] = [];

      // Create test actor to receive scheduled messages
      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          messages.push(message);
          return createResponse(message, { success: true });
        }
      }

      const testActor = new TestActor('test-actor', router);
      router.registerActor('/test-actor', testActor);

      // Schedule a message
      const response = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 1000,
            message: {
              to: address('/test-actor'),
              type: 'hello',
              payload: { data: 'world' },
            },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload).toHaveProperty('scheduleId');

      // Advance time to trigger message
      await virtualClock.advance(1000);

      expect(messages).toHaveLength(1);
      expect(messages[0].type).toBe('hello');
      expect(messages[0].payload).toEqual({ data: 'world' });
    });

    test('should not execute message before delay', async () => {
      const messages: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          messages.push(message);
          return createResponse(message, { success: true });
        }
      }

      const testActor = new TestActor('test-actor', router);
      router.registerActor('/test-actor', testActor);

      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 1000,
            message: {
              to: address('/test-actor'),
              type: 'hello',
            },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      // Advance time partially
      await virtualClock.advance(500);
      expect(messages).toHaveLength(0);

      // Advance rest of time
      await virtualClock.advance(500);
      expect(messages).toHaveLength(1);
    });

    test('should handle multiple scheduled messages', async () => {
      const messages: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          messages.push(message);
          return createResponse(message, { success: true });
        }
      }

      const testActor = new TestActor('test-actor', router);
      router.registerActor('/test-actor', testActor);

      // Schedule multiple messages
      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 100,
            message: { to: address('/test-actor'), type: 'msg1' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 200,
            message: { to: address('/test-actor'), type: 'msg2' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 50,
            message: { to: address('/test-actor'), type: 'msg3' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      // Advance time
      await virtualClock.advance(250);

      expect(messages).toHaveLength(3);
      expect(messages[0].type).toBe('msg3');
      expect(messages[1].type).toBe('msg1');
      expect(messages[2].type).toBe('msg2');
    });
  });

  describe('scheduler.recurring', () => {
    test('should schedule recurring message', async () => {
      const messages: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          messages.push(message);
          return createResponse(message, { success: true });
        }
      }

      const testActor = new TestActor('test-actor', router);
      router.registerActor('/test-actor', testActor);

      const response = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.recurring',
          {
            interval: 100,
            message: {
              to: address('/test-actor'),
              type: 'ping',
            },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload).toHaveProperty('scheduleId');

      // Advance time to trigger multiple messages
      await virtualClock.advance(350);

      expect(messages).toHaveLength(3);
      expect(messages.every((m) => m.type === 'ping')).toBe(true);
    });
  });

  describe('scheduler.cancel', () => {
    test('should cancel scheduled message', async () => {
      const messages: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          messages.push(message);
          return createResponse(message, { success: true });
        }
      }

      const testActor = new TestActor('test-actor', router);
      router.registerActor('/test-actor', testActor);

      // Schedule a message
      const scheduleResponse = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 1000,
            message: { to: address('/test-actor'), type: 'hello' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      const scheduleId = scheduleResponse.payload?.scheduleId;

      // Cancel it
      const cancelResponse = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.cancel',
          { scheduleId },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(cancelResponse.success).toBe(true);

      // Advance time
      await virtualClock.advance(1500);

      // Message should not be delivered
      expect(messages).toHaveLength(0);
    });

    test('should cancel recurring message', async () => {
      const messages: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          messages.push(message);
          return createResponse(message, { success: true });
        }
      }

      const testActor = new TestActor('test-actor', router);
      router.registerActor('/test-actor', testActor);

      // Schedule recurring
      const scheduleResponse = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.recurring',
          {
            interval: 100,
            message: { to: address('/test-actor'), type: 'ping' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      const scheduleId = scheduleResponse.payload?.scheduleId;

      // Let it run twice
      await virtualClock.advance(200);
      expect(messages).toHaveLength(2);

      // Cancel it
      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.cancel',
          { scheduleId },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      // Advance more time
      await virtualClock.advance(300);

      // Should not receive more messages
      expect(messages).toHaveLength(2);
    });

    test('should return error for non-existent schedule', async () => {
      const response = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.cancel',
          { scheduleId: 'non-existent' },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(false);
      expect(response.error).toContain('Schedule not found');
    });
  });

  describe('scheduler.list', () => {
    test('should list active schedules', async () => {
      // Schedule some messages
      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 1000,
            message: { to: address('/test'), type: 'msg1' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.recurring',
          {
            interval: 500,
            message: { to: address('/test'), type: 'msg2' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      // List schedules
      const response = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.list',
          {},
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload).toHaveProperty('schedules');
      expect(response.payload?.count).toBe(2);

      const schedules = response.payload?.schedules;
      expect(schedules).toHaveLength(2);

      const oneTime = schedules.find((s: any) => !s.recurring);
      const recurring = schedules.find((s: any) => s.recurring);

      expect(oneTime).toBeDefined();
      expect(recurring).toBeDefined();
      expect(recurring.interval).toBe(500);
    });

    test('should return empty list when no schedules', async () => {
      const response = await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.list',
          {},
          { pattern: 'ask', from: address('/caller') }
        )
      );

      expect(response.success).toBe(true);
      expect(response.payload?.count).toBe(0);
      expect(response.payload?.schedules).toHaveLength(0);
    });
  });

  describe('Actor base class helpers', () => {
    test('schedule() should work from actor', async () => {
      const received: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          received.push(message);

          if (message.type === 'start') {
            await this.schedule(100, 'timeout', { reason: 'test' });
            return createResponse(message, { status: 'scheduled' });
          }

          return createResponse(message, { status: 'ok' });
        }
      }

      const testActor = new TestActor('/test', router);
      router.registerActor('/test', testActor);

      // Start the actor
      await testActor.receive(
        createMessage(address('/test'), 'start', {}, { pattern: 'ask', from: address('/caller') })
      );

      // Advance time
      await virtualClock.advance(100);

      expect(received).toHaveLength(2);
      expect(received[1].type).toBe('timeout');
      expect(received[1].payload).toEqual({ reason: 'test' });
    });

    test('scheduleRecurring() should work from actor', async () => {
      const received: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          received.push(message);

          if (message.type === 'start') {
            await this.scheduleRecurring(50, 'tick');
            return createResponse(message, { status: 'scheduled' });
          }

          return createResponse(message, { status: 'ok' });
        }
      }

      const testActor = new TestActor('/test', router);
      router.registerActor('/test', testActor);

      // Start the actor
      await testActor.receive(
        createMessage(address('/test'), 'start', {}, { pattern: 'ask', from: address('/caller') })
      );

      // Advance time
      await virtualClock.advance(175);

      expect(received.filter((m) => m.type === 'tick')).toHaveLength(3);
    });

    test('cancelSchedule() should work from actor', async () => {
      const received: Message[] = [];
      let scheduleId: string | null = null;

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          received.push(message);

          if (message.type === 'start') {
            scheduleId = await this.schedule(100, 'timeout');
            return createResponse(message, { status: 'scheduled' });
          }

          if (message.type === 'cancel') {
            if (scheduleId) {
              await this.cancelSchedule(scheduleId);
            }
            return createResponse(message, { status: 'cancelled' });
          }

          return createResponse(message, { status: 'ok' });
        }
      }

      const testActor = new TestActor('/test', router);
      router.registerActor('/test', testActor);

      // Start and schedule
      await testActor.receive(
        createMessage(address('/test'), 'start', {}, { pattern: 'ask', from: address('/caller') })
      );

      // Cancel before it fires
      await testActor.receive(
        createMessage(address('/test'), 'cancel', {}, { pattern: 'ask', from: address('/caller') })
      );

      // Advance time
      await virtualClock.advance(200);

      // Should not have received timeout
      expect(received.filter((m) => m.type === 'timeout')).toHaveLength(0);
    });
  });

  describe('shutdown', () => {
    test('should cancel all schedules on shutdown', async () => {
      const messages: Message[] = [];

      class TestActor extends Actor {
        async receive(message: Message): Promise<MessageResponse> {
          messages.push(message);
          return createResponse(message, { success: true });
        }
      }

      const testActor = new TestActor('test-actor', router);
      router.registerActor('/test-actor', testActor);

      // Schedule multiple messages
      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.schedule',
          {
            delay: 1000,
            message: { to: address('/test-actor'), type: 'msg1' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      await scheduler.receive(
        createMessage(
          address('/system/scheduler'),
          'scheduler.recurring',
          {
            interval: 500,
            message: { to: address('/test-actor'), type: 'msg2' },
          },
          { pattern: 'ask', from: address('/caller') }
        )
      );

      // Shutdown
      await scheduler.shutdown();

      // Advance time
      await virtualClock.advance(2000);

      // No messages should be delivered
      expect(messages).toHaveLength(0);
    });
  });
});
