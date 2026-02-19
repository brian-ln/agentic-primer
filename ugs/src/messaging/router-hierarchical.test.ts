#!/usr/bin/env bun
/**
 * Router Hierarchical Routing Tests
 *
 * Tests for MessageRouter's hierarchical routing support.
 * Validates integration between router and supervisor-based delegation.
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import { MessageRouter } from './router';
import { SupervisorBase, LeafActor } from '@agentic-primer/actors';
import { address, createMessage, generateCorrelationId } from '@agentic-primer/actors';
import GraphStore from '@src/graph.ts';
import { ProgramManager } from '@src/entities/program.ts';

describe('MessageRouter - Hierarchical Routing', () => {
  let store: GraphStore;
  let programManager: ProgramManager;
  let router: MessageRouter;

  beforeEach(() => {
    store = new GraphStore();
    programManager = new ProgramManager(store);
    router = new MessageRouter(store, programManager);
  });

  describe('Router Integration', () => {
    test('registers supervisor with router', () => {
      const supervisor = new SupervisorBase('domain', router);

      router.registerActor('domain', supervisor);

      // Should not throw
      expect(true).toBe(true);
    });

    test('routes hierarchical message through registered supervisor', async () => {
      const inference = new LeafActor('inference', router, async (msg) => ({
        actor: 'inference',
        payload: msg.payload,
      }));

      router.registerActor('inference', inference);

      const message = createMessage(
        address('/inference'),
        'process',
        { input: 'test' },
        {
          pattern: 'ask',
          correlationId: generateCorrelationId(),
          from: address('client'),
        }
      );

      const response = await router.ask(message);

      expect(response.success).toBe(true);
      expect(response.payload.actor).toBe('inference');
      expect(response.payload.payload).toEqual({ input: 'test' });
    });

    test('routes through multi-level hierarchy via router', async () => {
      const domain = new SupervisorBase('domain', router);
      const services = new SupervisorBase('services', router);
      const llm = new LeafActor('llm', router, async (msg) => ({
        service: 'llm',
        depth: 3,
        data: msg.payload,
      }));

      services.addChild('llm', llm);
      domain.addChild('services', services);

      router.registerActor('domain', domain);

      const message = createMessage(
        address('/domain/services/llm'),
        'inference',
        { prompt: 'hello' },
        {
          pattern: 'ask',
          correlationId: generateCorrelationId(),
          from: address('client'),
        }
      );

      const response = await router.ask(message);

      expect(response.success).toBe(true);
      expect(response.payload.service).toBe('llm');
      expect(response.payload.depth).toBe(3);
      expect(response.payload.data).toEqual({ prompt: 'hello' });
    });

    test('returns error if root supervisor not registered', async () => {
      const message = createMessage(
        address('unregistered/child'),
        'test',
        {},
        {
          pattern: 'ask',
          correlationId: generateCorrelationId(),
          from: address('client'),
        }
      );

      const response = await router.ask(message);

      expect(response.success).toBe(false);
      expect(response.error).toContain('Root supervisor not found');
      expect(response.error).toContain('unregistered');
    });
  });

  describe('Tell Pattern', () => {
    test('routes tell messages hierarchically', async () => {
      let messageReceived = false;
      let receivedPayload: any = null;

      const leaf = new LeafActor('leaf', router, async (msg) => {
        messageReceived = true;
        receivedPayload = msg.payload;
        return { ok: true };
      });

      router.registerActor('leaf', leaf);

      const message = createMessage(
        address('/leaf'),
        'notify',
        { data: 'test notification' },
        {
          pattern: 'tell',
          from: address('client'),
        }
      );

      await router.tell(message);

      // Give async processing time to complete
      await new Promise(resolve => setTimeout(resolve, 10));

      expect(messageReceived).toBe(true);
      expect(receivedPayload).toEqual({ data: 'test notification' });
    });
  });

  describe('Error Handling', () => {
    test('propagates error from leaf actor', async () => {
      const failingLeaf = new LeafActor('failing', router, async () => {
        throw new Error('Leaf actor failed');
      });

      router.registerActor('failing', failingLeaf);

      const message = createMessage(
        address('/failing'),
        'test',
        {},
        {
          pattern: 'ask',
          correlationId: generateCorrelationId(),
          from: address('client'),
        }
      );

      const response = await router.ask(message);

      expect(response.success).toBe(false);
      expect(response.error).toBe('Leaf actor failed');
    });

    test('returns error for non-existent actor', async () => {
      const message = createMessage(
        address('/nonexistent'),
        'test',
        {},
        {
          pattern: 'ask',
          correlationId: generateCorrelationId(),
          from: address('client'),
        }
      );

      const response = await router.ask(message);

      expect(response.success).toBe(false);
      expect(response.error).toContain('Root supervisor not found: nonexistent');
    });
  });

  describe('Performance', () => {
    test('handles concurrent hierarchical requests', async () => {
      const domain = new SupervisorBase('domain', router);

      // Add multiple leaf actors
      for (let i = 0; i < 5; i++) {
        const leaf = new LeafActor(`worker-${i}`, router, async (msg) => ({
          worker: i,
          payload: msg.payload,
        }));
        domain.addChild(`worker-${i}`, leaf);
      }

      router.registerActor('domain', domain);

      // Send concurrent messages
      const promises = [];
      for (let i = 0; i < 50; i++) {
        const workerId = i % 5;
        const message = createMessage(
          address(`/domain/worker-${workerId}`),
          'work',
          { task: i },
          {
            pattern: 'ask',
            correlationId: generateCorrelationId(),
            from: address('client'),
          }
        );
        promises.push(router.ask(message));
      }

      const responses = await Promise.all(promises);

      // All should succeed
      expect(responses.length).toBe(50);
      for (let i = 0; i < 50; i++) {
        expect(responses[i].success).toBe(true);
        expect(responses[i].payload.worker).toBe(i % 5);
        expect(responses[i].payload.payload.task).toBe(i);
      }
    });
  });
});
