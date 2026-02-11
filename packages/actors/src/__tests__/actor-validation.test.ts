/**
 * Tests for Actor auto-validation feature
 *
 * Verifies that the base Actor class automatically validates
 * message payloads when schemas are registered.
 */

import { describe, it, expect, beforeEach } from 'bun:test';
import {
  Actor,
  registerMessageSchema,
  hasMessageSchema,
  getMessageSchema,
  type Message,
  type MessageResponse,
  createMessage,
  address,
  MapActorRegistry,
  MessageRouter,
} from '../index.ts';
import type { JSONSchema } from '../introspection.ts';

describe('Actor auto-validation', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const registry = new MapActorRegistry();
    router = new MessageRouter(registry);
  });

  describe('registerMessageSchema', () => {
    it('registers and retrieves schema', () => {
      const schema: JSONSchema = {
        type: 'object',
        properties: {
          name: { type: 'string' },
        },
        required: ['name'],
      };

      registerMessageSchema('test-register', schema);

      expect(hasMessageSchema('test-register')).toBe(true);
      expect(getMessageSchema('test-register')).toEqual(schema);
    });

    it('returns undefined for unregistered type', () => {
      expect(hasMessageSchema('nonexistent-type')).toBe(false);
      expect(getMessageSchema('nonexistent-type')).toBeUndefined();
    });
  });

  describe('Auto-validation with schema', () => {
    class TestActor extends Actor {
      protected async handleMessage(message: Message): Promise<MessageResponse> {
        return {
          id: 'response-1',
          correlationId: message.correlationId || message.id,
          from: this.address,
          to: message.from,
          success: true,
          payload: { received: message.type },
          timestamp: Date.now(),
        };
      }
    }

    it('validates payload against registered schema', async () => {
      // Register schema
      registerMessageSchema('create', {
        type: 'object',
        properties: {
          name: { type: 'string', minLength: 1 },
          age: { type: ['number', 'integer'], minimum: 0 },
        },
        required: ['name'],
      });

      const actor = new TestActor('test-actor', router);

      // Valid payload should succeed
      const validMessage = createMessage(
        address('test-actor'),
        'create',
        { name: 'Alice', age: 30 },
        { pattern: 'ask', from: address('sender') }
      );

      const response = await actor.receive(validMessage);
      expect(response.success).toBe(true);
    });

    it('rejects invalid payload', async () => {
      // Register schema
      registerMessageSchema('create-user', {
        type: 'object',
        properties: {
          name: { type: 'string', minLength: 1 },
        },
        required: ['name'],
      });

      const actor = new TestActor('test-actor', router);

      // Missing required field
      const invalidMessage = createMessage(
        address('test-actor'),
        'create-user',
        { age: 30 },
        { pattern: 'ask', from: address('sender') }
      );

      const response = await actor.receive(invalidMessage);
      expect(response.success).toBe(false);
      expect(response.error).toContain('Invalid payload');
      expect(response.error).toContain('name');
    });

    it('skips validation when schema not registered', async () => {
      const actor = new TestActor('test-actor', router);

      // No schema registered for 'unregistered-type'
      const message = createMessage(
        address('test-actor'),
        'unregistered-type',
        { anything: 'goes' },
        { pattern: 'ask', from: address('sender') }
      );

      const response = await actor.receive(message);
      expect(response.success).toBe(true);
    });
  });

  describe('Auto-validation opt-out', () => {
    class NoValidationActor extends Actor {
      constructor(id: string, router: MessageRouter) {
        super(id, router);
        this.enableAutoValidation = false; // Disable auto-validation
      }

      protected async handleMessage(message: Message): Promise<MessageResponse> {
        return {
          id: 'response-1',
          correlationId: message.correlationId || message.id,
          from: this.address,
          to: message.from,
          success: true,
          payload: { received: message.type },
          timestamp: Date.now(),
        };
      }
    }

    it('skips validation when disabled', async () => {
      // Register schema
      registerMessageSchema('strict-type', {
        type: 'object',
        properties: {
          required: { type: 'string' },
        },
        required: ['required'],
      });

      const actor = new NoValidationActor('test-actor', router);

      // Invalid payload (missing 'required' field)
      const message = createMessage(
        address('test-actor'),
        'strict-type',
        { wrong: 'field' },
        { pattern: 'ask', from: address('sender') }
      );

      // Should succeed because validation is disabled
      const response = await actor.receive(message);
      expect(response.success).toBe(true);
    });
  });

  describe('Schema validation errors', () => {
    class DetailedErrorActor extends Actor {
      protected async handleMessage(message: Message): Promise<MessageResponse> {
        return {
          id: 'response-1',
          correlationId: message.correlationId || message.id,
          from: this.address,
          to: message.from,
          success: true,
          payload: {},
          timestamp: Date.now(),
        };
      }
    }

    it('provides detailed validation errors', async () => {
      registerMessageSchema('detailed-schema', {
        type: 'object',
        properties: {
          email: { type: 'string', pattern: '^[^@]+@[^@]+\\.[^@]+$' },
          age: { type: ['number', 'integer'], minimum: 0, maximum: 150 },
        },
        required: ['email', 'age'],
      });

      const actor = new DetailedErrorActor('test-actor', router);

      const message = createMessage(
        address('test-actor'),
        'detailed-schema',
        {
          email: 'invalid-email',
          age: 200,
        },
        { pattern: 'ask', from: address('sender') }
      );

      const response = await actor.receive(message);
      expect(response.success).toBe(false);
      expect(response.error).toContain('email');
      expect(response.error).toContain('pattern');
      expect(response.error).toContain('age');
      expect(response.error).toContain('maximum');
    });
  });
});
