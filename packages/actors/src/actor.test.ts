/**
 * Tests for Actor auto-validation feature
 *
 * Verifies that the base Actor class automatically validates
 * message payloads when schemas are defined as class properties.
 */

import { describe, it, expect, beforeEach } from 'bun:test';
import {
  Actor,
  type Message,
  type MessageResponse,
  createMessage,
  address,
  MapActorRegistry,
  MessageRouter,
} from './index.ts';
import type { JSONSchema } from './introspection.ts';

describe('Actor auto-validation', () => {
  let router: MessageRouter;

  beforeEach(() => {
    const registry = new MapActorRegistry();
    router = new MessageRouter(registry);
  });

  describe('No schemas defined', () => {
    class NoSchemaActor extends Actor {
      // No schemas defined - validation skipped
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

    it('skips validation when no schemas defined', async () => {
      const actor = new NoSchemaActor('test-actor', router);

      const message = createMessage(
        address('test-actor'),
        'any-type',
        { anything: 'goes' },
        { pattern: 'ask', from: address('sender') }
      );

      const response = await actor.receive(message);
      expect(response.success).toBe(true);
    });
  });

  describe('Auto-validation with schema', () => {
    class TestActor extends Actor {
      // Define schemas as class property
      protected schemas = new Map<string, JSONSchema>([
        ['create', {
          type: 'object',
          properties: {
            name: { type: 'string', minLength: 1 },
            age: { type: ['number', 'integer'], minimum: 0 },
          },
          required: ['name'],
        }],
        ['create-user', {
          type: 'object',
          properties: {
            name: { type: 'string', minLength: 1 },
          },
          required: ['name'],
        }],
        ['unregistered-type', undefined as any], // Will be filtered
      ]);

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

    it('validates payload against defined schema', async () => {
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

    it('skips validation when schema not defined', async () => {
      const actor = new TestActor('test-actor', router);

      // No schema defined for 'unknown-message-type'
      const message = createMessage(
        address('test-actor'),
        'unknown-message-type',
        { anything: 'goes' },
        { pattern: 'ask', from: address('sender') }
      );

      const response = await actor.receive(message);
      expect(response.success).toBe(true);
    });
  });

  describe('Auto-validation opt-out', () => {
    class NoValidationActor extends Actor {
      // Define schemas
      protected schemas = new Map<string, JSONSchema>([
        ['strict-type', {
          type: 'object',
          properties: {
            required: { type: 'string' },
          },
          required: ['required'],
        }],
      ]);

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
      protected schemas = new Map<string, JSONSchema>([
        ['detailed-schema', {
          type: 'object',
          properties: {
            email: { type: 'string', pattern: '^[^@]+@[^@]+\\.[^@]+$' },
            age: { type: ['number', 'integer'], minimum: 0, maximum: 150 },
          },
          required: ['email', 'age'],
        }],
      ]);

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
