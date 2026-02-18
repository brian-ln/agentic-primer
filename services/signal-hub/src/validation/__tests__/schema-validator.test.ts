/**
 * Schema Validator Tests
 *
 * Tests for runtime JSON Schema validation:
 * - Strict mode (test environment)
 * - Log-only mode (production environment)
 * - Valid and invalid message validation
 * - Error response validation
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  SchemaValidator,
  getValidationMode,
  getValidator,
  resetValidator,
  type ValidationMode,
} from '../schema-validator';
import type { SharedMessage } from '../../types';

describe('Schema Validator', () => {
  afterEach(() => {
    resetValidator();
  });

  describe('getValidationMode', () => {
    it('should detect test environment and return strict mode', () => {
      const mode = getValidationMode();
      expect(mode.mode).toBe('strict');
      expect(mode.failOnError).toBe(true);
    });

    it('should respect SCHEMA_VALIDATION_MODE=strict override', () => {
      const originalEnv = process.env.SCHEMA_VALIDATION_MODE;
      process.env.SCHEMA_VALIDATION_MODE = 'strict';

      const mode = getValidationMode();
      expect(mode.mode).toBe('strict');
      expect(mode.failOnError).toBe(true);

      process.env.SCHEMA_VALIDATION_MODE = originalEnv;
    });

    it('should respect SCHEMA_VALIDATION_MODE=log-only override', () => {
      const originalEnv = process.env.SCHEMA_VALIDATION_MODE;
      process.env.SCHEMA_VALIDATION_MODE = 'log-only';

      const mode = getValidationMode();
      expect(mode.mode).toBe('log-only');
      expect(mode.failOnError).toBe(false);

      process.env.SCHEMA_VALIDATION_MODE = originalEnv;
    });
  });

  describe('SchemaValidator - Strict Mode', () => {
    let validator: SchemaValidator;

    beforeEach(() => {
      validator = new SchemaValidator({ mode: 'strict', failOnError: true });
    });

    it('should validate a valid SharedMessage', () => {
      const validMessage: SharedMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: '@(browser/client)',
        to: '@(cloudflare/signal-hub)',
        payload: { version: '1.0.0' },
        pattern: 'ask',
        timestamp: Date.now(),
        correlationId: null,
        ttl: null,
        metadata: {},
        signature: null,
      };

      const result = validator.validateMessage(validMessage, 'incoming');

      expect(result.valid).toBe(true);
      expect(result.messageType).toBe('hub:connect');
      expect(result.errors).toBeUndefined();
    });

    it('should throw on invalid SharedMessage in strict mode - missing required fields', () => {
      const invalidMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: '@(browser/client)',
        // Missing 'to' field (required)
        payload: { version: '1.0.0' },
      } as any;

      expect(() => {
        validator.validateMessage(invalidMessage, 'incoming');
      }).toThrow(/Schema validation failed/);
    });

    it('should throw on invalid SharedMessage in strict mode - wrong type for field', () => {
      const invalidMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: '@(browser/client)',
        to: '@(cloudflare/signal-hub)',
        payload: { version: '1.0.0' },
        pattern: 'invalid-pattern' as any, // Must be 'tell' or 'ask'
        timestamp: Date.now(),
        correlationId: null,
        ttl: null,
        metadata: {},
        signature: null,
      } as any;

      expect(() => {
        validator.validateMessage(invalidMessage, 'incoming');
      }).toThrow(/Schema validation failed/);
    });

    it('should throw on invalid canonical address format', () => {
      const invalidMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: 'invalid-address', // Should be '@(runtime/actor)'
        to: '@(cloudflare/signal-hub)',
        payload: { version: '1.0.0' },
        pattern: 'ask',
        timestamp: Date.now(),
        correlationId: null,
        ttl: null,
        metadata: {},
        signature: null,
      } as any;

      expect(() => {
        validator.validateMessage(invalidMessage, 'incoming');
      }).toThrow(/Schema validation failed/);
    });

    it('should throw on negative timestamp', () => {
      const invalidMessage: SharedMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: '@(browser/client)',
        to: '@(cloudflare/signal-hub)',
        payload: { version: '1.0.0' },
        pattern: 'ask',
        timestamp: -1000, // Must be >= 0
        correlationId: null,
        ttl: null,
        metadata: {},
        signature: null,
      };

      expect(() => {
        validator.validateMessage(invalidMessage, 'incoming');
      }).toThrow(/Schema validation failed/);
    });

    it('should validate correlationId as UUID format if present', () => {
      const invalidMessage = {
        id: 'test-id-123',
        type: 'hub:connected',
        from: '@(cloudflare/signal-hub)',
        to: '@(browser/client)',
        payload: {},
        pattern: 'tell',
        timestamp: Date.now(),
        correlationId: 'not-a-uuid', // Should be UUID format or null
        ttl: null,
        metadata: {},
        signature: null,
      } as any;

      expect(() => {
        validator.validateMessage(invalidMessage, 'incoming');
      }).toThrow(/Schema validation failed/);
    });
  });

  describe('SchemaValidator - Log-Only Mode', () => {
    let validator: SchemaValidator;

    beforeEach(() => {
      validator = new SchemaValidator({ mode: 'log-only', failOnError: false });
    });

    it('should not throw on invalid message in log-only mode', () => {
      const invalidMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: '@(browser/client)',
        // Missing 'to' field (required)
        payload: { version: '1.0.0' },
      } as any;

      // Should NOT throw, just log warning
      expect(() => {
        validator.validateMessage(invalidMessage, 'incoming');
      }).not.toThrow();
    });

    it('should return validation result with errors in log-only mode', () => {
      const invalidMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: '@(browser/client)',
        // Missing 'to' field (required)
        payload: { version: '1.0.0' },
      } as any;

      const result = validator.validateMessage(invalidMessage, 'incoming');

      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
      expect(result.errors!.length).toBeGreaterThan(0);
    });

    it('should still validate valid messages in log-only mode', () => {
      const validMessage: SharedMessage = {
        id: 'test-id-123',
        type: 'hub:connect',
        from: '@(browser/client)',
        to: '@(cloudflare/signal-hub)',
        payload: { version: '1.0.0' },
        pattern: 'ask',
        timestamp: Date.now(),
        correlationId: null,
        ttl: null,
        metadata: {},
        signature: null,
      };

      const result = validator.validateMessage(validMessage, 'incoming');

      expect(result.valid).toBe(true);
      expect(result.messageType).toBe('hub:connect');
    });
  });

  describe('Error Response Validation', () => {
    let validator: SchemaValidator;

    beforeEach(() => {
      validator = new SchemaValidator({ mode: 'strict', failOnError: true });
    });

    it('should validate a valid error response', () => {
      const validErrorResponse = {
        code: 'unauthorized',
        message: 'Invalid JWT token',
        details: {
          reason: 'Token expired',
        },
      };

      const result = validator.validateErrorResponse(validErrorResponse);

      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it('should throw on invalid error response - missing required fields', () => {
      const invalidErrorResponse = {
        code: 'unauthorized',
        // Missing 'message' field (required)
      };

      expect(() => {
        validator.validateErrorResponse(invalidErrorResponse);
      }).toThrow(/Error response validation failed/);
    });

    it('should throw on invalid error code', () => {
      const invalidErrorResponse = {
        code: 'invalid_code', // Not in enum
        message: 'Test error',
      };

      expect(() => {
        validator.validateErrorResponse(invalidErrorResponse);
      }).toThrow(/Error response validation failed/);
    });

    it('should throw on empty message string', () => {
      const invalidErrorResponse = {
        code: 'internal_error',
        message: '', // minLength: 1
      };

      expect(() => {
        validator.validateErrorResponse(invalidErrorResponse);
      }).toThrow(/Error response validation failed/);
    });
  });

  describe('Singleton Validator', () => {
    it('should return the same instance on multiple getValidator calls', () => {
      const validator1 = getValidator();
      const validator2 = getValidator();

      expect(validator1).toBe(validator2);
    });

    it('should create new instance after resetValidator', () => {
      const validator1 = getValidator();
      resetValidator();
      const validator2 = getValidator();

      expect(validator1).not.toBe(validator2);
    });
  });
});
