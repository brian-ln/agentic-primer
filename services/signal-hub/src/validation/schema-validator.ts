/**
 * Schema Validation Module
 *
 * Runtime JSON Schema validation for all Signal Hub messages.
 *
 * Strategy:
 * - TEST MODE (NODE_ENV=test or vitest): Strict validation - THROW on violations
 * - PRODUCTION MODE: Log-only validation - LOG violations but continue
 *
 * This allows safe rollout of schema validation without breaking production,
 * while ensuring tests catch schema violations immediately.
 */

import Ajv from 'ajv';
import addFormats from 'ajv-formats';
import type { SharedMessage } from '../types';

// Import schemas (TypeScript will load them via resolveJsonModule)
import sharedMessageSchema from '../../spec/schemas/shared-message.schema.json';
import canonicalAddressSchema from '../../spec/schemas/canonical-address.schema.json';
import errorResponseSchema from '../../spec/schemas/error-response.schema.json';

export interface ValidationResult {
  valid: boolean;
  errors?: string[];
  messageType?: string;
}

export interface ValidationMode {
  mode: 'strict' | 'log-only';
  failOnError: boolean;
}

/**
 * SchemaValidator class
 *
 * Validates messages against JSON Schema definitions.
 * Supports strict mode (throw on error) and log-only mode (warn only).
 */
export class SchemaValidator {
  private ajv: Ajv;
  private mode: ValidationMode;
  private validateSharedMessage: any;

  constructor(mode: ValidationMode = { mode: 'log-only', failOnError: false }) {
    this.mode = mode;

    // Initialize Ajv with strict mode disabled to allow additional properties
    this.ajv = new Ajv({
      allErrors: true,
      strict: false, // Allow additional properties not in schema
      validateFormats: true,
      schemas: [canonicalAddressSchema], // Add referenced schemas
    });

    // Add format validators (uuid, date-time, etc.)
    addFormats(this.ajv);

    // Compile shared-message schema
    this.validateSharedMessage = this.ajv.compile(sharedMessageSchema);

    console.log(JSON.stringify({
      event: 'schema_validator_initialized',
      mode: this.mode.mode,
      failOnError: this.mode.failOnError,
      timestamp: Date.now(),
    }));
  }

  /**
   * Validate SharedMessage envelope
   *
   * @param message - Message to validate
   * @param direction - 'incoming' (from client) or 'outgoing' (to client)
   * @returns Validation result
   * @throws Error if in strict mode and validation fails
   */
  validateMessage(message: SharedMessage, direction: 'incoming' | 'outgoing'): ValidationResult {
    const valid = this.validateSharedMessage(message);

    if (!valid) {
      const errors = this.validateSharedMessage.errors?.map((err: any) =>
        `${err.instancePath || '(root)'} ${err.message}`
      ) || ['Unknown validation error'];

      const errorDetail = {
        direction,
        messageType: message.type || '(missing type)',
        errors,
        // Only include full message in strict mode (tests), redact in production
        message: this.mode.mode === 'strict' ? message : '[redacted in log-only mode]',
      };

      if (this.mode.failOnError) {
        // Test mode: throw error to fail the test
        console.error('[SCHEMA_VALIDATION_FAILED]', JSON.stringify(errorDetail));
        throw new Error(`Schema validation failed (${direction}): ${errors.join(', ')}`);
      } else {
        // Production mode: log warning but don't block
        console.warn('[SCHEMA_VALIDATION_WARNING]', JSON.stringify(errorDetail));
      }

      return { valid: false, errors, messageType: message.type };
    }

    // Optional: Log successful validations in strict mode for debugging
    if (this.mode.mode === 'strict') {
      console.log(JSON.stringify({
        event: 'schema_validation_success',
        direction,
        messageType: message.type,
        timestamp: Date.now(),
      }));
    }

    return { valid: true, messageType: message.type };
  }

  /**
   * Validate error response payload
   *
   * @param payload - Error response payload to validate
   * @returns Validation result
   */
  validateErrorResponse(payload: unknown): ValidationResult {
    const validator = this.ajv.compile(errorResponseSchema);
    const valid = validator(payload);

    if (!valid) {
      const errors = validator.errors?.map((err: any) =>
        `${err.instancePath || '(root)'} ${err.message}`
      ) || ['Unknown validation error'];

      if (this.mode.failOnError) {
        console.error('[ERROR_RESPONSE_VALIDATION_FAILED]', JSON.stringify({ errors, payload }));
        throw new Error(`Error response validation failed: ${errors.join(', ')}`);
      } else {
        console.warn('[ERROR_RESPONSE_VALIDATION_WARNING]', JSON.stringify({ errors }));
      }

      return { valid: false, errors };
    }

    return { valid: true };
  }

  /**
   * Get current validation mode
   */
  getMode(): ValidationMode {
    return this.mode;
  }
}

/**
 * Detect validation mode based on environment
 *
 * Priority:
 * 1. SCHEMA_VALIDATION_MODE env var (override)
 * 2. Test environment detection (NODE_ENV=test, VITEST=true, or vitest globals)
 * 3. Default to log-only (production safe)
 *
 * @returns ValidationMode configuration
 */
export function getValidationMode(): ValidationMode {
  // Check for explicit override
  const envMode = process.env.SCHEMA_VALIDATION_MODE;
  if (envMode === 'strict') {
    return { mode: 'strict', failOnError: true };
  }
  if (envMode === 'log-only') {
    return { mode: 'log-only', failOnError: false };
  }

  // Auto-detect test environment
  const isTest =
    process.env.NODE_ENV === 'test' ||
    process.env.VITEST === 'true' ||
    typeof (globalThis as any).describe === 'function';

  return isTest
    ? { mode: 'strict', failOnError: true }
    : { mode: 'log-only', failOnError: false };
}

// Singleton instance
let validatorInstance: SchemaValidator | null = null;

/**
 * Get singleton validator instance
 *
 * Uses auto-detected validation mode based on environment.
 * Call resetValidator() to force re-initialization with new mode.
 */
export function getValidator(): SchemaValidator {
  if (!validatorInstance) {
    validatorInstance = new SchemaValidator(getValidationMode());
  }
  return validatorInstance;
}

/**
 * Reset validator singleton (for testing)
 *
 * Forces re-initialization on next getValidator() call.
 */
export function resetValidator(): void {
  validatorInstance = null;
}
