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
 *
 * NOTE: Uses handwritten validators instead of ajv to avoid eval/new Function()
 * which is blocked in Cloudflare Workers runtime.
 */

import type { SharedMessage } from '../types';

export interface ValidationResult {
  valid: boolean;
  errors?: string[];
  messageType?: string;
}

export interface ValidationMode {
  mode: 'strict' | 'log-only';
  failOnError: boolean;
}

// Valid error codes from error-response.schema.json
const VALID_ERROR_CODES = new Set([
  'version_mismatch',
  'unauthorized',
  'rate_limited',
  'unknown_actor',
  'message_too_large',
  'message_expired',
  'timeout',
  'internal_error',
  'invalid_token',
]);

// Valid message patterns from shared-message.schema.json
const VALID_PATTERNS = new Set(['tell', 'ask']);

// Canonical address pattern: @(<runtime>/<actor-id>)
const CANONICAL_ADDRESS_RE = /^@\([a-z0-9-]+\/[a-z0-9-]+\)$/;

// UUID v4 pattern (loose - any 8-4-4-4-12 hex format)
const UUID_RE = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

function isValidCanonicalAddress(value: unknown): boolean {
  return typeof value === 'string' && CANONICAL_ADDRESS_RE.test(value);
}

function isValidUUID(value: unknown): boolean {
  return typeof value === 'string' && UUID_RE.test(value);
}

/**
 * Validate a SharedMessage envelope against the shared-message schema.
 * Returns an array of error strings (empty = valid).
 */
function validateSharedMessageFields(data: unknown): string[] {
  const errors: string[] = [];

  if (typeof data !== 'object' || data === null) {
    errors.push('(root) must be object');
    return errors;
  }

  const msg = data as Record<string, unknown>;

  // Required: type (string)
  if (typeof msg.type !== 'string') {
    errors.push('/type must be string');
  }

  // Required: from (CanonicalAddress)
  if (!isValidCanonicalAddress(msg.from)) {
    errors.push('/from must be a valid CanonicalAddress (@(<runtime>/<actor-id>))');
  }

  // Required: to (CanonicalAddress)
  if (!isValidCanonicalAddress(msg.to)) {
    errors.push('/to must be a valid CanonicalAddress (@(<runtime>/<actor-id>))');
  }

  // Required: payload (any value, but key must exist)
  if (!Object.prototype.hasOwnProperty.call(msg, 'payload')) {
    errors.push('/payload is required');
  }

  // Optional: pattern (enum)
  if (msg.pattern !== undefined && !VALID_PATTERNS.has(msg.pattern as string)) {
    errors.push('/pattern must be "tell" or "ask"');
  }

  // Optional: timestamp (non-negative integer)
  if (msg.timestamp !== undefined) {
    if (typeof msg.timestamp !== 'number' || !Number.isInteger(msg.timestamp) || msg.timestamp < 0) {
      errors.push('/timestamp must be a non-negative integer');
    }
  }

  // Optional: correlationId (uuid string or null)
  if (msg.correlationId !== undefined && msg.correlationId !== null) {
    if (!isValidUUID(msg.correlationId)) {
      errors.push('/correlationId must be a UUID string or null');
    }
  }

  // Optional: ttl (non-negative integer or null)
  if (msg.ttl !== undefined && msg.ttl !== null) {
    if (typeof msg.ttl !== 'number' || !Number.isInteger(msg.ttl) || msg.ttl < 0) {
      errors.push('/ttl must be a non-negative integer or null');
    }
  }

  // Optional: metadata (object)
  if (msg.metadata !== undefined && (typeof msg.metadata !== 'object' || msg.metadata === null)) {
    errors.push('/metadata must be an object');
  }

  return errors;
}

/**
 * Validate an error response payload against the error-response schema.
 * Returns an array of error strings (empty = valid).
 */
function validateErrorResponseFields(data: unknown): string[] {
  const errors: string[] = [];

  if (typeof data !== 'object' || data === null) {
    errors.push('(root) must be object');
    return errors;
  }

  const payload = data as Record<string, unknown>;

  // Required: code (enum)
  if (!VALID_ERROR_CODES.has(payload.code as string)) {
    errors.push(`/code must be one of: ${Array.from(VALID_ERROR_CODES).join(', ')}`);
  }

  // Required: message (string, minLength 1)
  if (typeof payload.message !== 'string' || payload.message.length < 1) {
    errors.push('/message must be a non-empty string');
  }

  // Required: resolution (string, minLength 1)
  if (typeof payload.resolution !== 'string' || payload.resolution.length < 1) {
    errors.push('/resolution must be a non-empty string');
  }

  // Optional: details (object)
  if (payload.details !== undefined && (typeof payload.details !== 'object' || payload.details === null)) {
    errors.push('/details must be an object');
  }

  return errors;
}

/**
 * SchemaValidator class
 *
 * Validates messages against JSON Schema definitions using handwritten validators.
 * Supports strict mode (throw on error) and log-only mode (warn only).
 */
export class SchemaValidator {
  private mode: ValidationMode;

  constructor(mode: ValidationMode = { mode: 'log-only', failOnError: false }) {
    this.mode = mode;

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
    const errors = validateSharedMessageFields(message);
    const valid = errors.length === 0;

    if (!valid) {
      const errorDetail = {
        direction,
        messageType: (message as any).type || '(missing type)',
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

      return { valid: false, errors, messageType: (message as any).type };
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
    const errors = validateErrorResponseFields(payload);
    const valid = errors.length === 0;

    if (!valid) {
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
