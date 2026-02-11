/**
 * Storage layer validation for defense-in-depth security.
 *
 * These validators provide an independent security layer at the storage boundary.
 * They do NOT trust upstream validation and enforce strict limits based on
 * Cloudflare platform constraints (D1, KV).
 *
 * References:
 * - D1 limits: https://developers.cloudflare.com/d1/platform/limits/
 * - KV limits: https://developers.cloudflare.com/kv/platform/limits/
 */

import type { SqlValue } from '../interfaces.ts';

/**
 * Validate SQL query for dangerous patterns and size limits.
 *
 * Security rules:
 * - Max length: 10,000 characters (prevents resource exhaustion)
 * - Blocks: DROP, EXEC, xp_cmdshell, TRUNCATE (data loss operations)
 * - Allows: SELECT, INSERT, UPDATE, DELETE (standard CRUD)
 *
 * @throws {Error} if query is invalid or dangerous
 */
export function validateSqlQuery(sql: string): void {
  if (sql.length > 10_000) {
    throw new Error(`SQL query exceeds maximum length (10000 chars): got ${sql.length}`);
  }

  // Check for dangerous SQL patterns
  const dangerous = /\b(DROP|EXEC|xp_cmdshell|TRUNCATE)\b/i;
  const match = dangerous.exec(sql);
  if (match) {
    throw new Error(`SQL query contains dangerous pattern: ${match[0]}`);
  }
}

/**
 * Validate SQL parameters for count and size limits.
 *
 * Limits:
 * - Max 100 parameters per query (prevents resource exhaustion)
 * - String params: 10KB max (prevents memory exhaustion)
 * - Blob params: 1MB max (D1 practical limit)
 *
 * @throws {Error} if parameters are invalid
 */
export function validateSqlParams(params?: SqlValue[]): void {
  if (!params) return;

  if (params.length > 100) {
    throw new Error(`SQL parameter count exceeds maximum (100): got ${params.length}`);
  }

  for (let i = 0; i < params.length; i++) {
    const param = params[i];

    if (typeof param === 'string') {
      const bytes = new TextEncoder().encode(param).length;
      if (bytes > 10_000) {
        throw new Error(`SQL parameter ${i} exceeds maximum string length (10000 bytes): got ${bytes}`);
      }
    } else if (param instanceof Uint8Array) {
      if (param.byteLength > 1_048_576) {
        throw new Error(`SQL parameter ${i} exceeds maximum blob size (1MB): got ${param.byteLength}`);
      }
    }
  }
}

/**
 * Validate SQL batch size.
 *
 * D1 limit: 25 statements per batch
 *
 * @throws {Error} if batch size is invalid
 */
export function validateBatchSize(count: number): void {
  if (count > 25) {
    throw new Error(`SQL batch exceeds maximum size (25 statements): got ${count}`);
  }
  if (count < 1) {
    throw new Error(`SQL batch must contain at least 1 statement`);
  }
}

/**
 * Validate KV key format and size.
 *
 * Cloudflare KV constraints:
 * - Max 512 bytes (UTF-8 encoded)
 * - No path traversal sequences (../, ..\)
 * - No control characters (0x00-0x1F, 0x7F)
 *
 * @throws {Error} if key is invalid
 */
export function validateKvKey(key: string): void {
  const bytes = new TextEncoder().encode(key).length;
  if (bytes > 512) {
    throw new Error(`KV key exceeds maximum size (512 bytes): got ${bytes}`);
  }

  // Path traversal defense
  if (key.includes('../') || key.includes('..\\')) {
    throw new Error(`Invalid KV key: contains path traversal sequence: ${key}`);
  }

  // Control character defense
  if (/[\x00-\x1F\x7F]/.test(key)) {
    throw new Error(`Invalid KV key: contains control characters`);
  }
}

/**
 * Validate KV value size.
 *
 * Cloudflare KV constraint: 25MB max value size
 *
 * Note: ReadableStream sizes cannot be validated without consuming the stream,
 * so we skip validation for streams (runtime will enforce limit).
 *
 * @throws {Error} if value exceeds size limit
 */
export function validateKvValue(value: string | ArrayBuffer | ReadableStream): void {
  let size: number;

  if (typeof value === 'string') {
    size = new TextEncoder().encode(value).length;
  } else if (value instanceof ArrayBuffer) {
    size = value.byteLength;
  } else {
    // ReadableStream - can't validate size without consuming
    return;
  }

  const MAX_SIZE = 25 * 1024 * 1024; // 25MB
  if (size > MAX_SIZE) {
    throw new Error(`KV value exceeds maximum size (25MB): got ${size}`);
  }
}
