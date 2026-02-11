/**
 * Validated KV storage wrapper.
 *
 * Provides defense-in-depth validation for key-value operations.
 * Wraps any IKeyValueStorage implementation with input validation.
 *
 * Pattern: Decorator - maintains IKeyValueStorage interface while adding validation.
 *
 * Usage:
 * ```typescript
 * const rawStorage = new CloudflareKVStorage(env.KV);
 * const validatedStorage = new ValidatedKvStorage(rawStorage);
 * await validatedStorage.put('user:123', JSON.stringify(userData));
 * ```
 */

import type { IKeyValueStorage } from '../interfaces.ts';
import { validateKvKey, validateKvValue } from './storage-validation.ts';

/**
 * Validated KV storage wrapper.
 *
 * Validates all inputs before delegating to the inner storage implementation.
 * Provides an independent security layer that does not trust upstream validation.
 */
export class ValidatedKvStorage implements IKeyValueStorage {
  constructor(private inner: IKeyValueStorage) {}

  /**
   * Get a value from KV storage with key validation.
   *
   * Validates:
   * - Key format (size, path traversal, control characters)
   *
   * @throws {Error} if key validation fails
   */
  async get<T = string>(
    key: string,
    options?: { type?: 'text' | 'json' | 'arrayBuffer' }
  ): Promise<T | null> {
    validateKvKey(key);
    return this.inner.get<T>(key, options);
  }

  /**
   * Put a value into KV storage with validation.
   *
   * Validates:
   * - Key format (size, path traversal, control characters)
   * - Value size (max 25MB for Cloudflare KV)
   *
   * @throws {Error} if validation fails
   */
  async put(
    key: string,
    value: string | ArrayBuffer | ReadableStream,
    options?: {
      expirationTtl?: number;
      metadata?: Record<string, unknown>;
    }
  ): Promise<void> {
    validateKvKey(key);
    validateKvValue(value);
    return this.inner.put(key, value, options);
  }

  /**
   * Delete a key from KV storage with validation.
   *
   * Validates:
   * - Key format (size, path traversal, control characters)
   *
   * @throws {Error} if key validation fails
   */
  async delete(key: string): Promise<void> {
    validateKvKey(key);
    return this.inner.delete(key);
  }

  /**
   * List keys in KV storage with prefix validation.
   *
   * Validates:
   * - Prefix format (if provided)
   *
   * @throws {Error} if prefix validation fails
   */
  async list(options?: {
    prefix?: string;
    limit?: number;
    cursor?: string;
  }): Promise<{
    keys: Array<{ name: string; metadata?: unknown }>;
    cursor?: string;
  }> {
    if (options?.prefix) {
      validateKvKey(options.prefix);
    }
    return this.inner.list(options);
  }
}
