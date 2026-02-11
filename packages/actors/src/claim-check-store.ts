/**
 * Claim check storage operations.
 *
 * Stores large payloads in KV storage with TTL expiration.
 * Generates unique IDs and manages lifecycle.
 *
 * Pattern: External Store Pattern
 * - Store: serialize payload, generate UUID, store with TTL
 * - Retrieve: lookup by ID, deserialize, return payload
 * - Delete: optional cleanup (TTL handles expiration)
 *
 * TTL: 1 hour default (claim checks are short-lived)
 */

import type { IKeyValueStorage } from './interfaces.ts';
import type { ClaimCheckReference } from './claim-check.ts';

/**
 * Claim check storage operations
 *
 * Stores large payloads in KV storage with TTL expiration.
 * Generates unique IDs and manages lifecycle.
 */
export class ClaimCheckStore {
  constructor(private kv: IKeyValueStorage) {}

  /**
   * Store a payload externally and return reference
   *
   * @param payload - The payload to store
   * @returns ClaimCheckReference to include in message
   */
  async store(payload: unknown): Promise<ClaimCheckReference> {
    const id = `claim-check:${crypto.randomUUID()}`;
    const json = JSON.stringify(payload);

    await this.kv.put(id, json, {
      expirationTtl: 3600, // 1 hour TTL
      metadata: { type: 'claim-check', created: Date.now() }
    });

    return {
      claimCheckId: id,
      storageType: 'kv',
      contentType: 'json',
      size: json.length
    };
  }

  /**
   * Retrieve payload from storage
   *
   * @param ref - Claim check reference
   * @returns Original payload
   * @throws Error if claim check not found or expired
   */
  async retrieve(ref: ClaimCheckReference): Promise<unknown> {
    const json = await this.kv.get<string>(ref.claimCheckId, { type: 'text' });

    if (!json) {
      throw new Error(`Claim check not found: ${ref.claimCheckId} (expired or deleted)`);
    }

    return JSON.parse(json);
  }

  /**
   * Delete a claim check (optional cleanup)
   *
   * @param ref - Claim check reference
   */
  async delete(ref: ClaimCheckReference): Promise<void> {
    await this.kv.delete(ref.claimCheckId);
  }
}
