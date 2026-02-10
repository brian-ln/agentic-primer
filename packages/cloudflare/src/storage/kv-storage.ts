/**
 * KV storage adapter wrapping Cloudflare Workers KV.
 *
 * Provides the portable IKeyValueStorage interface from @agentic-primer/actors
 * around KVNamespace.
 * Follows brianln.ai patterns:
 * - Relay: env.ROUTES.get<WebhookRoute>('webhook:postmark', 'json')
 * - Auth: env.DEVICE_CODES with TTL for ephemeral device codes
 */

import type { IKeyValueStorage } from '@agentic-primer/actors';

/**
 * Cloudflare KV adapter implementing IKeyValueStorage.
 *
 * Usage:
 * ```typescript
 * const kv = new KVStorage(env.ROUTES);
 * const route = await kv.get<WebhookRoute>('webhook:postmark', { type: 'json' });
 * await kv.put('config:limits', JSON.stringify(limits), { expirationTtl: 3600 });
 * ```
 */
export class KVStorage implements IKeyValueStorage {
  constructor(private readonly namespace: KVNamespace) {}

  async get<T = string>(
    key: string,
    options?: { type?: 'text' | 'json' | 'arrayBuffer' }
  ): Promise<T | null> {
    const type = options?.type ?? 'text';
    switch (type) {
      case 'json':
        return this.namespace.get(key, 'json') as Promise<T | null>;
      case 'arrayBuffer': {
        const buf = await this.namespace.get(key, 'arrayBuffer');
        return buf as T | null;
      }
      case 'text':
      default:
        return this.namespace.get(key, 'text') as Promise<T | null>;
    }
  }

  async put(
    key: string,
    value: string | ArrayBuffer | ReadableStream,
    options?: {
      expirationTtl?: number;
      metadata?: Record<string, unknown>;
    }
  ): Promise<void> {
    await this.namespace.put(key, value as string, {
      expirationTtl: options?.expirationTtl,
      metadata: options?.metadata,
    });
  }

  async delete(key: string): Promise<void> {
    await this.namespace.delete(key);
  }

  async list(options?: {
    prefix?: string;
    limit?: number;
    cursor?: string;
  }): Promise<{
    keys: Array<{ name: string; metadata?: unknown }>;
    cursor?: string;
  }> {
    const result = await this.namespace.list({
      prefix: options?.prefix,
      limit: options?.limit,
      cursor: options?.cursor,
    });
    return {
      keys: result.keys.map((k) => ({
        name: k.name,
        metadata: k.metadata,
      })),
      cursor: result.list_complete ? undefined : result.cursor,
    };
  }
}
