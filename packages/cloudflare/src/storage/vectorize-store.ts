/**
 * Vectorize storage adapter implementing the portable IVectorStore interface.
 *
 * Wraps Cloudflare Vectorize's API into the IVectorStore interface
 * from @agentic-primer/actors. Supports both the beta VectorizeIndex
 * and the newer Vectorize (RC) class since they share the same method
 * signatures for upsert, query, and deleteByIds.
 *
 * Key design considerations:
 * - Vectorize mutations are async (RC returns mutationId, beta returns ids)
 * - Metadata values in Vectorize can be string | number | boolean | string[];
 *   our interface narrows to Record<string, string> for portability
 * - Filter maps Record<string, string> to VectorizeVectorMetadataFilter
 *   equality checks (each key-value pair becomes an exact-match filter)
 * - returnMetadata: 'all' is used to retrieve full metadata on query results
 */

import type { IVectorStore } from '@agentic-primer/actors';

/**
 * Cloudflare Vectorize adapter implementing IVectorStore.
 *
 * Usage:
 * ```typescript
 * const store = new VectorizeStore(env.MY_VECTORIZE_INDEX);
 * await store.index('doc-1', embedding, { source: 'manual' });
 * const results = await store.query(embedding, { limit: 5, minSimilarity: 0.8 });
 * ```
 *
 * Accepts both `VectorizeIndex` (beta) and `Vectorize` (RC) bindings since
 * they share compatible method signatures for the operations we use.
 */
export class VectorizeStore implements IVectorStore {
  constructor(
    private readonly vectorIndex: VectorizeIndex | Vectorize
  ) {}

  async index(
    id: string,
    vector: number[],
    metadata?: Record<string, string>
  ): Promise<void> {
    await this.vectorIndex.upsert([
      {
        id,
        values: vector,
        metadata: metadata as Record<string, VectorizeVectorMetadataValue> | undefined,
      },
    ]);
  }

  async query(
    vector: number[],
    options?: {
      limit?: number;
      minSimilarity?: number;
      filter?: Record<string, string>;
    }
  ): Promise<
    Array<{ id: string; similarity: number; metadata?: Record<string, string> }>
  > {
    const results = await this.vectorIndex.query(vector, {
      topK: options?.limit ?? 10,
      returnMetadata: 'all',
      filter: options?.filter as VectorizeVectorMetadataFilter | undefined,
    });

    return results.matches
      .filter(
        (m) =>
          !options?.minSimilarity || m.score >= options.minSimilarity
      )
      .map((m) => ({
        id: m.id,
        similarity: m.score,
        metadata: m.metadata
          ? toStringRecord(m.metadata as Record<string, VectorizeVectorMetadataValue>)
          : undefined,
      }));
  }

  async delete(id: string): Promise<void> {
    await this.vectorIndex.deleteByIds([id]);
  }
}

/**
 * Convert Vectorize metadata (which may contain numbers, booleans, string[])
 * to the portable Record<string, string> expected by IVectorStore.
 */
function toStringRecord(
  metadata: Record<string, VectorizeVectorMetadataValue>
): Record<string, string> {
  const result: Record<string, string> = {};
  for (const [key, value] of Object.entries(metadata)) {
    if (Array.isArray(value)) {
      result[key] = value.join(',');
    } else {
      result[key] = String(value);
    }
  }
  return result;
}
