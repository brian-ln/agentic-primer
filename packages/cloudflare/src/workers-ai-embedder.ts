/**
 * Workers AI embedder implementing the portable IEmbedder interface.
 *
 * Wraps Cloudflare Workers AI embedding models into the IEmbedder interface
 * from @agentic-primer/actors. Default model is @cf/baai/bge-base-en-v1.5
 * (768 dimensions).
 *
 * Key design considerations:
 * - Workers AI embedding models accept { text: string | string[] }
 * - Output shape is { data: number[][] } with one embedding per input text
 * - The model name must be a key in AiModels that accepts text-embedding input
 * - Dimension counts are model-specific and must match the Vectorize index config
 */

import type { IEmbedder } from '@agentic-primer/actors';

/** Known embedding models and their dimension counts. */
const MODEL_DIMENSIONS: Record<string, number> = {
  '@cf/baai/bge-base-en-v1.5': 768,
  '@cf/baai/bge-small-en-v1.5': 384,
  '@cf/baai/bge-large-en-v1.5': 1024,
  '@cf/baai/bge-m3': 1024,
};

const DEFAULT_MODEL = '@cf/baai/bge-base-en-v1.5';

export interface WorkersAIEmbedderConfig {
  /** Embedding model name. Default: '@cf/baai/bge-base-en-v1.5' */
  model?: string;
  /** Override dimension count (auto-detected for known models). */
  dimensions?: number;
}

/**
 * Cloudflare Workers AI adapter implementing IEmbedder.
 *
 * Usage:
 * ```typescript
 * const embedder = new WorkersAIEmbedder(env.AI);
 * const vector = await embedder.embed('hello world');
 * const vectors = await embedder.embedBatch(['hello', 'world']);
 * ```
 */
export class WorkersAIEmbedder implements IEmbedder {
  readonly dimensions: number;
  private readonly model: string;

  constructor(
    private readonly ai: Ai,
    config?: WorkersAIEmbedderConfig
  ) {
    this.model = config?.model ?? DEFAULT_MODEL;
    this.dimensions =
      config?.dimensions ?? MODEL_DIMENSIONS[this.model] ?? 768;
  }

  async embed(text: string): Promise<number[]> {
    const result = await this.ai.run(
      this.model as '@cf/baai/bge-base-en-v1.5',
      { text: [text] }
    );
    // result is the union type; the non-streaming branch has { data?: number[][] }
    const output = result as { data?: number[][] };
    if (!output.data || output.data.length === 0) {
      throw new Error(
        `Workers AI embedding returned no data for model ${this.model}`
      );
    }
    return output.data[0];
  }

  async embedBatch(texts: string[]): Promise<number[][]> {
    if (texts.length === 0) {
      return [];
    }
    const result = await this.ai.run(
      this.model as '@cf/baai/bge-base-en-v1.5',
      { text: texts }
    );
    const output = result as { data?: number[][] };
    if (!output.data || output.data.length !== texts.length) {
      throw new Error(
        `Workers AI embedding returned unexpected data shape for model ${this.model}: ` +
          `expected ${texts.length} embeddings, got ${output.data?.length ?? 0}`
      );
    }
    return output.data;
  }
}
