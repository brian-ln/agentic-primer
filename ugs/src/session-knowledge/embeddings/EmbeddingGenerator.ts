#!/usr/bin/env bun
/**
 * EmbeddingGenerator - OpenAI embeddings for semantic search
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

/**
 * Generate embeddings using OpenAI API or local LM Studio
 *
 * OpenAI: $0.00002 per 1K tokens (text-embedding-3-small)
 * LM Studio: Free, local inference
 *
 * For 200 messages @ 100 tokens avg = 20K tokens = $0.0004/session (OpenAI)
 */
export class EmbeddingGenerator {
  private apiKey: string;
  private baseUrl: string;
  private model: string;
  private dimensions: number = 1536;

  constructor(options?: {
    apiKey?: string;
    baseUrl?: string;
    model?: string;
    dimensions?: number;
  }) {
    // Default to LM Studio if running, otherwise OpenAI
    this.baseUrl = options?.baseUrl || process.env.EMBEDDING_BASE_URL || 'http://localhost:1234/v1';
    this.model = options?.model || process.env.EMBEDDING_MODEL || 'text-embedding-nomic-embed-text-v1.5';

    // Default dimensions: 768 for Nomic/BGE, 1536 for OpenAI
    this.dimensions = options?.dimensions ||
      (this.model.includes('nomic') || this.model.includes('bge') ? 768 : 1536);

    // API key required for OpenAI and Cloudflare
    if (this.baseUrl.includes('openai.com')) {
      this.apiKey = options?.apiKey || process.env.OPENAI_API_KEY || '';
      if (!this.apiKey) {
        throw new Error('OPENAI_API_KEY environment variable required for OpenAI');
      }
    } else if (this.baseUrl.includes('cloudflare.com') || this.model.startsWith('@cf/')) {
      this.apiKey = options?.apiKey || process.env.CLOUDFLARE_API_TOKEN || '';
      if (!this.apiKey) {
        throw new Error('CLOUDFLARE_API_TOKEN environment variable required for Cloudflare');
      }
      // Build Cloudflare Workers AI URL
      const accountId = process.env.CLOUDFLARE_ACCOUNT_ID;
      if (!accountId) {
        throw new Error('CLOUDFLARE_ACCOUNT_ID environment variable required for Cloudflare');
      }
      this.baseUrl = `https://api.cloudflare.com/client/v4/accounts/${accountId}/ai/run`;
    } else {
      this.apiKey = ''; // Not needed for local LM Studio
    }
  }

  /**
   * Generate embedding for a single text
   */
  async embed(text: string): Promise<Float32Array> {
    const embeddings = await this.embedBatch([text]);
    return embeddings[0];
  }

  /**
   * Generate embeddings for multiple texts (batch)
   * OpenAI allows up to 2048 inputs per request
   */
  async embedBatch(texts: string[], maxBatchSize: number = 25): Promise<Float32Array[]> {
    const results: Float32Array[] = [];

    // Process in batches
    for (let i = 0; i < texts.length; i += maxBatchSize) {
      const batch = texts.slice(i, i + maxBatchSize);
      const batchResults = await this.embedBatchInternal(batch);
      results.push(...batchResults);
    }

    return results;
  }

  private async embedBatchInternal(texts: string[]): Promise<Float32Array[]> {
    const headers: Record<string, string> = {
      'Content-Type': 'application/json'
    };

    // Add authorization header if API key is present
    if (this.apiKey) {
      headers['Authorization'] = `Bearer ${this.apiKey}`;
    }

    // Cloudflare Workers AI has different API format
    if (this.model.startsWith('@cf/')) {
      const results: Float32Array[] = [];

      // Cloudflare doesn't support batch, process one at a time
      for (const text of texts) {
        const response = await fetch(`${this.baseUrl}/${this.model}`, {
          method: 'POST',
          headers,
          body: JSON.stringify({ text })
        });

        if (!response.ok) {
          const error = await response.text();
          throw new Error(`Cloudflare embedding API error: ${response.status} ${error}`);
        }

        const data = await response.json();
        results.push(new Float32Array(data.result.data[0]));
      }

      return results;
    }

    // OpenAI / LM Studio format
    const response = await fetch(`${this.baseUrl}/embeddings`, {
      method: 'POST',
      headers,
      body: JSON.stringify({
        model: this.model,
        input: texts,
        // Only send dimensions for OpenAI (LM Studio ignores it)
        ...(this.baseUrl.includes('openai.com') ? { dimensions: this.dimensions } : {})
      })
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Embedding API error: ${response.status} ${error}`);
    }

    const data = await response.json();

    return data.data.map((item: any) =>
      new Float32Array(item.embedding)
    );
  }

  /**
   * Prepare text for embedding (truncate, clean)
   */
  static prepareText(text: string, maxTokens: number = 8000): string {
    // Rough approximation: 1 token ≈ 4 characters
    const maxChars = maxTokens * 4;

    if (text.length <= maxChars) {
      return text;
    }

    // Truncate and add ellipsis
    return text.slice(0, maxChars) + '...';
  }

  /**
   * Extract embeddable content from a session message
   */
  static extractEmbeddableContent(message: any): string | null {
    // User messages - just the text
    if (message.type === 'user' && message.message?.content) {
      if (typeof message.message.content === 'string') {
        return message.message.content;
      }
      // Array of content blocks
      const textBlocks = message.message.content
        .filter((b: any) => b.type === 'text')
        .map((b: any) => b.text);
      return textBlocks.join('\n');
    }

    // Assistant messages - text responses only (not tool calls)
    if (message.type === 'assistant' && message.message?.content) {
      const textBlocks = message.message.content
        .filter((b: any) => b.type === 'text')
        .map((b: any) => b.text);

      if (textBlocks.length > 0) {
        return textBlocks.join('\n');
      }
    }

    return null;
  }
}
