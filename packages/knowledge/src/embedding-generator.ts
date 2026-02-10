/**
 * EmbeddingGenerator - Multi-provider embedding generation
 *
 * Supports OpenAI, Cloudflare Workers AI, and local LM Studio.
 * Uses fetch() (web platform baseline).
 */

export interface EmbeddingConfig {
  apiKey?: string;
  baseUrl?: string;
  model?: string;
  dimensions?: number;
}

export class EmbeddingGenerator {
  private apiKey: string;
  private baseUrl: string;
  private model: string;
  private dimensions: number;

  constructor(options?: EmbeddingConfig) {
    this.baseUrl = options?.baseUrl || 'http://localhost:1234/v1';
    this.model = options?.model || 'text-embedding-nomic-embed-text-v1.5';

    this.dimensions = options?.dimensions ||
      (this.model.includes('nomic') || this.model.includes('bge') ? 768 : 1536);

    this.apiKey = options?.apiKey || '';
  }

  async embed(text: string): Promise<Float32Array> {
    const embeddings = await this.embedBatch([text]);
    return embeddings[0];
  }

  async embedBatch(texts: string[], maxBatchSize: number = 25): Promise<Float32Array[]> {
    const results: Float32Array[] = [];

    for (let i = 0; i < texts.length; i += maxBatchSize) {
      const batch = texts.slice(i, i + maxBatchSize);
      const batchResults = await this.embedBatchInternal(batch);
      results.push(...batchResults);
    }

    return results;
  }

  private async embedBatchInternal(texts: string[]): Promise<Float32Array[]> {
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
    };

    if (this.apiKey) {
      headers['Authorization'] = `Bearer ${this.apiKey}`;
    }

    // Cloudflare Workers AI
    if (this.model.startsWith('@cf/')) {
      const results: Float32Array[] = [];
      for (const text of texts) {
        const response = await fetch(`${this.baseUrl}/${this.model}`, {
          method: 'POST',
          headers,
          body: JSON.stringify({ text }),
        });

        if (!response.ok) {
          const error = await response.text();
          throw new Error(`Cloudflare embedding API error: ${response.status} ${error}`);
        }

        const data = await response.json() as any;
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
        ...(this.baseUrl.includes('openai.com') ? { dimensions: this.dimensions } : {}),
      }),
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Embedding API error: ${response.status} ${error}`);
    }

    const data = await response.json() as any;
    return data.data.map((item: any) => new Float32Array(item.embedding));
  }

  static prepareText(text: string, maxTokens: number = 8000): string {
    const maxChars = maxTokens * 4;
    if (text.length <= maxChars) return text;
    return text.slice(0, maxChars) + '...';
  }

  static extractEmbeddableContent(message: any): string | null {
    if (message.type === 'user' && message.message?.content) {
      if (typeof message.message.content === 'string') {
        return message.message.content;
      }
      const textBlocks = message.message.content
        .filter((b: any) => b.type === 'text')
        .map((b: any) => b.text);
      return textBlocks.join('\n');
    }

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
