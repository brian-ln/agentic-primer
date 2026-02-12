#!/usr/bin/env bun
/**
 * Embedding Entity for Universal Graph System
 *
 * Provides embedding generation and similarity search capabilities.
 * Uses Cloudflare Workers AI for embeddings via the existing AI Gateway.
 * Model: @cf/baai/bge-base-en-v1.5 (768 dimensions)
 */

import GraphStore, { Node } from '../graph.ts';
import { getCurrentContext, ExecutionContext } from '../context.ts';

// Embedding event types
export type EmbeddingEventType =
  | 'NODE_EMBEDDED'
  | 'SIMILARITY_SEARCH';

// Embedding event structure
export interface EmbeddingEvent {
  id: string;
  timestamp: number;
  type: EmbeddingEventType;
  data: any;
}

// Similarity search result
export interface SimilarityResult {
  node: Node;
  similarity: number;
}

// Embedding options
export interface EmbedOptions {
  text?: string;  // Custom text to embed (defaults to node content)
}

// Similarity search options
export interface SimilaritySearchOptions {
  type?: string;      // Filter by node type
  limit?: number;     // Max results (default 10)
  minSimilarity?: number;  // Minimum similarity threshold (default 0)
}

// Cloudflare Workers AI response type
interface CloudflareEmbeddingResponse {
  result: {
    shape: number[];
    data: number[][];
  };
  success: boolean;
  errors: any[];
  messages: any[];
}

/**
 * EmbeddingManager - Manages embeddings and similarity search
 */
export class EmbeddingManager {
  private store: GraphStore;
  private embeddingEvents: EmbeddingEvent[] = [];
  private eventCounter = 0;

  // Cloudflare Workers AI model
  private readonly MODEL = '@cf/baai/bge-base-en-v1.5';
  private readonly EMBEDDING_DIMENSIONS = 768;

  constructor(store: GraphStore) {
    this.store = store;
  }

  /**
   * Get the underlying GraphStore
   */
  getStore(): GraphStore {
    return this.store;
  }

  /**
   * Generate unique event ID
   */
  private generateEventId(): string {
    return `emb_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit an embedding event
   */
  private async emitEvent(type: EmbeddingEventType, data: any): Promise<void> {
    const event: EmbeddingEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      data
    };

    this.embeddingEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'embedding_event', {
      eventType: type,
      ...data
    });
  }

  /**
   * Get text content from a node for embedding
   */
  private getNodeText(node: Node): string {
    const parts: string[] = [];

    // Include node ID
    parts.push(node.id);

    // Include type if present
    if (node.type) {
      parts.push(node.type);
    }

    // Include common text properties
    const textProperties = ['name', 'title', 'description', 'content', 'text', 'summary'];
    for (const prop of textProperties) {
      const value = node.properties.get(prop);
      if (value && typeof value === 'string') {
        parts.push(value);
      }
    }

    // Include other string properties
    for (const [key, value] of node.properties) {
      if (!textProperties.includes(key) && typeof value === 'string') {
        parts.push(value);
      }
    }

    // Include data if it's a string
    if (typeof node.data === 'string') {
      parts.push(node.data);
    } else if (node.data && typeof node.data === 'object') {
      // Try to extract text from data object
      try {
        const dataStr = JSON.stringify(node.data);
        if (dataStr.length < 10000) {  // Avoid huge data
          parts.push(dataStr);
        }
      } catch {
        // Ignore serialization errors
      }
    }

    return parts.join(' ').trim();
  }

  /**
   * Get embedding vector from Cloudflare Workers AI
   */
  async embed(text: string, ctx?: ExecutionContext): Promise<number[]> {
    const context = ctx || getCurrentContext();

    // Support both /ai config names (CF_*) and standard names (CLOUDFLARE_*)
    const accountId = context.getCredential('CLOUDFLARE_ACCOUNT_ID')
                   || context.getCredential('CF_ACCOUNT_ID');
    // Workers AI requires a Cloudflare API token with "Workers AI Read" permission
    // This is separate from the AI Gateway token used for unified billing
    const apiToken = context.getCredential('CLOUDFLARE_WORKERS_AI_TOKEN')
                  || context.getCredential('CLOUDFLARE_API_TOKEN')
                  || context.getCredential('CF_AIG_TOKEN');

    if (!accountId) {
      throw new Error('CLOUDFLARE_ACCOUNT_ID or CF_ACCOUNT_ID not found in context or environment');
    }

    if (!apiToken) {
      throw new Error('CLOUDFLARE_WORKERS_AI_TOKEN, CLOUDFLARE_API_TOKEN, or CF_AIG_TOKEN not found in context or environment');
    }

    // Cloudflare Workers AI direct API (not unified billing - requires separate API token)
    const url = `https://api.cloudflare.com/client/v4/accounts/${accountId}/ai/run/${this.MODEL}`;

    const response = await fetch(url, {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${apiToken}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        text: [text]  // API expects array of texts
      })
    });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`Cloudflare AI API error: ${response.status} - ${errorText}`);
    }

    const data = await response.json() as CloudflareEmbeddingResponse;

    if (!data.success) {
      throw new Error(`Cloudflare AI embedding failed: ${JSON.stringify(data.errors)}`);
    }

    if (!data.result?.data?.[0]) {
      throw new Error('No embedding returned from Cloudflare AI');
    }

    return data.result.data[0];
  }

  /**
   * Encode embedding as base64 for efficient storage
   */
  private encodeEmbedding(embedding: number[]): string {
    const float32 = new Float32Array(embedding);
    return Buffer.from(float32.buffer).toString('base64');
  }

  /**
   * Decode embedding from base64 storage format
   */
  private decodeEmbedding(base64: string, dimensions: number): number[] {
    const buffer = Buffer.from(base64, 'base64');
    const float32 = new Float32Array(buffer.buffer, buffer.byteOffset, dimensions);
    return Array.from(float32);
  }

  /**
   * Embed a node's content and store the embedding
   */
  async embedNode(nodeId: string, options: EmbedOptions = {}): Promise<number[]> {
    const node = this.store.get(nodeId);
    if (!node || !(node instanceof Node)) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    // Get text to embed
    const text = options.text || this.getNodeText(node);
    if (!text.trim()) {
      throw new Error(`No text content to embed for node: ${nodeId}`);
    }

    // Generate embedding
    const embedding = await this.embed(text);

    // Store embedding in node properties as base64 (5x smaller than JSON)
    await this.store.updateNode(nodeId, {
      embedding: this.encodeEmbedding(embedding),
      embeddingDimensions: embedding.length,
      embeddingModel: this.MODEL,
      embeddingTimestamp: Date.now()
    });

    // Emit event
    await this.emitEvent('NODE_EMBEDDED', {
      nodeId,
      textLength: text.length,
      model: this.MODEL,
      dimensions: embedding.length
    });

    return embedding;
  }

  /**
   * Get the stored embedding for a node
   */
  getNodeEmbedding(nodeId: string): number[] | null {
    const node = this.store.get(nodeId);
    if (!node || !(node instanceof Node)) {
      return null;
    }

    const embeddingStr = node.properties.get('embedding');
    if (!embeddingStr) {
      return null;
    }

    const dimensions = node.properties.get('embeddingDimensions');

    // If dimensions are stored, it's base64 format
    if (dimensions && typeof dimensions === 'number') {
      try {
        return this.decodeEmbedding(embeddingStr, dimensions);
      } catch {
        return null;
      }
    }

    // Legacy: try JSON parse for old format
    try {
      return JSON.parse(embeddingStr);
    } catch {
      return null;
    }
  }

  /**
   * Check if a node has an embedding
   */
  hasEmbedding(nodeId: string): boolean {
    return this.getNodeEmbedding(nodeId) !== null;
  }

  /**
   * Calculate cosine similarity between two vectors
   */
  cosineSimilarity(a: number[], b: number[]): number {
    if (a.length !== b.length) {
      throw new Error(`Vector length mismatch: ${a.length} vs ${b.length}`);
    }

    let dotProduct = 0;
    let normA = 0;
    let normB = 0;

    for (let i = 0; i < a.length; i++) {
      dotProduct += a[i] * b[i];
      normA += a[i] * a[i];
      normB += b[i] * b[i];
    }

    if (normA === 0 || normB === 0) {
      return 0;
    }

    return dotProduct / (Math.sqrt(normA) * Math.sqrt(normB));
  }

  /**
   * Find nodes similar to a query vector
   */
  findSimilar(queryVector: number[], options: SimilaritySearchOptions = {}): SimilarityResult[] {
    const { type, limit = 10, minSimilarity = 0 } = options;

    const results: SimilarityResult[] = [];

    // Iterate through all nodes
    for (const node of this.store.nodes.values()) {
      // Filter by type if specified
      if (type && node.type !== type) {
        continue;
      }

      // Get stored embedding
      const embedding = this.getNodeEmbedding(node.id);
      if (!embedding) {
        continue;
      }

      // Calculate similarity
      const similarity = this.cosineSimilarity(queryVector, embedding);

      // Filter by minimum similarity
      if (similarity < minSimilarity) {
        continue;
      }

      results.push({ node, similarity });
    }

    // Sort by similarity (descending) and limit
    results.sort((a, b) => b.similarity - a.similarity);

    const limitedResults = results.slice(0, limit);

    // Emit event
    this.emitEvent('SIMILARITY_SEARCH', {
      queryVectorLength: queryVector.length,
      type,
      limit,
      minSimilarity,
      resultsCount: limitedResults.length
    });

    return limitedResults;
  }

  /**
   * Find nodes similar to a given node
   */
  async findSimilarToNode(nodeId: string, options: SimilaritySearchOptions = {}): Promise<SimilarityResult[]> {
    const embedding = this.getNodeEmbedding(nodeId);
    if (!embedding) {
      // Try to generate embedding first
      await this.embedNode(nodeId);
      const newEmbedding = this.getNodeEmbedding(nodeId);
      if (!newEmbedding) {
        throw new Error(`Could not get or generate embedding for node: ${nodeId}`);
      }
      return this.findSimilarExcluding(newEmbedding, nodeId, options);
    }

    return this.findSimilarExcluding(embedding, nodeId, options);
  }

  /**
   * Find similar nodes excluding a specific node
   */
  private findSimilarExcluding(queryVector: number[], excludeNodeId: string, options: SimilaritySearchOptions = {}): SimilarityResult[] {
    const { type, limit = 10, minSimilarity = 0 } = options;

    const results: SimilarityResult[] = [];

    for (const node of this.store.nodes.values()) {
      // Skip the excluded node
      if (node.id === excludeNodeId) {
        continue;
      }

      // Filter by type if specified
      if (type && node.type !== type) {
        continue;
      }

      // Get stored embedding
      const embedding = this.getNodeEmbedding(node.id);
      if (!embedding) {
        continue;
      }

      // Calculate similarity
      const similarity = this.cosineSimilarity(queryVector, embedding);

      // Filter by minimum similarity
      if (similarity < minSimilarity) {
        continue;
      }

      results.push({ node, similarity });
    }

    // Sort by similarity (descending) and limit
    results.sort((a, b) => b.similarity - a.similarity);

    return results.slice(0, limit);
  }

  /**
   * Find nodes similar to query text
   */
  async findSimilarToText(text: string, options: SimilaritySearchOptions = {}): Promise<SimilarityResult[]> {
    const queryVector = await this.embed(text);
    return this.findSimilar(queryVector, options);
  }

  /**
   * Get all embedding events
   */
  getEmbeddingEvents(limit?: number): EmbeddingEvent[] {
    const events = this.embeddingEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get embedding statistics
   */
  getStats(): { totalNodes: number; embeddedNodes: number; model: string; dimensions: number } {
    let embeddedNodes = 0;
    for (const node of this.store.nodes.values()) {
      if (this.hasEmbedding(node.id)) {
        embeddedNodes++;
      }
    }

    return {
      totalNodes: this.store.nodes.size,
      embeddedNodes,
      model: this.MODEL,
      dimensions: this.EMBEDDING_DIMENSIONS
    };
  }

  /**
   * Batch embed multiple nodes
   * Returns a map of successful embeddings; failed nodes are silently skipped
   */
  async embedNodes(nodeIds: string[]): Promise<Map<string, number[]>> {
    const results = new Map<string, number[]>();

    for (const nodeId of nodeIds) {
      try {
        const embedding = await this.embedNode(nodeId);
        results.set(nodeId, embedding);
      } catch {
        // Skip nodes that fail to embed - caller can compare input vs output size
        // to detect failures if needed
      }
    }

    return results;
  }
}

export default EmbeddingManager;
