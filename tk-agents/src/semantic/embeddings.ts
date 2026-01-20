// OpenAI Embeddings Integration for Semantic Prompt Analysis
// Provides embedding generation with caching to avoid recomputation

import OpenAI from "openai";

// Initialize OpenAI client
// API key must be provided via OPENAI_API_KEY environment variable
const openai = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY || "",
});

// In-memory cache for embeddings (text hash -> embedding)
// TODO: Consider persistent caching for production
const embeddingCache = new Map<string, number[]>();

/**
 * Simple hash function for cache keys
 * Uses FNV-1a algorithm for fast hashing
 */
function hashString(text: string): string {
  let hash = 2166136261;
  for (let i = 0; i < text.length; i++) {
    hash ^= text.charCodeAt(i);
    hash = Math.imul(hash, 16777619);
  }
  return (hash >>> 0).toString(36);
}

/**
 * Generate embedding for a single text using OpenAI API
 * Model: text-embedding-3-small (1536 dimensions, $0.02/1M tokens)
 *
 * @param text - The text to embed
 * @returns Promise<number[]> - 1536-dimensional embedding vector
 * @throws Error if API call fails
 */
export async function generateEmbedding(text: string): Promise<number[]> {
  if (!text || text.trim().length === 0) {
    throw new Error("Cannot generate embedding for empty text");
  }

  if (!process.env.OPENAI_API_KEY) {
    throw new Error("OPENAI_API_KEY environment variable not set");
  }

  try {
    const response = await openai.embeddings.create({
      model: "text-embedding-3-small",
      input: text,
      dimensions: 1536,
    });

    return response.data[0].embedding;
  } catch (error) {
    throw new Error(`OpenAI embedding generation failed: ${error instanceof Error ? error.message : String(error)}`);
  }
}

/**
 * Get embedding with caching
 * Checks cache first, generates if not found
 *
 * @param text - The text to embed
 * @returns Promise<number[]> - 1536-dimensional embedding vector
 */
export async function getEmbedding(text: string): Promise<number[]> {
  const hash = hashString(text);

  // Check cache
  if (embeddingCache.has(hash)) {
    return embeddingCache.get(hash)!;
  }

  // Generate and cache
  const embedding = await generateEmbedding(text);
  embeddingCache.set(hash, embedding);

  return embedding;
}

/**
 * Batch embedding generation
 * More efficient than multiple single calls
 *
 * @param texts - Array of texts to embed
 * @returns Promise<number[][]> - Array of embedding vectors
 */
export async function getEmbeddings(texts: string[]): Promise<number[][]> {
  if (texts.length === 0) {
    return [];
  }

  if (!process.env.OPENAI_API_KEY) {
    throw new Error("OPENAI_API_KEY environment variable not set");
  }

  // Filter out cached embeddings
  const uncachedTexts: string[] = [];
  const uncachedIndices: number[] = [];
  const results: number[][] = new Array(texts.length);

  for (let i = 0; i < texts.length; i++) {
    const hash = hashString(texts[i]);
    if (embeddingCache.has(hash)) {
      results[i] = embeddingCache.get(hash)!;
    } else {
      uncachedTexts.push(texts[i]);
      uncachedIndices.push(i);
    }
  }

  // Generate embeddings for uncached texts
  if (uncachedTexts.length > 0) {
    try {
      const response = await openai.embeddings.create({
        model: "text-embedding-3-small",
        input: uncachedTexts,
        dimensions: 1536,
      });

      // Cache and store results
      for (let i = 0; i < uncachedTexts.length; i++) {
        const embedding = response.data[i].embedding;
        const hash = hashString(uncachedTexts[i]);
        embeddingCache.set(hash, embedding);
        results[uncachedIndices[i]] = embedding;
      }
    } catch (error) {
      throw new Error(`OpenAI batch embedding generation failed: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  return results;
}

/**
 * Clear the embedding cache
 * Useful for testing or memory management
 */
export function clearEmbeddingCache(): void {
  embeddingCache.clear();
}

/**
 * Get cache statistics
 * @returns { size: number, memoryEstimate: number } - Cache size and memory estimate in bytes
 */
export function getCacheStats(): { size: number; memoryEstimate: number } {
  const size = embeddingCache.size;
  // Each embedding: 1536 dimensions * 8 bytes (float64) + overhead
  const memoryEstimate = size * (1536 * 8 + 100);
  return { size, memoryEstimate };
}
