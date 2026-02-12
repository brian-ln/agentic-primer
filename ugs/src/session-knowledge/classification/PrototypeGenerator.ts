#!/usr/bin/env bun
/**
 * PrototypeGenerator - Generate category prototype embeddings
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.4
 */

import { createClient, type Client } from '@libsql/client';
import { EmbeddingGenerator } from '../embeddings/EmbeddingGenerator';
import { join } from 'path';

const INDEX_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(INDEX_DIR, 'sessions-libsql.db');

/**
 * Curated examples for each category
 * These represent canonical instances to create prototype vectors
 */
const DECISION_EXAMPLES = [
  "We decided to use JWT tokens for authentication instead of session cookies",
  "Chose libSQL over sqlite-vec because it has native vector support and cleaner implementation",
  "Going with React for the frontend framework rather than Vue or Angular",
  "Opted for TypeScript instead of plain JavaScript for better type safety",
  "Selected PostgreSQL as the primary database instead of MySQL",
  "Will use REST API instead of GraphQL for simpler implementation",
  "Decided to deploy on AWS Lambda rather than traditional EC2 instances",
  "Choosing to implement SSR with Next.js instead of CSR",
  "Using Redis for caching instead of in-memory cache",
  "Picked Tailwind CSS over styled-components for styling"
];

const LEARNING_EXAMPLES = [
  "Discovered that libSQL vectors use cosine distance by default, not Euclidean",
  "Learned that embedding batch size impacts memory usage significantly",
  "Key insight: DiskANN index provides 10x faster similarity search than brute force",
  "Found that message embeddings should exclude tool calls to improve relevance",
  "Realized that prototype vectors need at least 5-10 examples for good accuracy",
  "Learned that local LM Studio can handle 768-dim embeddings efficiently",
  "Key finding: Confidence scores above 0.8 indicate reliable classifications",
  "Discovered that semantic search works better with normalized text",
  "Insight: Two-stage classification reduces costs by 90% while maintaining accuracy",
  "Found that temperature=0.1 gives more consistent LLM outputs than 0.7"
];

const ERROR_EXAMPLES = [
  "Error: Connection timeout when calling embedding API",
  "Failed to load model: CUDA out of memory",
  "TypeError: Cannot read property 'embedding' of undefined",
  "Database error: UNIQUE constraint failed on message_id",
  "API rate limit exceeded: 429 Too Many Requests",
  "Failed to parse JSON response from LLM",
  "Vector dimension mismatch: expected 768, got 1536",
  "File not found: session transcript does not exist",
  "Permission denied: cannot write to database file",
  "Network error: fetch failed to reach LM Studio endpoint"
];

const WORKFLOW_EXAMPLES = [
  "Used /bg to delegate research while continuing with implementation in parallel",
  "Breaking the task into smaller subtasks made it easier to track progress",
  "Spawning multiple background agents in parallel sped up the work significantly",
  "Planning the architecture before coding saved time and prevented rework",
  "Using /reflect at end of session documented what was accomplished effectively",
  "Delegating file analysis to background agent while focusing on planning was efficient",
  "Sequential approach worked better than parallel for dependent tasks",
  "Creating skills for repeated workflows eliminated manual repetition",
  "Using Task tool to track progress kept the work organized",
  "Consolidating scattered files into unified structure improved discoverability"
];

export interface PrototypeInfo {
  category: string;
  exampleCount: number;
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Generate and store prototype embeddings for classification
 */
export class PrototypeGenerator {
  private client: Client;
  private embedder: EmbeddingGenerator;

  constructor(dbPath: string = DB_PATH) {
    this.client = createClient({ url: `file:${dbPath}` });
    this.embedder = new EmbeddingGenerator();
  }

  /**
   * Generate all category prototypes
   */
  async generateAll(): Promise<void> {
    console.log('\n🧬 Generating prototype embeddings...\n');

    await this.generatePrototype('decision', DECISION_EXAMPLES);
    await this.generatePrototype('learning', LEARNING_EXAMPLES);
    await this.generatePrototype('error', ERROR_EXAMPLES);
    await this.generatePrototype('workflow', WORKFLOW_EXAMPLES);

    console.log('\n✓ All prototypes generated\n');
  }

  /**
   * Generate prototype for a single category
   */
  async generatePrototype(category: string, examples: string[]): Promise<void> {
    console.log(`  Generating ${category} prototype (${examples.length} examples)...`);

    // Generate embeddings for all examples
    const embeddings = await this.embedder.embedBatch(examples);

    // Calculate average (centroid) vector
    const dimensions = embeddings[0].length;
    const prototypeVector = new Float32Array(dimensions);

    for (let i = 0; i < dimensions; i++) {
      let sum = 0;
      for (const embedding of embeddings) {
        sum += embedding[i];
      }
      prototypeVector[i] = sum / embeddings.length;
    }

    // Normalize the prototype vector (unit length)
    const magnitude = Math.sqrt(
      Array.from(prototypeVector).reduce((sum, val) => sum + val * val, 0)
    );
    for (let i = 0; i < dimensions; i++) {
      prototypeVector[i] /= magnitude;
    }

    // Store in database
    const vectorJson = `[${Array.from(prototypeVector).join(',')}]`;
    const now = Date.now();

    await this.client.execute({
      sql: `INSERT INTO prototype_embeddings (category, embedding, example_count, created_at, updated_at)
            VALUES (?, vector(?), ?, ?, ?)
            ON CONFLICT(category) DO UPDATE SET
              embedding = excluded.embedding,
              example_count = excluded.example_count,
              updated_at = excluded.updated_at`,
      args: [category, vectorJson, examples.length, now, now]
    });

    console.log(`    ✓ ${category} prototype stored (${dimensions} dimensions)`);
  }

  /**
   * Get prototype embedding for a category
   */
  async getPrototype(category: string): Promise<Float32Array | null> {
    const result = await this.client.execute({
      sql: 'SELECT embedding FROM prototype_embeddings WHERE category = ?',
      args: [category]
    });

    if (!result.rows[0]?.embedding) {
      return null;
    }

    // libSQL returns F32_BLOB as Float32Array directly
    return result.rows[0].embedding as Float32Array;
  }

  /**
   * List all prototypes
   */
  async listPrototypes(): Promise<PrototypeInfo[]> {
    const result = await this.client.execute(
      'SELECT category, example_count, created_at, updated_at FROM prototype_embeddings ORDER BY category'
    );

    return result.rows.map((row: any) => ({
      category: row.category as string,
      exampleCount: row.example_count as number,
      createdAt: new Date(row.created_at as number),
      updatedAt: new Date(row.updated_at as number)
    }));
  }

  /**
   * Delete a prototype
   */
  async deletePrototype(category: string): Promise<void> {
    await this.client.execute({
      sql: 'DELETE FROM prototype_embeddings WHERE category = ?',
      args: [category]
    });
  }

  /**
   * Delete all prototypes
   */
  async deleteAll(): Promise<void> {
    await this.client.execute('DELETE FROM prototype_embeddings');
  }

  async close(): Promise<void> {
    this.client.close();
  }
}

// CLI entry point
if (import.meta.main) {
  const generator = new PrototypeGenerator();

  try {
    const command = process.argv[2];

    switch (command) {
      case 'list': {
        const prototypes = await generator.listPrototypes();
        console.log('\n📋 Prototype Embeddings\n');
        prototypes.forEach(p => {
          console.log(`  ${p.category.padEnd(10)} ${p.exampleCount} examples, updated ${p.updatedAt.toISOString()}`);
        });
        console.log();
        break;
      }

      case 'delete': {
        const category = process.argv[3];
        if (!category) {
          console.error('Usage: generate-prototypes delete <category>');
          process.exit(1);
        }
        await generator.deletePrototype(category);
        console.log(`✓ Deleted ${category} prototype`);
        break;
      }

      case 'clear': {
        await generator.deleteAll();
        console.log('✓ Deleted all prototypes');
        break;
      }

      default: {
        // Generate all prototypes
        await generator.generateAll();
        const prototypes = await generator.listPrototypes();
        console.log('Stored prototypes:');
        prototypes.forEach(p => {
          console.log(`  ✓ ${p.category}: ${p.exampleCount} examples`);
        });
      }
    }
  } catch (error) {
    console.error('Error:', error instanceof Error ? error.message : error);
    process.exit(1);
  } finally {
    await generator.close();
  }
}
