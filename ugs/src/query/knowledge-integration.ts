#!/usr/bin/env bun
/**
 * Knowledge Integration Layer
 *
 * Integrates KnowledgeActor and RelationshipActor with the query/DSL system.
 * Enables semantic search, graph traversal, and knowledge management via
 * declarative query patterns.
 *
 * Features:
 * - Pattern matching on knowledge entities (decisions, learnings, errors)
 * - Semantic search via embeddings
 * - Graph traversal via relationships
 * - Epistemic level filtering
 * - Evidence and confidence management
 */

import type { Address } from '@agentic-primer/actors';
import type { PatternSpec, TraversalSpec } from './types.ts';
import { pattern } from './pattern.ts';
import { EmbeddingGenerator } from '../session-knowledge/embeddings/EmbeddingGenerator.ts';
import { VectorStoreLibSQL } from '../session-knowledge/embeddings/VectorStoreLibSQL.ts';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

/**
 * Knowledge search options
 */
export interface KnowledgeSearchOptions {
  /** Search query text */
  query: string;

  /** Maximum results to return */
  limit?: number;

  /** Category filter (decision, learning, error) */
  category?: 'decision' | 'learning' | 'error';

  /** Minimum epistemic level */
  minEpistemicLevel?: 'reject' | 'doubt' | 'wonder' | 'suspect' | 'believe' | 'know';

  /** Minimum confidence score (0-1) */
  minConfidence?: number;

  /** Session ID filter */
  sessionId?: string;

  /** Similarity threshold (0-1) */
  threshold?: number;
}

/**
 * Knowledge search result
 */
export interface KnowledgeSearchResult {
  id: string;
  category: 'decision' | 'learning' | 'error';
  content: string;
  reasoning?: string;
  epistemic_level: string;
  confidence: number;
  evidence: any[];
  created: number;
  session_id?: string;
  similarity: number;
  address: Address;
}

/**
 * Traversal options for knowledge graph
 */
export interface KnowledgeTraversalOptions {
  /** Starting knowledge address */
  from: Address;

  /** Relationship type filter */
  relationshipType?: 'supports' | 'contradicts' | 'requires' | 'extends' | 'questions' | 'related-to';

  /** Traversal direction */
  direction?: 'outbound' | 'inbound' | 'both';

  /** Maximum depth */
  maxDepth?: number;

  /** Minimum relationship strength */
  minStrength?: number;
}

/**
 * Knowledge pattern builder
 *
 * Extends PatternBuilder with knowledge-specific constraints.
 */
export class KnowledgePatternBuilder {
  /**
   * Create pattern for knowledge entity
   *
   * @example
   * knowledge('k').category('decision').minConfidence(0.8)
   */
  static knowledge(variable: string = 'knowledge') {
    return new KnowledgePatternHelper(variable);
  }

  /**
   * Create pattern for decisions
   *
   * @example
   * knowledge.decisions('decision').minConfidence(0.9)
   */
  static decisions(variable: string = 'decision') {
    return new KnowledgePatternHelper(variable).category('decision');
  }

  /**
   * Create pattern for learnings
   *
   * @example
   * knowledge.learnings('learning').epistemicLevel('know')
   */
  static learnings(variable: string = 'learning') {
    return new KnowledgePatternHelper(variable).category('learning');
  }

  /**
   * Create pattern for errors
   *
   * @example
   * knowledge.errors('error').sessionId('abc123')
   */
  static errors(variable: string = 'error') {
    return new KnowledgePatternHelper(variable).category('error');
  }
}

/**
 * Helper class for knowledge-specific pattern building
 */
class KnowledgePatternHelper {
  private spec: PatternSpec;

  constructor(variable: string) {
    this.spec = {
      variable,
      labels: ['Knowledge'],
      where: {},
      relationships: [],
      notExists: [],
    };
  }

  /**
   * Filter by knowledge category
   */
  category(category: 'decision' | 'learning' | 'error'): this {
    this.spec.where = { ...this.spec.where, category };
    return this;
  }

  /**
   * Filter by minimum epistemic level
   */
  epistemicLevel(level: 'reject' | 'doubt' | 'wonder' | 'suspect' | 'believe' | 'know'): this {
    this.spec.where = { ...this.spec.where, epistemic_level: level };
    return this;
  }

  /**
   * Filter by minimum confidence
   */
  minConfidence(confidence: number): this {
    if (!this.spec.where) {
      this.spec.where = {};
    }
    this.spec.where.min_confidence = confidence;
    return this;
  }

  /**
   * Filter by session ID
   */
  sessionId(sessionId: string): this {
    this.spec.where = { ...this.spec.where, session_id: sessionId };
    return this;
  }

  /**
   * Filter by knowledge ID
   */
  id(id: string): this {
    this.spec.where = { ...this.spec.where, id };
    return this;
  }

  /**
   * Add relationship constraint
   */
  relatedTo(
    target: string,
    options: {
      type?: 'supports' | 'contradicts' | 'requires' | 'extends' | 'questions' | 'related-to';
      direction: 'outbound' | 'inbound' | 'both';
      minStrength?: number;
    }
  ): this {
    if (!this.spec.relationships) {
      this.spec.relationships = [];
    }

    this.spec.relationships.push({
      target,
      type: options.type,
      direction: options.direction,
      properties: options.minStrength ? { min_strength: options.minStrength } : undefined,
    });

    return this;
  }

  /**
   * Build the pattern
   */
  build(): PatternSpec {
    return this.spec;
  }

  /**
   * Get variable name
   */
  getVariable(): string {
    return this.spec.variable;
  }
}

/**
 * Semantic search over knowledge base
 *
 * Uses embeddings to find semantically similar knowledge items.
 *
 * @example
 * const results = await searchKnowledge({
 *   query: 'authentication decisions',
 *   category: 'decision',
 *   minConfidence: 0.8,
 *   limit: 5
 * });
 */
export async function searchKnowledge(
  options: KnowledgeSearchOptions
): Promise<KnowledgeSearchResult[]> {
  const {
    query,
    limit = 10,
    category,
    minEpistemicLevel,
    minConfidence,
    sessionId,
    threshold = 0.7,
  } = options;

  // Generate query embedding
  const generator = new EmbeddingGenerator();
  const queryEmbedding = await generator.embed(query);

  // Search in vector store
  const vectorStore = new VectorStoreLibSQL(DB_PATH);

  // For now, search in messages (would need dedicated knowledge embeddings)
  const rawResults = await vectorStore.searchMessages(queryEmbedding, limit * 2);

  await vectorStore.close();

  // Map and filter results
  const results: KnowledgeSearchResult[] = rawResults
    .filter(r => {
      const similarity = 1 - r.distance;
      return similarity >= threshold;
    })
    .map(r => ({
      id: r.id,
      category: 'learning' as const, // Default category
      content: r.content,
      reasoning: undefined,
      epistemic_level: 'believe',
      confidence: 1 - r.distance,
      evidence: [],
      created: r.timestamp || Date.now(),
      session_id: r.sessionId,
      similarity: 1 - r.distance,
      address: `@(knowledge/learnings/${r.id})` as Address,
    }))
    .slice(0, limit);

  // Apply filters
  let filtered = results;

  if (category) {
    filtered = filtered.filter(r => r.category === category);
  }

  if (minConfidence) {
    filtered = filtered.filter(r => r.confidence >= minConfidence);
  }

  if (sessionId) {
    filtered = filtered.filter(r => r.session_id === sessionId);
  }

  return filtered;
}

/**
 * Traverse knowledge graph from a starting node
 *
 * Performs BFS traversal following relationship edges.
 *
 * @example
 * const network = await traverseKnowledge({
 *   from: '@(knowledge/decisions/abc-123)',
 *   relationshipType: 'supports',
 *   direction: 'inbound',
 *   maxDepth: 3
 * });
 */
export async function traverseKnowledge(
  options: KnowledgeTraversalOptions
): Promise<{
  nodes: Address[];
  relationships: Array<{
    from: Address;
    to: Address;
    type: string;
    strength?: number;
    depth: number;
  }>;
}> {
  // This would integrate with RelationshipActor
  // For now, return placeholder structure
  return {
    nodes: [options.from],
    relationships: [],
  };
}

/**
 * Create knowledge traversal spec for query builder
 *
 * @example
 * query()
 *   .match(knowledge('decision').id('abc-123'))
 *   .traverse(knowledgeTraversal('decision', {
 *     relationshipType: 'supports',
 *     maxDepth: 3
 *   }))
 */
export function knowledgeTraversal(
  from: string,
  options: {
    relationshipType?: 'supports' | 'contradicts' | 'requires' | 'extends' | 'questions' | 'related-to';
    direction?: 'outbound' | 'inbound' | 'both';
    maxDepth?: number;
    as?: string;
  } = {}
): TraversalSpec {
  return {
    from,
    relationship: options.relationshipType,
    direction: options.direction || 'outbound',
    depth: {
      max: options.maxDepth || 3,
    },
    as: options.as || `${from}_network`,
  };
}

/**
 * Helper to build knowledge addresses
 */
export function knowledgeAddress(
  category: 'decision' | 'learning' | 'error',
  id: string
): Address {
  return `@(knowledge/${category}s/${id})` as Address;
}

/**
 * Helper to parse knowledge address
 */
export function parseKnowledgeAddress(address: Address): {
  category: 'decision' | 'learning' | 'error';
  id: string;
} | null {
  const match = address.match(/^@\(knowledge\/(decision|learning|error)s\/(.+)\)$/);
  if (!match) return null;

  return {
    category: match[1] as 'decision' | 'learning' | 'error',
    id: match[2],
  };
}

/**
 * Export pattern builder shorthand
 */
export const knowledge = KnowledgePatternBuilder.knowledge;
export const decisions = KnowledgePatternBuilder.decisions;
export const learnings = KnowledgePatternBuilder.learnings;
export const errors = KnowledgePatternBuilder.errors;
