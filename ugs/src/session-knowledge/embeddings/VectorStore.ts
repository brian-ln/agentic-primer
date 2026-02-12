#!/usr/bin/env bun
/**
 * VectorStore - sqlite-vec integration for semantic search
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { Database } from 'bun:sqlite';
import * as sqliteVec from 'sqlite-vec';

export interface VectorSearchResult {
  id: string;
  content: string;
  distance: number;
  sessionId?: string;
  timestamp?: number;
}

/**
 * Vector store using sqlite-vec extension
 */
export class VectorStore {
  private db: Database;
  private loaded: boolean = false;

  constructor(db: Database) {
    this.db = db;
  }

  /**
   * Load sqlite-vec extension into database
   */
  async load(): Promise<void> {
    if (this.loaded) return;

    try {
      sqliteVec.load(this.db);
      this.loaded = true;
    } catch (err) {
      throw new Error(`Failed to load sqlite-vec: ${err.message}\n\nMake sure to call Database.setCustomSQLite() before creating the database instance.`);
    }
  }

  /**
   * Store embedding for a message
   */
  storeMessageEmbedding(
    sessionId: string,
    messageId: string,
    content: string,
    embedding: Float32Array,
    timestamp: number
  ): void {
    this.db.run(
      `INSERT INTO message_embeddings (session_id, message_id, content, embedding, timestamp)
       VALUES (?, ?, ?, ?, ?)
       ON CONFLICT(message_id) DO UPDATE SET
         embedding = excluded.embedding,
         content = excluded.content`,
      [sessionId, messageId, content, embedding, timestamp]
    );
  }

  /**
   * Store embedding for a session summary
   */
  storeSessionEmbedding(
    sessionId: string,
    summary: string,
    embedding: Float32Array
  ): void {
    this.db.run(
      `UPDATE sessions
       SET summary_embedding = ?
       WHERE id = ?`,
      [embedding, sessionId]
    );
  }

  /**
   * Semantic search across messages
   */
  searchMessages(
    queryEmbedding: Float32Array,
    limit: number = 10,
    sessionId?: string
  ): VectorSearchResult[] {
    const sessionFilter = sessionId ? 'AND session_id = ?' : '';
    const params = sessionId ? [queryEmbedding, limit, sessionId] : [queryEmbedding, limit];

    const results = this.db.query(`
      SELECT
        message_id as id,
        session_id as sessionId,
        content,
        timestamp,
        vec_distance_cosine(embedding, ?) as distance
      FROM message_embeddings
      WHERE 1=1 ${sessionFilter}
      ORDER BY distance ASC
      LIMIT ?
    `).all(...params) as any[];

    return results.map(r => ({
      id: r.id,
      sessionId: r.sessionId,
      content: r.content,
      timestamp: r.timestamp,
      distance: r.distance
    }));
  }

  /**
   * Semantic search across session summaries
   */
  searchSessions(
    queryEmbedding: Float32Array,
    limit: number = 10
  ): VectorSearchResult[] {
    const results = this.db.query(`
      SELECT
        id,
        summary as content,
        created as timestamp,
        vec_distance_cosine(summary_embedding, ?) as distance
      FROM sessions
      WHERE summary_embedding IS NOT NULL
      ORDER BY distance ASC
      LIMIT ?
    `).all(queryEmbedding, limit) as any[];

    return results.map(r => ({
      id: r.id,
      content: r.content,
      timestamp: r.timestamp,
      distance: r.distance
    }));
  }

  /**
   * Find similar sessions to a given session
   */
  findSimilarSessions(sessionId: string, limit: number = 5): VectorSearchResult[] {
    const session = this.db.query(`
      SELECT summary_embedding FROM sessions WHERE id = ?
    `).get(sessionId) as any;

    if (!session?.summary_embedding) {
      return [];
    }

    const results = this.db.query(`
      SELECT
        id,
        summary as content,
        created as timestamp,
        vec_distance_cosine(summary_embedding, ?) as distance
      FROM sessions
      WHERE id != ? AND summary_embedding IS NOT NULL
      ORDER BY distance ASC
      LIMIT ?
    `).all(session.summary_embedding, sessionId, limit) as any[];

    return results.map(r => ({
      id: r.id,
      content: r.content,
      timestamp: r.timestamp,
      distance: r.distance
    }));
  }

  /**
   * Get embedding statistics
   */
  getStats(): {
    messageEmbeddings: number;
    sessionEmbeddings: number;
    totalSize: number;
    avgDimensions: number;
  } {
    const msgCount = this.db.query(
      'SELECT COUNT(*) as count FROM message_embeddings'
    ).get() as any;

    const sessionCount = this.db.query(
      'SELECT COUNT(*) as count FROM sessions WHERE summary_embedding IS NOT NULL'
    ).get() as any;

    // Get actual embedding size from a sample
    const sample = this.db.query(
      'SELECT LENGTH(embedding) as size FROM message_embeddings LIMIT 1'
    ).get() as any;

    const bytesPerEmbedding = sample?.size || 3072; // Default: 768 dims × 4 bytes
    const dimensions = bytesPerEmbedding / 4;
    const totalSize = (msgCount.count + sessionCount.count) * bytesPerEmbedding;

    return {
      messageEmbeddings: msgCount.count,
      sessionEmbeddings: sessionCount.count,
      totalSize,
      avgDimensions: dimensions
    };
  }
}
