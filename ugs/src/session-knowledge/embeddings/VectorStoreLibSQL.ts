#!/usr/bin/env bun
/**
 * VectorStore - libsql native vector support
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { createClient, type Client } from '@libsql/client';

export interface VectorSearchResult {
  id: string;
  content: string;
  distance: number;
  sessionId?: string;
  timestamp?: number;
}

/**
 * Vector store using libsql native F32_BLOB vectors
 */
export class VectorStoreLibSQL {
  private client: Client;

  constructor(dbPath: string) {
    this.client = createClient({
      url: `file:${dbPath}`
    });
  }

  /**
   * Store embedding for a message
   */
  async storeMessageEmbedding(
    sessionId: string,
    messageId: string,
    content: string,
    embedding: Float32Array,
    timestamp: number
  ): Promise<void> {
    // Convert Float32Array to JSON array string for vector() function
    const vectorJson = `[${Array.from(embedding).join(',')}]`;

    await this.client.execute({
      sql: `INSERT INTO message_embeddings (session_id, message_id, content, embedding, timestamp)
            VALUES (?, ?, ?, vector(?), ?)
            ON CONFLICT(message_id) DO UPDATE SET
              embedding = excluded.embedding,
              content = excluded.content`,
      args: [sessionId, messageId, content, vectorJson, timestamp]
    });
  }

  /**
   * Store embedding for a session summary
   */
  async storeSessionEmbedding(
    sessionId: string,
    summary: string,
    embedding: Float32Array
  ): Promise<void> {
    const vectorJson = `[${Array.from(embedding).join(',')}]`;

    await this.client.execute({
      sql: `UPDATE sessions
            SET summary_embedding = vector(?)
            WHERE id = ?`,
      args: [vectorJson, sessionId]
    });
  }

  /**
   * Semantic search across messages using DiskANN
   */
  async searchMessages(
    queryEmbedding: Float32Array,
    limit: number = 10,
    sessionId?: string
  ): Promise<VectorSearchResult[]> {
    const vectorJson = `[${Array.from(queryEmbedding).join(',')}]`;

    // Use vector_top_k for ANN search with index
    const sql = sessionId
      ? `SELECT m.message_id as id, m.session_id as sessionId, m.content, m.timestamp,
                vector_distance_cos(m.embedding, vector(?)) as distance
         FROM message_embeddings m
         WHERE m.session_id = ?
         ORDER BY distance ASC
         LIMIT ?`
      : `SELECT m.message_id as id, m.session_id as sessionId, m.content, m.timestamp,
                vector_distance_cos(m.embedding, vector(?)) as distance
         FROM message_embeddings m
         ORDER BY distance ASC
         LIMIT ?`;

    const args = sessionId ? [vectorJson, sessionId, limit] : [vectorJson, limit];

    const result = await this.client.execute({ sql, args });

    return result.rows.map((row: any) => ({
      id: row.id as string,
      sessionId: row.sessionId as string,
      content: row.content as string,
      timestamp: row.timestamp as number,
      distance: row.distance as number
    }));
  }

  /**
   * Semantic search across session summaries
   */
  async searchSessions(
    queryEmbedding: Float32Array,
    limit: number = 10
  ): Promise<VectorSearchResult[]> {
    const vectorJson = `[${Array.from(queryEmbedding).join(',')}]`;

    const result = await this.client.execute({
      sql: `SELECT id, summary as content, created as timestamp,
                   vector_distance_cos(summary_embedding, vector(?)) as distance
            FROM sessions
            WHERE summary_embedding IS NOT NULL
            ORDER BY distance ASC
            LIMIT ?`,
      args: [vectorJson, limit]
    });

    return result.rows.map((row: any) => ({
      id: row.id as string,
      content: row.content as string,
      timestamp: row.timestamp as number,
      distance: row.distance as number
    }));
  }

  /**
   * Find similar sessions to a given session
   */
  async findSimilarSessions(sessionId: string, limit: number = 5): Promise<VectorSearchResult[]> {
    const session = await this.client.execute({
      sql: 'SELECT summary_embedding FROM sessions WHERE id = ?',
      args: [sessionId]
    });

    if (!session.rows[0]?.summary_embedding) {
      return [];
    }

    const embedding = session.rows[0].summary_embedding;

    const result = await this.client.execute({
      sql: `SELECT id, summary as content, created as timestamp,
                   vector_distance_cos(summary_embedding, ?) as distance
            FROM sessions
            WHERE id != ? AND summary_embedding IS NOT NULL
            ORDER BY distance ASC
            LIMIT ?`,
      args: [embedding, sessionId, limit]
    });

    return result.rows.map((row: any) => ({
      id: row.id as string,
      content: row.content as string,
      timestamp: row.timestamp as number,
      distance: row.distance as number
    }));
  }

  /**
   * Get embedding statistics
   */
  async getStats(): Promise<{
    messageEmbeddings: number;
    sessionEmbeddings: number;
    totalSize: number;
    avgDimensions: number;
  }> {
    const msgCount = await this.client.execute(
      'SELECT COUNT(*) as count FROM message_embeddings'
    );

    const sessionCount = await this.client.execute(
      'SELECT COUNT(*) as count FROM sessions WHERE summary_embedding IS NOT NULL'
    );

    // Get actual embedding size from a sample
    const sample = await this.client.execute(
      'SELECT LENGTH(embedding) as size FROM message_embeddings LIMIT 1'
    );

    const bytesPerEmbedding = (sample.rows[0]?.size as number) || 3072; // Default: 768 dims × 4 bytes
    const dimensions = bytesPerEmbedding / 4;
    const totalCount = (msgCount.rows[0]?.count as number) + (sessionCount.rows[0]?.count as number);
    const totalSize = totalCount * bytesPerEmbedding;

    return {
      messageEmbeddings: msgCount.rows[0]?.count as number || 0,
      sessionEmbeddings: sessionCount.rows[0]?.count as number || 0,
      totalSize,
      avgDimensions: dimensions
    };
  }

  async close(): Promise<void> {
    this.client.close();
  }
}
