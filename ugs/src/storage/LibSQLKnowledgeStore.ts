#!/usr/bin/env bun
/**
 * LibSQL Knowledge Store - Persistent storage for graph-addressable knowledge
 *
 * Adapts KnowledgeActor and RelationshipActor to use libSQL database.
 * Connects to existing session-knowledge schema.
 */

import type { Client } from '@libsql/client';
import { createClient } from '@libsql/client';
import { join } from 'path';
import type { Address } from '@agentic-primer/actors';
import type { KnowledgeItem, EpistemicLevel, EvidenceLink } from '../messaging/actors/knowledge.ts';
import type { Relationship, RelationshipType } from '../messaging/actors/relationship.ts';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

/**
 * Storage adapter for knowledge and relationships
 */
export class LibSQLKnowledgeStore {
  private client: Client;

  constructor(dbPath?: string) {
    this.client = createClient({
      url: `file:${dbPath || DB_PATH}`
    });
  }

  /**
   * Knowledge operations
   */

  async createKnowledge(item: KnowledgeItem): Promise<void> {
    const table = `session_${item.category}s`;

    // Map category to table columns
    const columns = this.getKnowledgeColumns(item.category);

    await this.client.execute({
      sql: `INSERT INTO ${table}
            (id, session_id, timestamp, ${columns.content}, ${columns.reasoning},
             epistemic_level, confidence, evidence, last_validated)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      args: [
        item.id,
        item.session_id || 'unknown',
        item.created,
        item.content,
        item.reasoning || null,
        item.epistemic_level,
        item.confidence,
        JSON.stringify(item.evidence),
        item.last_validated || item.created
      ]
    });
  }

  async getKnowledge(id: string, category: 'decision' | 'learning' | 'error'): Promise<KnowledgeItem | null> {
    const table = `session_${category}s`;
    const columns = this.getKnowledgeColumns(category);

    const result = await this.client.execute({
      sql: `SELECT id, session_id, timestamp, ${columns.content}, ${columns.reasoning},
                   epistemic_level, confidence, evidence, last_validated
            FROM ${table}
            WHERE id = ?`,
      args: [id]
    });

    if (result.rows.length === 0) return null;

    const row: any = result.rows[0];
    return this.mapRowToKnowledge(row, category);
  }

  async getKnowledgeById(id: string): Promise<KnowledgeItem | null> {
    // Search all categories to find the item
    const categories: Array<'decision' | 'learning' | 'error'> = ['decision', 'learning', 'error'];

    for (const category of categories) {
      const item = await this.getKnowledge(id, category);
      if (item) return item;
    }

    return null;
  }

  async queryKnowledge(filter: any, limit: number = 100): Promise<KnowledgeItem[]> {
    // Build query based on filters
    let sql = '';
    const args: any[] = [];

    if (filter.category) {
      const table = `session_${filter.category}s`;
      const columns = this.getKnowledgeColumns(filter.category);

      sql = `SELECT id, session_id, timestamp, ${columns.content}, ${columns.reasoning},
                    epistemic_level, confidence, evidence, last_validated
             FROM ${table} WHERE 1=1`;

      if (filter.epistemic_level) {
        sql += ' AND epistemic_level = ?';
        args.push(filter.epistemic_level);
      }

      if (filter.min_confidence !== undefined) {
        sql += ' AND confidence >= ?';
        args.push(filter.min_confidence);
      }

      if (filter.max_confidence !== undefined) {
        sql += ' AND confidence <= ?';
        args.push(filter.max_confidence);
      }

      if (filter.session_id) {
        sql += ' AND session_id = ?';
        args.push(filter.session_id);
      }

      sql += ' ORDER BY timestamp DESC LIMIT ?';
      args.push(limit);

      const result = await this.client.execute({ sql, args });
      return result.rows.map((row: any) => this.mapRowToKnowledge(row, filter.category));
    } else {
      // Query all categories
      const categories: Array<'decision' | 'learning' | 'error'> = ['decision', 'learning', 'error'];
      const results: KnowledgeItem[] = [];

      for (const category of categories) {
        const categoryFilter = { ...filter, category };
        const items = await this.queryKnowledge(categoryFilter, limit);
        results.push(...items);
      }

      return results.slice(0, limit);
    }
  }

  async updateKnowledge(id: string, category: 'decision' | 'learning' | 'error', updates: Partial<KnowledgeItem>): Promise<void> {
    const table = `session_${category}s`;
    const setClauses: string[] = [];
    const args: any[] = [];

    if (updates.epistemic_level) {
      setClauses.push('epistemic_level = ?');
      args.push(updates.epistemic_level);
    }

    if (updates.confidence !== undefined) {
      setClauses.push('confidence = ?');
      args.push(updates.confidence);
      setClauses.push('last_validated = ?');
      args.push(Date.now());
    }

    if (updates.reasoning) {
      const columns = this.getKnowledgeColumns(category);
      setClauses.push(`${columns.reasoning} = ?`);
      args.push(updates.reasoning);
    }

    if (updates.evidence) {
      setClauses.push('evidence = ?');
      args.push(JSON.stringify(updates.evidence));
    }

    if (setClauses.length === 0) return;

    args.push(id);

    await this.client.execute({
      sql: `UPDATE ${table} SET ${setClauses.join(', ')} WHERE id = ?`,
      args
    });
  }

  /**
   * Relationship operations
   */

  async createRelationship(rel: Relationship): Promise<void> {
    // Parse addresses to extract type and id
    // @(knowledge/decisions/k_123) -> type: knowledge/decisions, id: k_123
    const from = this.parseKnowledgeAddress(rel.from);
    const to = this.parseKnowledgeAddress(rel.to);

    await this.client.execute({
      sql: `INSERT INTO knowledge_relationships
            (id, from_type, from_id, to_type, to_id, relationship_type, confidence, created_at, evidence)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      args: [
        rel.id,
        from.type,
        from.id,
        to.type,
        to.id,
        rel.type,
        rel.strength || 1.0,
        rel.created,
        rel.evidence || null
      ]
    });
  }

  async getRelationship(id: string): Promise<Relationship | null> {
    const result = await this.client.execute({
      sql: `SELECT * FROM knowledge_relationships WHERE id = ?`,
      args: [id]
    });

    if (result.rows.length === 0) return null;

    return this.mapRowToRelationship(result.rows[0] as any);
  }

  async queryRelationships(filter: any, limit: number = 100): Promise<Relationship[]> {
    let sql = 'SELECT * FROM knowledge_relationships WHERE 1=1';
    const args: any[] = [];

    if (filter.type) {
      sql += ' AND relationship_type = ?';
      args.push(filter.type);
    }

    if (filter.from) {
      const from = this.parseKnowledgeAddress(filter.from);
      sql += ' AND from_type = ? AND from_id = ?';
      args.push(from.type, from.id);
    }

    if (filter.to) {
      const to = this.parseKnowledgeAddress(filter.to);
      sql += ' AND to_type = ? AND to_id = ?';
      args.push(to.type, to.id);
    }

    if (filter.min_strength !== undefined) {
      sql += ' AND confidence >= ?';
      args.push(filter.min_strength);
    }

    sql += ' ORDER BY created_at DESC LIMIT ?';
    args.push(limit);

    const result = await this.client.execute({ sql, args });
    return result.rows.map((row: any) => this.mapRowToRelationship(row));
  }

  async updateRelationship(rel: Relationship): Promise<void> {
    const fromParsed = this.parseKnowledgeAddress(rel.from);
    const toParsed = this.parseKnowledgeAddress(rel.to);

    await this.client.execute({
      sql: `UPDATE knowledge_relationships
            SET type = ?,
                from_type = ?, from_id = ?,
                to_type = ?, to_id = ?,
                strength = ?,
                evidence = ?,
                metadata = ?
            WHERE id = ?`,
      args: [
        rel.type,
        fromParsed.type, fromParsed.id,
        toParsed.type, toParsed.id,
        rel.strength ?? null,
        rel.evidence ?? null,
        JSON.stringify(rel.metadata || {}),
        rel.id
      ]
    });
  }

  async deleteRelationship(id: string): Promise<void> {
    await this.client.execute({
      sql: 'DELETE FROM knowledge_relationships WHERE id = ?',
      args: [id]
    });
  }

  /**
   * Graph traversal helpers
   */

  async getRelationshipsFrom(address: Address): Promise<Relationship[]> {
    const parsed = this.parseKnowledgeAddress(address);

    const result = await this.client.execute({
      sql: 'SELECT * FROM knowledge_relationships WHERE from_type = ? AND from_id = ?',
      args: [parsed.type, parsed.id]
    });

    return result.rows.map((row: any) => this.mapRowToRelationship(row));
  }

  async getRelationshipsTo(address: Address): Promise<Relationship[]> {
    const parsed = this.parseKnowledgeAddress(address);

    const result = await this.client.execute({
      sql: 'SELECT * FROM knowledge_relationships WHERE to_type = ? AND to_id = ?',
      args: [parsed.type, parsed.id]
    });

    return result.rows.map((row: any) => this.mapRowToRelationship(row));
  }

  /**
   * Helper methods
   */

  private getKnowledgeColumns(category: 'decision' | 'learning' | 'error'): { content: string; reasoning: string } {
    switch (category) {
      case 'decision':
        return { content: 'decision', reasoning: 'reasoning' };
      case 'learning':
        return { content: 'learning', reasoning: 'context' };
      case 'error':
        return { content: 'error_type', reasoning: 'root_cause' };
    }
  }

  private mapRowToKnowledge(row: any, category: 'decision' | 'learning' | 'error'): KnowledgeItem {
    const columns = this.getKnowledgeColumns(category);

    // Handle evidence - could be JSON array (new format) or plain text (old format)
    let evidence: EvidenceLink[] = [];
    if (row.evidence) {
      const evidenceStr = row.evidence as string;
      try {
        // Try parsing as JSON array
        const parsed = JSON.parse(evidenceStr);
        evidence = Array.isArray(parsed) ? parsed : [];
      } catch {
        // If not JSON, treat as plain text evidence (old format)
        evidence = [{
          type: 'CITED',
          description: evidenceStr,
          confidence: row.confidence as number
        }];
      }
    }

    return {
      id: row.id as string,
      category,
      content: row[columns.content] as string,
      reasoning: row[columns.reasoning] as string | undefined,
      epistemic_level: row.epistemic_level as EpistemicLevel,
      confidence: row.confidence as number,
      evidence,
      created: row.timestamp as number,
      last_validated: row.last_validated as number | undefined,
      session_id: row.session_id as string | undefined
    };
  }

  private mapRowToRelationship(row: any): Relationship {
    return {
      id: row.id as string,
      type: row.relationship_type as RelationshipType,
      from: this.buildKnowledgeAddress(row.from_type, row.from_id),
      to: this.buildKnowledgeAddress(row.to_type, row.to_id),
      strength: row.confidence as number,
      evidence: row.evidence as string | undefined,
      created: row.created_at as number,
      metadata: {}
    };
  }

  private parseKnowledgeAddress(address: Address): { type: string; id: string } {
    // Parse @(knowledge/decisions/k_123) -> { type: "knowledge/decisions", id: "k_123" }
    const match = address.match(/^@\((.+)\/([^/]+)\)$/);
    if (!match) {
      throw new Error(`Invalid knowledge address: ${address}`);
    }

    return {
      type: match[1],      // "knowledge/decisions"
      id: match[2]         // "k_123"
    };
  }

  private buildKnowledgeAddress(type: string, id: string): Address {
    return `@(${type}/${id})` as Address;
  }

  async close(): Promise<void> {
    await this.client.close();
  }
}

export default LibSQLKnowledgeStore;
