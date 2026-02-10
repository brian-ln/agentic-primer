/**
 * KnowledgeStore - Persistent storage for graph-addressable knowledge
 *
 * CRUD operations for knowledge items and relationships.
 * Uses ISqlStorage interface — no direct dependency on any SQL driver.
 */

import type { ISqlStorage, SqlValue } from '@agentic-primer/actors';
import type {
  KnowledgeItem,
  KnowledgeCategory,
  KnowledgeFilter,
  Relationship,
  RelationshipType,
  RelationshipFilter,
  EvidenceLink,
  EpistemicLevel,
} from './types.ts';

export class KnowledgeStore {
  constructor(private storage: ISqlStorage) {}

  // --- Knowledge Operations ---

  async createKnowledge(item: KnowledgeItem): Promise<void> {
    const table = `session_${item.category}s`;
    const columns = this.getKnowledgeColumns(item.category);

    await this.storage.execute(
      `INSERT INTO ${table}
            (id, session_id, timestamp, ${columns.content}, ${columns.reasoning},
             epistemic_level, confidence, evidence, last_validated)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      [
        item.id,
        item.session_id || 'unknown',
        item.created,
        item.content,
        item.reasoning || null,
        item.epistemic_level,
        item.confidence,
        JSON.stringify(item.evidence),
        item.last_validated || item.created,
      ]
    );
  }

  async getKnowledge(id: string, category: KnowledgeCategory): Promise<KnowledgeItem | null> {
    const table = `session_${category}s`;
    const columns = this.getKnowledgeColumns(category);

    const result = await this.storage.execute(
      `SELECT id, session_id, timestamp, ${columns.content}, ${columns.reasoning},
                   epistemic_level, confidence, evidence, last_validated
            FROM ${table}
            WHERE id = ?`,
      [id]
    );

    if (result.rows.length === 0) return null;
    return this.mapRowToKnowledge(result.columns, result.rows[0], category);
  }

  async getKnowledgeById(id: string): Promise<KnowledgeItem | null> {
    const categories: KnowledgeCategory[] = ['decision', 'learning', 'error'];
    for (const category of categories) {
      const item = await this.getKnowledge(id, category);
      if (item) return item;
    }
    return null;
  }

  async queryKnowledge(filter: KnowledgeFilter, limit: number = 100): Promise<KnowledgeItem[]> {
    if (filter.category) {
      const table = `session_${filter.category}s`;
      const columns = this.getKnowledgeColumns(filter.category);

      let sql = `SELECT id, session_id, timestamp, ${columns.content}, ${columns.reasoning},
                        epistemic_level, confidence, evidence, last_validated
                 FROM ${table} WHERE 1=1`;
      const args: SqlValue[] = [];

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

      const result = await this.storage.execute(sql, args);
      return result.rows.map((row) => this.mapRowToKnowledge(result.columns, row, filter.category!));
    }

    // Query all categories
    const categories: KnowledgeCategory[] = ['decision', 'learning', 'error'];
    const results: KnowledgeItem[] = [];
    for (const category of categories) {
      const items = await this.queryKnowledge({ ...filter, category }, limit);
      results.push(...items);
    }
    return results.slice(0, limit);
  }

  async updateKnowledge(id: string, category: KnowledgeCategory, updates: Partial<KnowledgeItem>): Promise<void> {
    const table = `session_${category}s`;
    const setClauses: string[] = [];
    const args: SqlValue[] = [];

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
    await this.storage.execute(
      `UPDATE ${table} SET ${setClauses.join(', ')} WHERE id = ?`,
      args
    );
  }

  // --- Relationship Operations ---

  async createRelationship(rel: Relationship): Promise<void> {
    const from = this.parseKnowledgeAddress(rel.from);
    const to = this.parseKnowledgeAddress(rel.to);

    await this.storage.execute(
      `INSERT INTO knowledge_relationships
            (id, from_type, from_id, to_type, to_id, relationship_type, confidence, created_at, evidence)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      [
        rel.id,
        from.type, from.id,
        to.type, to.id,
        rel.type,
        rel.strength || 1.0,
        rel.created,
        rel.evidence || null,
      ]
    );
  }

  async getRelationship(id: string): Promise<Relationship | null> {
    const result = await this.storage.execute(
      'SELECT * FROM knowledge_relationships WHERE id = ?',
      [id]
    );
    if (result.rows.length === 0) return null;
    return this.mapRowToRelationship(result.columns, result.rows[0]);
  }

  async queryRelationships(filter: RelationshipFilter, limit: number = 100): Promise<Relationship[]> {
    let sql = 'SELECT * FROM knowledge_relationships WHERE 1=1';
    const args: SqlValue[] = [];

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

    const result = await this.storage.execute(sql, args);
    return result.rows.map((row) => this.mapRowToRelationship(result.columns, row));
  }

  async deleteRelationship(id: string): Promise<void> {
    await this.storage.execute(
      'DELETE FROM knowledge_relationships WHERE id = ?',
      [id]
    );
  }

  async getRelationshipsFrom(address: string): Promise<Relationship[]> {
    const parsed = this.parseKnowledgeAddress(address);
    const result = await this.storage.execute(
      'SELECT * FROM knowledge_relationships WHERE from_type = ? AND from_id = ?',
      [parsed.type, parsed.id]
    );
    return result.rows.map((row) => this.mapRowToRelationship(result.columns, row));
  }

  async getRelationshipsTo(address: string): Promise<Relationship[]> {
    const parsed = this.parseKnowledgeAddress(address);
    const result = await this.storage.execute(
      'SELECT * FROM knowledge_relationships WHERE to_type = ? AND to_id = ?',
      [parsed.type, parsed.id]
    );
    return result.rows.map((row) => this.mapRowToRelationship(result.columns, row));
  }

  // --- Helpers ---

  private getKnowledgeColumns(category: KnowledgeCategory): { content: string; reasoning: string } {
    switch (category) {
      case 'decision': return { content: 'decision', reasoning: 'reasoning' };
      case 'learning': return { content: 'learning', reasoning: 'context' };
      case 'error':    return { content: 'error_type', reasoning: 'root_cause' };
    }
  }

  /** Get a column value by name from a positional row. */
  private col(columns: string[], row: SqlValue[], name: string): SqlValue {
    const idx = columns.indexOf(name);
    return idx >= 0 ? row[idx] : null;
  }

  private mapRowToKnowledge(columns: string[], row: SqlValue[], category: KnowledgeCategory): KnowledgeItem {
    const cols = this.getKnowledgeColumns(category);

    let evidence: EvidenceLink[] = [];
    const evidenceRaw = this.col(columns, row, 'evidence');
    if (evidenceRaw && typeof evidenceRaw === 'string') {
      try {
        const parsed = JSON.parse(evidenceRaw);
        evidence = Array.isArray(parsed) ? parsed : [];
      } catch {
        evidence = [{ type: 'CITED', description: evidenceRaw, confidence: this.col(columns, row, 'confidence') as number }];
      }
    }

    return {
      id: this.col(columns, row, 'id') as string,
      category,
      content: this.col(columns, row, cols.content) as string,
      reasoning: this.col(columns, row, cols.reasoning) as string | undefined,
      epistemic_level: this.col(columns, row, 'epistemic_level') as EpistemicLevel,
      confidence: this.col(columns, row, 'confidence') as number,
      evidence,
      created: this.col(columns, row, 'timestamp') as number,
      last_validated: this.col(columns, row, 'last_validated') as number | undefined,
      session_id: this.col(columns, row, 'session_id') as string | undefined,
    };
  }

  private mapRowToRelationship(columns: string[], row: SqlValue[]): Relationship {
    return {
      id: this.col(columns, row, 'id') as string,
      type: this.col(columns, row, 'relationship_type') as RelationshipType,
      from: `@(${this.col(columns, row, 'from_type')}/${this.col(columns, row, 'from_id')})`,
      to: `@(${this.col(columns, row, 'to_type')}/${this.col(columns, row, 'to_id')})`,
      strength: this.col(columns, row, 'confidence') as number,
      evidence: this.col(columns, row, 'evidence') as string | undefined,
      created: this.col(columns, row, 'created_at') as number,
      metadata: {},
    };
  }

  private parseKnowledgeAddress(address: string): { type: string; id: string } {
    const match = address.match(/^@\((.+)\/([^/]+)\)$/);
    if (!match) {
      throw new Error(`Invalid knowledge address: ${address}`);
    }
    return { type: match[1], id: match[2] };
  }
}
