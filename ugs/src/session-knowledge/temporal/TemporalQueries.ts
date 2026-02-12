/**
 * Temporal Query API
 * Epic: agentic-primer-9ad
 * Phase: Cognitive Integration v1
 *
 * Provides bi-temporal querying capabilities:
 * - Query knowledge as it was known at a specific point in time
 * - Track validity periods for facts
 * - Detect changes between time periods
 * - Apply confidence decay to knowledge based on age
 */

import { createClient, type Client } from '@libsql/client';
import { join } from 'path';
import { ConfidenceDecay } from './ConfidenceDecay';
import { sanitizeLikePattern, withRateLimit, dbRateLimiter } from '@agentic-primer/knowledge';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

export interface Knowledge {
  id: string;
  type: 'decision' | 'learning' | 'error' | 'workflow';
  content: string;
  sessionId: string;
  timestamp: number;
  validFrom: number | null;
  validTo: number | null;
  transactionFrom: number | null;
  transactionTo: number | null;
  baseConfidence: number;
  currentConfidence?: number; // With decay applied
  domain: string | null;
  metadata: Record<string, any>;
}

export interface Change {
  knowledgeId: string;
  type: 'decision' | 'learning' | 'error' | 'workflow';
  changeType: 'added' | 'modified' | 'invalidated';
  timestamp: number;
  content: string;
  previousContent?: string;
}

export interface ValidityPeriod {
  validFrom: Date;
  validTo: Date | null; // null = still valid
  transactionFrom: Date;
  transactionTo: Date | null; // null = current version
}

export class TemporalQueries {
  private db: Client;
  private decay: ConfidenceDecay;

  constructor() {
    this.db = createClient({ url: `file:${DB_PATH}` });
    this.decay = new ConfidenceDecay();
  }

  /**
   * Query knowledge as it was known at a specific point in time
   *
   * This uses both valid_time and transaction_time:
   * - valid_time: When the fact was true in the real world
   * - transaction_time: When we learned about it
   *
   * Example: "What did we decide about auth on 2026-01-15?"
   */
  async queryAtTime(query: string, asOf: Date): Promise<Knowledge[]> {
    const asOfMs = asOf.getTime();
    const sanitizedQuery = sanitizeLikePattern(query);

    // Search across all knowledge types that were:
    // 1. Valid at the requested time (valid_from <= asOf <= valid_to OR valid_to IS NULL)
    // 2. Known at the requested time (transaction_from <= asOf AND (transaction_to IS NULL OR transaction_to > asOf))

    const results: Knowledge[] = [];

    // Query decisions
    const decisions = await withRateLimit(async () => {
      return await this.db.execute({
        sql: `
          SELECT id, decision as content, session_id, timestamp,
                 valid_from, valid_to, transaction_from, transaction_to,
                 base_confidence, domain, reasoning, alternatives, context
          FROM session_decisions
          WHERE (valid_from IS NULL OR valid_from <= ?)
            AND (valid_to IS NULL OR valid_to > ?)
            AND (transaction_from IS NULL OR transaction_from <= ?)
            AND (transaction_to IS NULL OR transaction_to > ?)
            AND (decision LIKE '%' || ? || '%' ESCAPE '\\' OR reasoning LIKE '%' || ? || '%' ESCAPE '\\')
          ORDER BY timestamp DESC
        `,
        args: [asOfMs, asOfMs, asOfMs, asOfMs, sanitizedQuery, sanitizedQuery]
      });
    }, dbRateLimiter);

    for (const row of decisions.rows) {
      results.push({
        id: row.id as string,
        type: 'decision',
        content: row.content as string,
        sessionId: row.session_id as string,
        timestamp: row.timestamp as number,
        validFrom: row.valid_from as number | null,
        validTo: row.valid_to as number | null,
        transactionFrom: row.transaction_from as number | null,
        transactionTo: row.transaction_to as number | null,
        baseConfidence: row.base_confidence as number,
        domain: row.domain as string | null,
        metadata: {
          reasoning: row.reasoning,
          alternatives: row.alternatives,
          context: row.context
        }
      });
    }

    // Query learnings
    const learnings = await withRateLimit(async () => {
      return await this.db.execute({
        sql: `
          SELECT id, learning as content, session_id, timestamp,
                 valid_from, valid_to, transaction_from, transaction_to,
                 base_confidence, domain, category, evidence, application, context, actionable
          FROM session_learnings
          WHERE (valid_from IS NULL OR valid_from <= ?)
            AND (valid_to IS NULL OR valid_to > ?)
            AND (transaction_from IS NULL OR transaction_from <= ?)
            AND (transaction_to IS NULL OR transaction_to > ?)
            AND (learning LIKE '%' || ? || '%' ESCAPE '\\' OR context LIKE '%' || ? || '%' ESCAPE '\\')
          ORDER BY timestamp DESC
        `,
        args: [asOfMs, asOfMs, asOfMs, asOfMs, sanitizedQuery, sanitizedQuery]
      });
    }, dbRateLimiter);

    for (const row of learnings.rows) {
      results.push({
        id: row.id as string,
        type: 'learning',
        content: row.content as string,
        sessionId: row.session_id as string,
        timestamp: row.timestamp as number,
        validFrom: row.valid_from as number | null,
        validTo: row.valid_to as number | null,
        transactionFrom: row.transaction_from as number | null,
        transactionTo: row.transaction_to as number | null,
        baseConfidence: row.base_confidence as number,
        domain: row.domain as string | null,
        metadata: {
          category: row.category,
          evidence: row.evidence,
          application: row.application,
          context: row.context,
          actionable: row.actionable
        }
      });
    }

    // Query errors
    const errors = await withRateLimit(async () => {
      return await this.db.execute({
        sql: `
          SELECT id, error_message as content, session_id, timestamp,
                 valid_from, valid_to, transaction_from, transaction_to,
                 base_confidence, domain, error_type, tool_name, root_cause, suggested_fix, resolution, prevention
          FROM session_errors
          WHERE (valid_from IS NULL OR valid_from <= ?)
            AND (valid_to IS NULL OR valid_to > ?)
            AND (transaction_from IS NULL OR transaction_from <= ?)
            AND (transaction_to IS NULL OR transaction_to > ?)
            AND (error_message LIKE '%' || ? || '%' ESCAPE '\\' OR root_cause LIKE '%' || ? || '%' ESCAPE '\\')
          ORDER BY timestamp DESC
        `,
        args: [asOfMs, asOfMs, asOfMs, asOfMs, sanitizedQuery, sanitizedQuery]
      });
    }, dbRateLimiter);

    for (const row of errors.rows) {
      results.push({
        id: row.id as string,
        type: 'error',
        content: row.content as string,
        sessionId: row.session_id as string,
        timestamp: row.timestamp as number,
        validFrom: row.valid_from as number | null,
        validTo: row.valid_to as number | null,
        transactionFrom: row.transaction_from as number | null,
        transactionTo: row.transaction_to as number | null,
        baseConfidence: row.base_confidence as number,
        domain: row.domain as string | null,
        metadata: {
          errorType: row.error_type,
          toolName: row.tool_name,
          rootCause: row.root_cause,
          suggestedFix: row.suggested_fix,
          resolution: row.resolution,
          prevention: row.prevention
        }
      });
    }

    // Query workflows
    const workflows = await withRateLimit(async () => {
      return await this.db.execute({
        sql: `
          SELECT id, description as content, session_id, timestamp,
                 valid_from, valid_to, transaction_from, transaction_to,
                 base_confidence, domain, workflow_type, effectiveness, context, tools_involved, outcome, lessons
          FROM session_workflows
          WHERE (valid_from IS NULL OR valid_from <= ?)
            AND (valid_to IS NULL OR valid_to > ?)
            AND (transaction_from IS NULL OR transaction_from <= ?)
            AND (transaction_to IS NULL OR transaction_to > ?)
            AND (description LIKE '%' || ? || '%' ESCAPE '\\' OR lessons LIKE '%' || ? || '%' ESCAPE '\\')
          ORDER BY timestamp DESC
          LIMIT 20
        `,
        args: [asOfMs, asOfMs, asOfMs, asOfMs, sanitizedQuery, sanitizedQuery]
      });
    }, dbRateLimiter);

    for (const row of workflows.rows) {
      results.push({
        id: row.id as string,
        type: 'workflow',
        content: row.content as string,
        sessionId: row.session_id as string,
        timestamp: row.timestamp as number,
        validFrom: row.valid_from as number | null,
        validTo: row.valid_to as number | null,
        transactionFrom: row.transaction_from as number | null,
        transactionTo: row.transaction_to as number | null,
        baseConfidence: row.base_confidence as number,
        domain: row.domain as string | null,
        metadata: {
          workflowType: row.workflow_type,
          effectiveness: row.effectiveness,
          context: row.context,
          toolsInvolved: row.tools_involved,
          outcome: row.outcome,
          lessons: row.lessons
        }
      });
    }

    return results;
  }

  /**
   * Get the validity period for a specific knowledge item
   *
   * Returns when the knowledge was/is valid in the real world
   * and when we learned about it (transaction time)
   */
  async getValidityPeriod(knowledgeId: string, type: 'decision' | 'learning' | 'error' | 'workflow'): Promise<ValidityPeriod | null> {
    const tableName = `session_${type}s`;

    const result = await this.db.execute({
      sql: `
        SELECT valid_from, valid_to, transaction_from, transaction_to
        FROM ${tableName}
        WHERE id = ?
      `,
      args: [knowledgeId]
    });

    if (result.rows.length === 0) {
      return null;
    }

    const row = result.rows[0];
    return {
      validFrom: new Date(row.valid_from as number),
      validTo: row.valid_to ? new Date(row.valid_to as number) : null,
      transactionFrom: new Date(row.transaction_from as number),
      transactionTo: row.transaction_to ? new Date(row.transaction_to as number) : null
    };
  }

  /**
   * Get all changes that occurred between two dates
   *
   * Detects:
   * - New knowledge added
   * - Knowledge invalidated (valid_to set)
   * - Knowledge superseded (transaction_to set)
   */
  async getChangesBetween(start: Date, end: Date): Promise<Change[]> {
    const startMs = start.getTime();
    const endMs = end.getTime();
    const changes: Change[] = [];

    // Find items added in this period (transaction_from in range)
    const tables: Array<{ name: string; type: 'decision' | 'learning' | 'error' | 'workflow'; contentField: string }> = [
      { name: 'session_decisions', type: 'decision', contentField: 'decision' },
      { name: 'session_learnings', type: 'learning', contentField: 'learning' },
      { name: 'session_errors', type: 'error', contentField: 'error_message' },
      { name: 'session_workflows', type: 'workflow', contentField: 'description' }
    ];

    for (const table of tables) {
      // Added items
      const added = await this.db.execute({
        sql: `
          SELECT id, ${table.contentField} as content, transaction_from as timestamp
          FROM ${table.name}
          WHERE transaction_from >= ? AND transaction_from < ?
          ORDER BY transaction_from DESC
        `,
        args: [startMs, endMs]
      });

      for (const row of added.rows) {
        changes.push({
          knowledgeId: row.id as string,
          type: table.type,
          changeType: 'added',
          timestamp: row.timestamp as number,
          content: row.content as string
        });
      }

      // Invalidated items (valid_to set in this period)
      const invalidated = await this.db.execute({
        sql: `
          SELECT id, ${table.contentField} as content, valid_to as timestamp
          FROM ${table.name}
          WHERE valid_to >= ? AND valid_to < ?
          ORDER BY valid_to DESC
        `,
        args: [startMs, endMs]
      });

      for (const row of invalidated.rows) {
        changes.push({
          knowledgeId: row.id as string,
          type: table.type,
          changeType: 'invalidated',
          timestamp: row.timestamp as number,
          content: row.content as string
        });
      }

      // Modified items (transaction_to set in this period = superseded by new version)
      const modified = await this.db.execute({
        sql: `
          SELECT id, ${table.contentField} as content, transaction_to as timestamp
          FROM ${table.name}
          WHERE transaction_to >= ? AND transaction_to < ?
          ORDER BY transaction_to DESC
        `,
        args: [startMs, endMs]
      });

      for (const row of modified.rows) {
        changes.push({
          knowledgeId: row.id as string,
          type: table.type,
          changeType: 'modified',
          timestamp: row.timestamp as number,
          content: row.content as string
        });
      }
    }

    // Sort all changes by timestamp
    changes.sort((a, b) => b.timestamp - a.timestamp);

    return changes;
  }

  /**
   * Get current knowledge with confidence decay applied
   *
   * Returns knowledge that is currently valid, with confidence
   * adjusted based on age and domain
   */
  async getWithDecay(query: string, domain?: string): Promise<Knowledge[]> {
    // Query current knowledge (valid now, not superseded)
    const now = Date.now();
    const items = await this.queryAtTime(query, new Date(now));

    // Apply confidence decay to each item
    for (const item of items) {
      if (item.domain && item.validFrom) {
        const ageMs = now - item.validFrom;
        item.currentConfidence = this.decay.calculateDecay(
          item.baseConfidence,
          ageMs,
          item.domain
        );
      } else {
        // No domain or no valid_from - use base confidence
        item.currentConfidence = item.baseConfidence;
      }
    }

    // Filter by domain if specified
    if (domain) {
      return items.filter(item => item.domain === domain);
    }

    // Sort by current confidence (highest first)
    items.sort((a, b) => (b.currentConfidence || 0) - (a.currentConfidence || 0));

    return items;
  }

  close() {
    this.db.close();
  }
}
