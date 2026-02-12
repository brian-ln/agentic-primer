#!/usr/bin/env bun
/**
 * QueryEngine - Fast session queries
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.1
 */

import { createClient, Client } from '@libsql/client';
import { join } from 'path';
import { sanitizeLikePattern, RateLimiter } from '@agentic-primer/knowledge';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

export interface QueryOptions {
  limit?: number;
  offset?: number;
  projectPath?: string;
}

export interface SessionResult {
  id: string;
  created: Date;
  summary: string;
  cost: number;
  messageCount: number;
  filesModified?: string[];
}

export class QueryEngine {
  private db: Client;
  private rateLimiter: RateLimiter;

  constructor() {
    this.db = createClient({ url: `file:${DB_PATH}` });
    this.rateLimiter = new RateLimiter({
      maxRequestsPerSecond: 50,
      minDelayMs: 20,
      maxConcurrent: 10,
      backoffMultiplier: 1.5,
      maxBackoffMs: 5_000,
    });
  }

  /**
   * Get sessions from yesterday
   */
  async yesterday(options: QueryOptions = {}): Promise<SessionResult[]> {
    await this.rateLimiter.throttle();
    const yesterday = new Date();
    yesterday.setDate(yesterday.getDate() - 1);
    yesterday.setHours(0, 0, 0, 0);

    const tomorrow = new Date(yesterday);
    tomorrow.setDate(tomorrow.getDate() + 1);

    const result = await this.db.execute({
      sql: `
        SELECT id, created, summary, cost, message_count as messageCount
        FROM sessions
        WHERE created >= ? AND created < ?
        ORDER BY created DESC
        LIMIT ?
      `,
      args: [yesterday.getTime(), tomorrow.getTime(), options.limit || 100]
    });
    return result.rows as SessionResult[];
  }

  /**
   * Get sessions from today
   */
  async today(options: QueryOptions = {}): Promise<SessionResult[]> {
    await this.rateLimiter.throttle();
    const today = new Date();
    today.setHours(0, 0, 0, 0);

    const tomorrow = new Date(today);
    tomorrow.setDate(tomorrow.getDate() + 1);

    const result = await this.db.execute({
      sql: `
        SELECT id, created, summary, cost, message_count as messageCount
        FROM sessions
        WHERE created >= ? AND created < ?
        ORDER BY created DESC
        LIMIT ?
      `,
      args: [today.getTime(), tomorrow.getTime(), options.limit || 100]
    });
    return result.rows as SessionResult[];
  }

  /**
   * Get recent sessions
   */
  async recent(limit: number = 10): Promise<SessionResult[]> {
    await this.rateLimiter.throttle();
    const result = await this.db.execute({
      sql: `
        SELECT id, created, summary, cost, message_count as messageCount
        FROM sessions
        ORDER BY created DESC
        LIMIT ?
      `,
      args: [limit]
    });
    return result.rows as SessionResult[];
  }

  /**
   * Find sessions that modified a specific file
   */
  async byFile(filePath: string, options: QueryOptions = {}): Promise<SessionResult[]> {
    await this.rateLimiter.throttle();
    const sanitizedPath = sanitizeLikePattern(filePath);
    const result = await this.db.execute({
      sql: `
        SELECT DISTINCT s.id, s.created, s.summary, s.cost, s.message_count as messageCount
        FROM sessions s
        JOIN session_files sf ON s.id = sf.session_id
        WHERE sf.file_path LIKE ? ESCAPE '\\'
        ORDER BY s.created DESC
        LIMIT ?
      `,
      args: [`%${sanitizedPath}%`, options.limit || 50]
    });
    return result.rows as SessionResult[];
  }

  /**
   * Search sessions by keyword in summary
   */
  async search(query: string, options: QueryOptions = {}): Promise<SessionResult[]> {
    await this.rateLimiter.throttle();
    const sanitizedQuery = sanitizeLikePattern(query);
    const result = await this.db.execute({
      sql: `
        SELECT id, created, summary, cost, message_count as messageCount
        FROM sessions
        WHERE summary LIKE ? ESCAPE '\\'
        ORDER BY created DESC
        LIMIT ?
      `,
      args: [`%${sanitizedQuery}%`, options.limit || 20]
    });
    return result.rows as SessionResult[];
  }

  /**
   * Get sessions within date range
   */
  async dateRange(start: Date, end: Date, options: QueryOptions = {}): Promise<SessionResult[]> {
    await this.rateLimiter.throttle();
    const result = await this.db.execute({
      sql: `
        SELECT id, created, summary, cost, message_count as messageCount
        FROM sessions
        WHERE created >= ? AND created <= ?
        ORDER BY created DESC
        LIMIT ?
      `,
      args: [start.getTime(), end.getTime(), options.limit || 100]
    });
    return result.rows as SessionResult[];
  }

  /**
   * Get cost summary
   */
  async costSummary(days: number = 7): Promise<{
    sessionCount: number;
    totalCost: number;
    avgCost: number;
    totalMessages: number;
  }> {
    const since = Date.now() - (days * 24 * 60 * 60 * 1000);

    const result = await this.db.execute({
      sql: `
        SELECT
          COUNT(*) as sessionCount,
          SUM(cost) as totalCost,
          AVG(cost) as avgCost,
          SUM(message_count) as totalMessages
        FROM sessions
        WHERE created >= ?
      `,
      args: [since]
    });

    const row = result.rows[0] as any;
    return {
      sessionCount: row.sessionCount || 0,
      totalCost: row.totalCost || 0,
      avgCost: row.avgCost || 0,
      totalMessages: row.totalMessages || 0
    };
  }

  /**
   * Get tool usage statistics
   */
  async toolStats(options: QueryOptions = {}): Promise<Array<{ tool: string; uses: number; sessions: number }>> {
    const result = await this.db.execute({
      sql: `
        SELECT
          tool_name as tool,
          SUM(count) as uses,
          COUNT(DISTINCT session_id) as sessions
        FROM session_tools
        GROUP BY tool_name
        ORDER BY uses DESC
        LIMIT ?
      `,
      args: [options.limit || 20]
    });
    return result.rows as any[];
  }

  /**
   * Get agent type statistics
   */
  async agentStats(options: QueryOptions = {}): Promise<Array<{ type: string; count: number }>> {
    const result = await this.db.execute({
      sql: `
        SELECT
          agent_type as type,
          COUNT(*) as count
        FROM session_agents
        WHERE agent_type IS NOT NULL
        GROUP BY agent_type
        ORDER BY count DESC
        LIMIT ?
      `,
      args: [options.limit || 20]
    });
    return result.rows as any[];
  }

  /**
   * Get files modified across sessions
   */
  async filesModified(limit: number = 50): Promise<Array<{ file: string; sessions: number }>> {
    const result = await this.db.execute({
      sql: `
        SELECT
          file_path as file,
          COUNT(DISTINCT session_id) as sessions
        FROM session_files
        GROUP BY file_path
        ORDER BY sessions DESC
        LIMIT ?
      `,
      args: [limit]
    });
    return result.rows as any[];
  }

  close() {
    this.db.close();
  }
}
