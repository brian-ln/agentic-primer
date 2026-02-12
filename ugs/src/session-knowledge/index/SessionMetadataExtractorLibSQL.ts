#!/usr/bin/env bun
/**
 * SessionMetadataExtractorLibSQL - Extracts session metadata from JSONL logs using libSQL
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.1
 */

import { createClient, type Client } from '@libsql/client';
import { readdir, stat } from 'fs/promises';
import { join } from 'path';
import { createHash } from 'crypto';

const INDEX_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(INDEX_DIR, 'sessions-libsql.db');

export interface SessionSummary {
  id: string;
  created: Date;
  modified: Date;
  summary: string;
  messageCount: number;
  agentCount: number;
  filesModified: Map<string, string>; // file_path -> operation
  toolsUsed: Map<string, number>;
  agents: Agent[];
  cost: number;
  duration: number;
  gitBranch?: string;
  projectPath?: string;
  contentHash: string;
}

export interface Agent {
  id: string;
  type: string;
  task: string;
  outcome: string;
  spawnedAt: number;
}

export class SessionMetadataExtractorLibSQL {
  private client: Client;
  private sessionsDir: string;
  private projectPath: string;

  private constructor(projectPath: string, client: Client, sessionsDir: string) {
    this.projectPath = projectPath;
    this.client = client;
    this.sessionsDir = sessionsDir;
  }

  static async create(projectPath: string = process.cwd()): Promise<SessionMetadataExtractorLibSQL> {
    const dirName = projectPath.replace(/\//g, '-').replace(/^-/, '-');
    const sessionsDir = join(process.env.HOME!, '.claude/projects', dirName);

    const client = createClient({
      url: `file:${DB_PATH}`
    });

    const builder = new SessionMetadataExtractorLibSQL(projectPath, client, sessionsDir);
    await builder.initSchema();
    return builder;
  }

  private async initSchema(): Promise<void> {
    const schemaPath = join(import.meta.dir, 'schema-libsql.sql');
    const schema = await Bun.file(schemaPath).text();

    // Execute schema as a single batch
    await this.client.batch(schema.split(';').filter(s => s.trim().length > 0), 'write');
  }

  async indexAllSessions(options: { force?: boolean } = {}): Promise<number> {
    const files = await readdir(this.sessionsDir);
    const sessionFiles = files.filter(f => f.endsWith('.jsonl') && !f.includes('subagents'));

    console.log(`📊 Scanning sessions in ${this.sessionsDir}...`);
    console.log(`Found ${sessionFiles.length} sessions`);

    let indexed = 0;
    let skipped = 0;

    for (const file of sessionFiles) {
      const sessionId = file.replace('.jsonl', '');
      await this.indexSession(sessionId, options.force || false);
      indexed++;
    }

    return indexed;
  }

  private async indexSession(sessionId: string, force: boolean): Promise<void> {
    try {
      const filePath = join(this.sessionsDir, `${sessionId}.jsonl`);
      const content = await Bun.file(filePath).text();
      const contentHash = createHash('md5').update(content).digest('hex');

      // Check if already indexed with same content
      if (!force) {
        const existing = await this.client.execute({
          sql: 'SELECT content_hash FROM sessions WHERE id = ?',
          args: [sessionId]
        });

        if (existing.rows[0]?.content_hash === contentHash) {
          return; // Skip unchanged
        }
      }

      // Parse events
      const lines = content.trim().split('\n');
      const events = lines.map(l => {
        try {
          return JSON.parse(l);
        } catch {
          return null;
        }
      }).filter(Boolean);

      const summary = await this.extractSummary(sessionId, events);
      summary.contentHash = contentHash;

      // Use transaction for atomic updates
      await this.client.execute('BEGIN');

      try {
        // Insert/update session
        await this.client.execute({
          sql: `INSERT INTO sessions (id, created, modified, summary, message_count, agent_count,
                                      cost, duration, git_branch, project_path, content_hash, indexed_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(id) DO UPDATE SET
                  modified = excluded.modified,
                  summary = excluded.summary,
                  message_count = excluded.message_count,
                  agent_count = excluded.agent_count,
                  cost = excluded.cost,
                  duration = excluded.duration,
                  git_branch = excluded.git_branch,
                  project_path = excluded.project_path,
                  content_hash = excluded.content_hash,
                  indexed_at = excluded.indexed_at`,
          args: [
            sessionId,
            summary.created.getTime(),
            summary.modified.getTime(),
            summary.summary,
            summary.messageCount,
            summary.agentCount,
            summary.cost,
            summary.duration,
            summary.gitBranch || null,
            summary.projectPath || null,
            summary.contentHash,
            Date.now()
          ]
        });

        // Delete and reinsert related data
        await this.client.execute({ sql: 'DELETE FROM session_files WHERE session_id = ?', args: [sessionId] });
        await this.client.execute({ sql: 'DELETE FROM session_tools WHERE session_id = ?', args: [sessionId] });
        await this.client.execute({ sql: 'DELETE FROM session_agents WHERE session_id = ?', args: [sessionId] });

        // Insert files
        for (const [file, operation] of summary.filesModified) {
          await this.client.execute({
            sql: 'INSERT INTO session_files (session_id, file_path, operation, timestamp) VALUES (?, ?, ?, ?)',
            args: [sessionId, file, operation, summary.modified.getTime()]
          });
        }

        // Insert tools
        for (const [tool, count] of summary.toolsUsed) {
          await this.client.execute({
            sql: 'INSERT INTO session_tools (session_id, tool_name, count) VALUES (?, ?, ?)',
            args: [sessionId, tool, count]
          });
        }

        // Insert agents
        for (const agent of summary.agents) {
          await this.client.execute({
            sql: 'INSERT INTO session_agents (session_id, agent_id, agent_type, task, outcome, spawned_at) VALUES (?, ?, ?, ?, ?, ?)',
            args: [sessionId, agent.id, agent.type, agent.task, agent.outcome, agent.spawnedAt]
          });
        }

        await this.client.execute('COMMIT');

        console.log(`  ✓ ${sessionId.slice(0, 8)} - ${summary.messageCount} msgs`);
      } catch (err) {
        await this.client.execute('ROLLBACK');
        throw err;
      }

    } catch (err) {
      console.error(`  ✗ ${sessionId}: ${err.message}`);
    }
  }

  private async extractSummary(sessionId: string, events: any[]): Promise<SessionSummary> {
    const messages = new Map();
    const filesModified = new Map<string, string>();
    const toolsUsed = new Map<string, number>();
    const agents: Agent[] = [];
    let totalCost = 0;

    for (const event of events) {
      const msgId = event.message?.id ?? event.uuid;
      if (!messages.has(msgId) && (event.type === 'user' || event.type === 'assistant')) {
        messages.set(msgId, event);
      }

      if (event.message?.content) {
        for (const block of event.message.content) {
          if (block.type === 'tool_use') {
            toolsUsed.set(block.name, (toolsUsed.get(block.name) || 0) + 1);

            if (block.input?.file_path) {
              const operation = block.name.toLowerCase();
              filesModified.set(block.input.file_path, operation);
            }

            if (block.name === 'Task' && block.input?.subagent_type) {
              // Agent will be extracted from tool result
            }
          }

          if (block.type === 'tool_result' && block.content) {
            const content = typeof block.content === 'string' ? block.content : JSON.stringify(block.content);
            const agentMatch = content.match(/agentId:\s*([a-f0-9]+)/);
            if (agentMatch) {
              const agentId = agentMatch[1];
              const toolUse = event.message.content.find(b => b.type === 'tool_use' && b.name === 'Task');
              if (toolUse) {
                agents.push({
                  id: agentId,
                  type: toolUse.input.subagent_type || 'unknown',
                  task: toolUse.input.description || toolUse.input.prompt || '',
                  outcome: 'success',
                  spawnedAt: new Date(event.timestamp).getTime()
                });
              }
            }
          }
        }
      }

      if (event.message?.usage) {
        totalCost += this.calculateCost(event.message.usage);
      }
    }

    const timestamps = events.map(e => new Date(e.timestamp).getTime()).filter(t => !isNaN(t));
    const created = new Date(Math.min(...timestamps));
    const modified = new Date(Math.max(...timestamps));

    let summary = `Session ${sessionId.slice(0, 8)}`;
    try {
      const indexPath = join(this.sessionsDir, 'sessions-index.json');
      const indexData = JSON.parse(await Bun.file(indexPath).text());
      const entry = indexData.entries?.find((e: any) => e.sessionId === sessionId);
      if (entry?.summary) {
        summary = entry.summary;
      }
    } catch {}

    return {
      id: sessionId,
      created,
      modified,
      summary,
      messageCount: messages.size,
      agentCount: agents.length,
      filesModified,
      toolsUsed,
      agents,
      cost: totalCost,
      duration: modified.getTime() - created.getTime(),
      gitBranch: events[0]?.gitBranch,
      projectPath: this.projectPath,
      contentHash: ''
    };
  }

  private calculateCost(usage: any): number {
    // Claude Code logs do not contain cost data, only token counts.
    // Cost calculation removed per user directive: "The database shouldn't worry about the price unless the cost is on the log record."
    // To calculate costs, multiply token counts by published API rates for the specific model used.
    return 0;
  }

  async close() {
    this.client.close();
  }
}

// CLI entry point
if (import.meta.main) {
  const builder = await SessionMetadataExtractorLibSQL.create();
  const force = process.argv.includes('--force');

  console.time('Indexing');
  const count = await builder.indexAllSessions({ force });
  console.timeEnd('Indexing');

  await builder.close();
  console.log(`\n✓ Index complete: ${count} sessions updated`);
}
