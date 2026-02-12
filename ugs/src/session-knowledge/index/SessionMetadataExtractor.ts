#!/usr/bin/env bun
/**
 * SessionMetadataExtractor - Extracts session metadata from JSONL logs
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.1
 */

import { Database } from 'bun:sqlite';
import { readdir, readFile, stat } from 'fs/promises';
import { join } from 'path';
import { createHash } from 'crypto';

const INDEX_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(INDEX_DIR, 'sessions.db');

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
  completedAt?: number;
}

export class SessionMetadataExtractor {
  private db: Database;
  private sessionsDir: string;

  private constructor(projectPath: string, db: Database, sessionsDir: string) {
    this.db = db;
    this.sessionsDir = sessionsDir;
  }

  static async create(projectPath: string = process.cwd()): Promise<SessionMetadataExtractor> {
    // Convert project path to sessions directory
    const dirName = projectPath.replace(/\//g, '-').replace(/^-/, '-');
    const sessionsDir = join(process.env.HOME!, '.claude/projects', dirName);

    // Open database
    const db = new Database(DB_PATH);
    db.exec('PRAGMA journal_mode = WAL');
    db.exec('PRAGMA synchronous = NORMAL');

    const builder = new SessionMetadataExtractor(projectPath, db, sessionsDir);
    await builder.initSchema();
    return builder;
  }

  private async initSchema() {
    const schemaPath = join(import.meta.dir, 'schema.sql');
    const schema = await Bun.file(schemaPath).text();
    this.db.exec(schema);
  }

  async indexAllSessions(options: { force?: boolean } = {}): Promise<number> {
    console.log(`📊 Scanning sessions in ${this.sessionsDir}...`);

    const files = await readdir(this.sessionsDir);
    const sessionFiles = files.filter(f =>
      f.endsWith('.jsonl') &&
      !f.includes('subagents') &&
      !f.startsWith('.')
    );

    console.log(`Found ${sessionFiles.length} sessions`);

    let indexed = 0;
    let skipped = 0;

    for (const file of sessionFiles) {
      const sessionId = file.replace('.jsonl', '');
      const needsIndex = options.force || await this.needsIndexing(sessionId);

      if (needsIndex) {
        await this.indexSession(sessionId);
        indexed++;
      } else {
        skipped++;
      }
    }

    // Update metadata
    this.db.run(
      'UPDATE index_metadata SET value = ?, updated_at = ? WHERE key = ?',
      [Date.now().toString(), Date.now(), 'last_full_index']
    );

    console.log(`✓ Indexed: ${indexed}, Skipped: ${skipped} (unchanged)`);
    return indexed;
  }

  private async needsIndexing(sessionId: string): Promise<boolean> {
    const filePath = join(this.sessionsDir, `${sessionId}.jsonl`);

    // Calculate content hash
    const content = await Bun.file(filePath).text();
    const hash = createHash('md5').update(content).digest('hex');

    // Check if already indexed with same hash
    const existing = this.db.query(
      'SELECT content_hash FROM sessions WHERE id = ?'
    ).get(sessionId) as { content_hash: string } | null;

    if (existing && existing.content_hash === hash) {
      return false; // Already indexed, unchanged
    }

    return true;
  }

  async indexSession(sessionId: string): Promise<void> {
    const filePath = join(this.sessionsDir, `${sessionId}.jsonl`);

    try {
      const stats = await stat(filePath);
      const content = await Bun.file(filePath).text();
      const hash = createHash('md5').update(content).digest('hex');

      // Parse events
      const lines = content.trim().split('\n');
      const events = lines
        .filter(l => l.trim())
        .map(line => {
          try {
            return JSON.parse(line);
          } catch {
            return null;
          }
        })
        .filter(Boolean);

      // Extract summary
      const summary = await this.extractSummary(sessionId, events);
      summary.contentHash = hash;

      // Begin transaction
      this.db.run('BEGIN TRANSACTION');

      try {
        // Insert/update session
        this.db.run(`
          INSERT OR REPLACE INTO sessions
          (id, created, modified, summary, message_count, agent_count, cost, duration, git_branch, project_path, content_hash, indexed_at)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        `, [
          sessionId,
          summary.created.getTime(),
          summary.modified.getTime(),
          summary.summary,
          summary.messageCount,
          summary.agents.length,
          summary.cost,
          summary.duration,
          summary.gitBranch || null,
          summary.projectPath || null,
          summary.contentHash,
          Date.now()
        ]);

        // Delete old related data
        this.db.run('DELETE FROM session_files WHERE session_id = ?', [sessionId]);
        this.db.run('DELETE FROM session_tools WHERE session_id = ?', [sessionId]);
        this.db.run('DELETE FROM session_agents WHERE session_id = ?', [sessionId]);

        // Insert files
        for (const [file, operation] of summary.filesModified) {
          this.db.run(
            'INSERT INTO session_files (session_id, file_path, operation, timestamp) VALUES (?, ?, ?, ?)',
            [sessionId, file, operation, summary.modified.getTime()]
          );
        }

        // Insert tools
        for (const [tool, count] of summary.toolsUsed) {
          this.db.run(
            'INSERT INTO session_tools (session_id, tool_name, count) VALUES (?, ?, ?)',
            [sessionId, tool, count]
          );
        }

        // Insert agents
        for (const agent of summary.agents) {
          this.db.run(
            'INSERT INTO session_agents (session_id, agent_id, agent_type, task, outcome, spawned_at, completed_at) VALUES (?, ?, ?, ?, ?, ?, ?)',
            [sessionId, agent.id, agent.type, agent.task, agent.outcome, agent.spawnedAt, agent.completedAt || null]
          );
        }

        this.db.run('COMMIT');
        console.log(`  ✓ ${sessionId.slice(0, 8)} - ${summary.messageCount} msgs`);

      } catch (err) {
        this.db.run('ROLLBACK');
        throw err;
      }

    } catch (err) {
      console.error(`  ✗ ${sessionId}: ${err.message}`);
    }
  }

  private async extractSummary(sessionId: string, events: any[]): Promise<SessionSummary> {
    const messages = new Map();
    const filesModified = new Map<string, string>(); // file_path -> operation
    const toolsUsed = new Map<string, number>();
    const agents: Agent[] = [];
    let totalCost = 0;

    // Deduplicate messages and extract data
    for (const event of events) {
      // Deduplicate by message ID
      const msgId = event.message?.id ?? event.uuid;
      if (!messages.has(msgId) && (event.type === 'user' || event.type === 'assistant')) {
        messages.set(msgId, event);
      }

      // Track tool usage
      if (event.message?.content) {
        for (const block of event.message.content) {
          if (block.type === 'tool_use') {
            toolsUsed.set(block.name, (toolsUsed.get(block.name) || 0) + 1);

            // Track file operations
            if (block.input?.file_path) {
              const operation = block.name.toLowerCase(); // Read->read, Write->write, Edit->edit
              filesModified.set(block.input.file_path, operation);
            }

            // Track agent spawns
            if (block.name === 'Task' && block.input?.subagent_type) {
              // Agent will be extracted from tool result
            }
          }

          // Extract agent info from tool results
          if (block.type === 'tool_result' && block.content) {
            const content = typeof block.content === 'string' ? block.content : JSON.stringify(block.content);
            const agentMatch = content.match(/agentId:\s*([a-f0-9]+)/);
            if (agentMatch) {
              const agentId = agentMatch[1];
              // Find corresponding tool use
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

      // Track cost
      if (event.message?.usage) {
        totalCost += this.calculateCost(event.message.usage);
      }
    }

    // Get timestamps
    const timestamps = events.map(e => new Date(e.timestamp).getTime()).filter(t => !isNaN(t));
    const created = new Date(Math.min(...timestamps));
    const modified = new Date(Math.max(...timestamps));

    // Get summary from sessions-index.json if available
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
      projectPath: events[0]?.cwd,
      contentHash: '' // Set by caller
    };
  }

  private calculateCost(usage: any): number {
    // Claude Code logs do not contain cost data, only token counts.
    // Cost calculation removed per user directive: "The database shouldn't worry about the price unless the cost is on the log record."
    // To calculate costs, multiply token counts by published API rates for the specific model used.
    return 0;
  }

  close() {
    this.db.close();
  }
}

// CLI entry point
if (import.meta.main) {
  const builder = await SessionMetadataExtractor.create();
  const force = process.argv.includes('--force');

  console.time('Indexing');
  const count = await builder.indexAllSessions({ force });
  console.timeEnd('Indexing');

  builder.close();
  console.log(`\n✓ Index complete: ${count} sessions updated`);
}
