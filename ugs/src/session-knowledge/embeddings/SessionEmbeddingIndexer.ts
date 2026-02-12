#!/usr/bin/env bun
/**
 * SessionEmbeddingIndexer - Efficient batch processing of embeddings
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { Database } from 'bun:sqlite';
import { join } from 'path';
import { EmbeddingGenerator } from './EmbeddingGenerator';
import { VectorStore } from './VectorStore';

const SESSION_DIR_PREFIX = join(process.env.HOME!, '.claude/projects');
const DB_PATH = join(process.env.HOME!, '.claude/index/sessions.db');

// Set custom SQLite for extension support on macOS
try {
  Database.setCustomSQLite("/opt/homebrew/opt/sqlite/lib/libsqlite3.dylib");
} catch {
  try {
    Database.setCustomSQLite("/usr/local/opt/sqlite/lib/libsqlite3.dylib");
  } catch {}
}

export class SessionEmbeddingIndexer {
  private db: Database;
  private generator: EmbeddingGenerator;
  private vectorStore: VectorStore;

  constructor() {
    this.db = new Database(DB_PATH);
    this.generator = new EmbeddingGenerator();
    this.vectorStore = new VectorStore(this.db);
  }

  /**
   * Initialize - load sqlite-vec extension
   */
  async init(): Promise<void> {
    await this.vectorStore.load();
  }

  /**
   * Embed all sessions that don't have embeddings yet
   */
  async embedAllSessions(options: { force?: boolean } = {}): Promise<number> {
    const sessions = this.db.query(`
      SELECT id, summary
      FROM sessions
      WHERE ${options.force ? '1=1' : 'summary_embedding IS NULL'}
        AND summary IS NOT NULL
    `).all() as Array<{ id: string; summary: string }>;

    console.log(`\n🔮 Embedding ${sessions.length} sessions...`);

    let count = 0;

    // Process in batches of 25
    for (let i = 0; i < sessions.length; i += 25) {
      const batch = sessions.slice(i, i + 25);
      const summaries = batch.map(s => EmbeddingGenerator.prepareText(s.summary, 8000));

      // Generate embeddings
      const embeddings = await this.generator.embedBatch(summaries);

      // Store embeddings
      for (let j = 0; j < batch.length; j++) {
        this.vectorStore.storeSessionEmbedding(
          batch[j].id,
          batch[j].summary,
          embeddings[j]
        );
        count++;
      }

      console.log(`  ✓ ${count}/${sessions.length} sessions embedded`);
    }

    return count;
  }

  /**
   * Embed messages for specific sessions
   */
  async embedSessionMessages(sessionId: string): Promise<number> {
    const session = this.db.query(`
      SELECT id, project_path FROM sessions WHERE id = ?
    `).get(sessionId) as any;

    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }

    // Load session JSONL
    const dirName = session.project_path.replace(/\//g, '-').replace(/^-/, '-');
    const sessionPath = join(SESSION_DIR_PREFIX, dirName, `${sessionId}.jsonl`);
    const content = await Bun.file(sessionPath).text();

    const lines = content.trim().split('\n');
    const events = lines.map(l => {
      try {
        return JSON.parse(l);
      } catch {
        return null;
      }
    }).filter(Boolean);

    // Extract embeddable messages
    const messages: Array<{
      id: string;
      content: string;
      timestamp: number;
    }> = [];

    for (const event of events) {
      const msgId = event.message?.id ?? event.uuid;
      const content = EmbeddingGenerator.extractEmbeddableContent(event);

      if (content && msgId) {
        messages.push({
          id: msgId,
          content: EmbeddingGenerator.prepareText(content, 8000),
          timestamp: new Date(event.timestamp).getTime()
        });
      }
    }

    // Deduplicate by message ID
    const uniqueMessages = Array.from(
      new Map(messages.map(m => [m.id, m])).values()
    );

    console.log(`\n🔮 Embedding ${uniqueMessages.length} messages from session ${sessionId.slice(0, 8)}...`);

    let count = 0;

    // Process in batches of 25
    for (let i = 0; i < uniqueMessages.length; i += 25) {
      const batch = uniqueMessages.slice(i, i + 25);
      const contents = batch.map(m => m.content);

      // Generate embeddings
      const embeddings = await this.generator.embedBatch(contents);

      // Store embeddings
      for (let j = 0; j < batch.length; j++) {
        this.vectorStore.storeMessageEmbedding(
          sessionId,
          batch[j].id,
          batch[j].content,
          embeddings[j],
          batch[j].timestamp
        );
        count++;
      }

      console.log(`  ✓ ${count}/${uniqueMessages.length} messages embedded`);
    }

    return count;
  }

  /**
   * Get embedding statistics
   */
  getStats() {
    return this.vectorStore.getStats();
  }

  close() {
    this.db.close();
  }
}

// CLI entry point
if (import.meta.main) {
  const embedder = new SessionEmbeddingIndexer();
  await embedder.init();

  const command = process.argv[2] || 'sessions';

  if (command === 'sessions') {
    const force = process.argv.includes('--force');
    const count = await embedder.embedAllSessions({ force });
    console.log(`\n✓ Embedded ${count} sessions`);
  } else if (command === 'messages') {
    const sessionId = process.argv[3];
    if (!sessionId) {
      console.error('Usage: bun run SessionEmbeddingIndexer.ts messages <session-id>');
      process.exit(1);
    }
    const count = await embedder.embedSessionMessages(sessionId);
    console.log(`\n✓ Embedded ${count} messages`);
  } else if (command === 'stats') {
    const stats = embedder.getStats();
    console.log(`
📊 Embedding Stats

Session embeddings: ${stats.sessionEmbeddings}
Message embeddings: ${stats.messageEmbeddings}
Total size: ${(stats.totalSize / 1024 / 1024).toFixed(2)} MB
`.trim());
  }

  embedder.close();
}
