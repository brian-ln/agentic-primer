#!/usr/bin/env bun
/**
 * SessionEmbeddingIndexerLibSQL - Generates and indexes embeddings for session search
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { createClient, type Client } from '@libsql/client';
import { join } from 'path';
import { EmbeddingGenerator } from './EmbeddingGenerator';
import { VectorStoreLibSQL } from './VectorStoreLibSQL';

const SESSION_DIR_PREFIX = join(process.env.HOME!, '.claude/projects');
const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

export class SessionEmbeddingIndexerLibSQL {
  private client: Client;
  private generator: EmbeddingGenerator;
  private vectorStore: VectorStoreLibSQL;

  constructor() {
    this.client = createClient({ url: `file:${DB_PATH}` });
    this.generator = new EmbeddingGenerator();
    this.vectorStore = new VectorStoreLibSQL(DB_PATH);
  }

  /**
   * Embed all sessions that don't have embeddings yet
   */
  async embedAllSessions(options: { force?: boolean } = {}): Promise<number> {
    const result = await this.client.execute(`
      SELECT id, summary
      FROM sessions
      WHERE ${options.force ? '1=1' : 'summary_embedding IS NULL'}
        AND summary IS NOT NULL
    `);

    const sessions = result.rows as Array<{ id: string; summary: string }>;

    console.log(`\n🔮 Embedding ${sessions.length} sessions...`);

    let count = 0;

    // Process in batches of 25
    for (let i = 0; i < sessions.length; i += 25) {
      const batch = sessions.slice(i, i + 25);
      const summaries = batch.map(s => EmbeddingGenerator.prepareText(s.summary as string, 8000));

      // Generate embeddings
      const embeddings = await this.generator.embedBatch(summaries);

      // Store embeddings
      for (let j = 0; j < batch.length; j++) {
        await this.vectorStore.storeSessionEmbedding(
          batch[j].id as string,
          batch[j].summary as string,
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
    const session = await this.client.execute({
      sql: 'SELECT id, project_path FROM sessions WHERE id = ?',
      args: [sessionId]
    });

    if (!session.rows[0]) {
      throw new Error(`Session ${sessionId} not found`);
    }

    const projectPath = session.rows[0].project_path as string;

    // Load session JSONL
    const dirName = projectPath.replace(/\//g, '-').replace(/^-/, '-');
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
        await this.vectorStore.storeMessageEmbedding(
          sessionId,
          batch[j].id,
          batch[j].content,
          embeddings[j],
          batch[j].timestamp
        );
        count++;
      }

      console.log(`  ✓ ${count}/${uniqueMessages.length} messages embedded`);

      // Small delay between batches to avoid overwhelming the API
      if (i + 25 < uniqueMessages.length) {
        await new Promise(resolve => setTimeout(resolve, 100));
      }
    }

    return count;
  }

  /**
   * Get embedding statistics
   */
  async getStats() {
    return await this.vectorStore.getStats();
  }

  async close() {
    await this.vectorStore.close();
    this.client.close();
  }
}

// CLI entry point
if (import.meta.main) {
  const embedder = new SessionEmbeddingIndexerLibSQL();

  const command = process.argv[2] || 'sessions';

  if (command === 'sessions') {
    const force = process.argv.includes('--force');
    const count = await embedder.embedAllSessions({ force });
    console.log(`\n✓ Embedded ${count} sessions`);
  } else if (command === 'messages') {
    const sessionId = process.argv[3];
    if (!sessionId) {
      console.error('Usage: bun run SessionEmbeddingIndexerLibSQL.ts messages <session-id>');
      process.exit(1);
    }
    const count = await embedder.embedSessionMessages(sessionId);
    console.log(`\n✓ Embedded ${count} messages`);
  } else if (command === 'stats') {
    const stats = await embedder.getStats();
    console.log(`
📊 Embedding Stats

Session embeddings: ${stats.sessionEmbeddings}
Message embeddings: ${stats.messageEmbeddings}
Total size: ${(stats.totalSize / 1024 / 1024).toFixed(2)} MB
Dimensions: ${stats.avgDimensions}
`.trim());
  }

  await embedder.close();
}
