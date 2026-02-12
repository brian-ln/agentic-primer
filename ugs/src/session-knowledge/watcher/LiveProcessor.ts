#!/usr/bin/env bun
/**
 * LiveProcessor - Integrate FileWatcher + SessionStats
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { FileWatcher } from './FileWatcher';
import { SessionStats } from './SessionStats';
import { Database } from 'bun:sqlite';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions.db');

export class LiveProcessor {
  private watcher: FileWatcher;
  private stats: SessionStats;
  private db: Database;

  constructor(projectPath: string = process.cwd()) {
    this.stats = new SessionStats();
    this.db = new Database(DB_PATH);

    this.watcher = new FileWatcher(projectPath, {
      onNewEvent: async (event) => {
        const sessionId = this.extractSessionId(event);
        if (sessionId) {
          // Tier 1: Update stats (< 5ms)
          this.stats.processEvent(sessionId, event);

          // Tier 2-4: Queue for later processing
          // TODO: Add embedding queue, decision detector, etc.
        }
      },

      onSessionStart: async (sessionId) => {
        console.log(`\n🆕 Session started: ${sessionId.slice(0, 8)}...`);
      },

      onSessionEnd: async (sessionId) => {
        // Finalize session stats
        const finalStats = this.stats.get(sessionId);
        if (finalStats) {
          console.log(`\n✓ Session ended: ${sessionId.slice(0, 8)}...`);
          console.log(this.stats.format(finalStats));

          // Update database with final stats
          this.updateDatabase(sessionId, finalStats);

          // Clear from memory
          this.stats.clear(sessionId);
        }
      }
    });
  }

  private extractSessionId(event: any): string | null {
    return event.sessionId || null;
  }

  private updateDatabase(sessionId: string, stats: any): void {
    // Update session with real-time calculated stats
    this.db.run(`
      UPDATE sessions
      SET cost = ?, message_count = ?
      WHERE id = ?
    `, [stats.cost, stats.messages, sessionId]);
  }

  async start(): Promise<void> {
    await this.watcher.start();
  }

  stop(): void {
    this.watcher.stop();
    this.db.close();
  }

  /**
   * Get current stats for all active sessions
   */
  getCurrentStats(): any[] {
    return this.stats.getAll().map(stat => ({
      sessionId: stat.sessionId.slice(0, 8),
      cost: `$${stat.cost.toFixed(3)}`,
      messages: stat.messages,
      duration: Math.floor((stat.lastUpdate.getTime() - stat.started.getTime()) / 60000) + 'm',
      cacheHitRate: `${(stat.cacheHitRate * 100).toFixed(1)}%`,
      tools: stat.toolCalls.size,
      files: stat.filesModified.size,
      errors: stat.errors
    }));
  }

  /**
   * Get detailed stats for a specific session
   */
  getSessionStats(sessionId: string): string | null {
    const stat = this.stats.get(sessionId);
    return stat ? this.stats.format(stat) : null;
  }
}

// CLI entry point
if (import.meta.main) {
  const processor = new LiveProcessor();

  console.log('🚀 Starting live session processor...');
  await processor.start();

  // Show stats every 30 seconds
  setInterval(() => {
    const stats = processor.getCurrentStats();
    if (stats.length > 0) {
      console.log('\n📊 Active Sessions:');
      console.table(stats);
    }
  }, 30_000);

  console.log('\nPress Ctrl+C to stop...\n');

  // Graceful shutdown
  process.on('SIGINT', () => {
    console.log('\n\nShutting down...');
    processor.stop();
    process.exit(0);
  });
}
