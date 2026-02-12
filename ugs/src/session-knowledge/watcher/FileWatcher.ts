#!/usr/bin/env bun
/**
 * FileWatcher - Real-time session monitoring
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2 (file watching component)
 */

import { watch, type FSWatcher } from 'fs';
import { stat, readdir } from 'fs/promises';
import { join } from 'path';

export interface WatcherOptions {
  onNewEvent?: (event: any) => void | Promise<void>;
  onSessionStart?: (sessionId: string) => void | Promise<void>;
  onSessionEnd?: (sessionId: string) => void | Promise<void>;
  inactiveThreshold?: number; // ms before considering session inactive
}

export class FileWatcher {
  private sessionsDir: string;
  private watchers: Map<string, {
    watcher: FSWatcher;
    lastPosition: number;
    lastActivity: number;
  }> = new Map();

  private options: Required<WatcherOptions>;
  private checkInterval?: Timer;

  constructor(projectPath: string = process.cwd(), options: WatcherOptions = {}) {
    const dirName = projectPath.replace(/\//g, '-').replace(/^-/, '-');
    this.sessionsDir = join(process.env.HOME!, '.claude/projects', dirName);

    this.options = {
      onNewEvent: options.onNewEvent || (() => {}),
      onSessionStart: options.onSessionStart || (() => {}),
      onSessionEnd: options.onSessionEnd || (() => {}),
      inactiveThreshold: options.inactiveThreshold || (30 * 60 * 1000) // 30 minutes
    };
  }

  /**
   * Start watching all active sessions
   */
  async start(): Promise<void> {
    console.log(`👀 Watching sessions in ${this.sessionsDir}...`);

    // Watch for new session files
    await this.watchSessionsDirectory();

    // Watch existing active sessions
    await this.watchActiveSessions();

    // Periodically check for inactive sessions
    this.checkInterval = setInterval(() => {
      this.checkInactiveSessions();
    }, 5 * 60 * 1000); // Every 5 minutes

    console.log(`✓ Watching ${this.watchers.size} active sessions`);
  }

  /**
   * Stop all watchers
   */
  stop(): void {
    console.log('Stopping all watchers...');

    for (const [sessionId, { watcher }] of this.watchers) {
      watcher.close();
    }

    this.watchers.clear();

    if (this.checkInterval) {
      clearInterval(this.checkInterval);
    }

    console.log('✓ All watchers stopped');
  }

  private async watchSessionsDirectory(): Promise<void> {
    // Watch for new .jsonl files being created
    const dirWatcher = watch(this.sessionsDir, async (event, filename) => {
      if (event === 'rename' && filename?.endsWith('.jsonl') && !filename.includes('subagents')) {
        const sessionId = filename.replace('.jsonl', '');

        // Check if this is a new file (not deletion)
        try {
          const filePath = join(this.sessionsDir, filename);
          await stat(filePath);

          // New session detected
          if (!this.watchers.has(sessionId)) {
            await this.options.onSessionStart(sessionId);
            await this.startWatchingSession(sessionId);
          }
        } catch {
          // File was deleted, ignore
        }
      }
    });
  }

  private async watchActiveSessions(): Promise<void> {
    try {
      const files = await readdir(this.sessionsDir);

      for (const file of files) {
        if (!file.endsWith('.jsonl') || file.includes('subagents')) continue;

        const filePath = join(this.sessionsDir, file);
        const stats = await stat(filePath);

        // Consider active if modified in last threshold
        const age = Date.now() - stats.mtimeMs;
        if (age < this.options.inactiveThreshold) {
          const sessionId = file.replace('.jsonl', '');
          await this.startWatchingSession(sessionId);
        }
      }
    } catch (err) {
      console.error('Error watching active sessions:', err);
    }
  }

  private async startWatchingSession(sessionId: string): Promise<void> {
    if (this.watchers.has(sessionId)) {
      return; // Already watching
    }

    const filePath = join(this.sessionsDir, `${sessionId}.jsonl`);

    try {
      // Get current file size
      const stats = await stat(filePath);
      const lastPosition = stats.size;

      // Watch for changes
      const watcher = watch(filePath, async (event) => {
        if (event === 'change') {
          await this.processNewContent(sessionId);
        }
      });

      this.watchers.set(sessionId, {
        watcher,
        lastPosition,
        lastActivity: Date.now()
      });

      console.log(`  ✓ Watching session: ${sessionId.slice(0, 8)}...`);

    } catch (err) {
      console.error(`Error watching session ${sessionId}:`, err);
    }
  }

  private async processNewContent(sessionId: string): Promise<void> {
    const watchInfo = this.watchers.get(sessionId);
    if (!watchInfo) return;

    try {
      const filePath = join(this.sessionsDir, `${sessionId}.jsonl`);
      const content = await Bun.file(filePath).text();

      // Only process content after last position
      const newContent = content.slice(watchInfo.lastPosition);
      watchInfo.lastPosition = content.length;
      watchInfo.lastActivity = Date.now();

      if (!newContent.trim()) return;

      // Parse new lines
      const newLines = newContent.split('\n').filter(l => l.trim());

      for (const line of newLines) {
        try {
          const event = JSON.parse(line);
          await this.options.onNewEvent(event);
        } catch (err) {
          // Skip malformed JSON
        }
      }

    } catch (err) {
      console.error(`Error processing session ${sessionId}:`, err);
    }
  }

  private async checkInactiveSessions(): Promise<void> {
    const now = Date.now();

    for (const [sessionId, { watcher, lastActivity }] of this.watchers) {
      const inactive = now - lastActivity;

      if (inactive > this.options.inactiveThreshold) {
        console.log(`  Session ${sessionId.slice(0, 8)}... inactive, stopping watch`);

        watcher.close();
        this.watchers.delete(sessionId);

        await this.options.onSessionEnd(sessionId);
      }
    }
  }

  /**
   * Get list of currently watched sessions
   */
  getWatchedSessions(): string[] {
    return Array.from(this.watchers.keys());
  }

  /**
   * Get watch info for a session
   */
  getSessionInfo(sessionId: string) {
    const info = this.watchers.get(sessionId);
    if (!info) return null;

    return {
      sessionId,
      lastPosition: info.lastPosition,
      lastActivity: new Date(info.lastActivity),
      inactiveFor: Date.now() - info.lastActivity
    };
  }
}

// CLI entry point
if (import.meta.main) {
  const watcher = new FileWatcher(process.cwd(), {
    onNewEvent: async (event) => {
      console.log(`[${new Date().toISOString()}] ${event.type}: ${event.uuid?.slice(0, 8)}`);
    },
    onSessionStart: async (sessionId) => {
      console.log(`\n🆕 New session started: ${sessionId.slice(0, 8)}...`);
    },
    onSessionEnd: async (sessionId) => {
      console.log(`\n✓ Session ended: ${sessionId.slice(0, 8)}...`);
    }
  });

  await watcher.start();

  // Keep alive
  console.log('\nPress Ctrl+C to stop watching...\n');

  // Handle graceful shutdown
  process.on('SIGINT', () => {
    console.log('\n\nShutting down...');
    watcher.stop();
    process.exit(0);
  });
}
