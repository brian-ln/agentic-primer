/**
 * SessionStats - Real-time session statistics (Tier 1 processing)
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2 (incremental processing)
 */

export interface Stats {
  sessionId: string;
  cost: number;
  messages: number;
  toolCalls: Map<string, number>;
  filesModified: Set<string>;
  errors: number;
  cacheHitRate: number;
  started: Date;
  lastUpdate: Date;
}

export class SessionStats {
  private stats: Map<string, Stats> = new Map();

  /**
   * Process an event and update stats (< 5ms)
   */
  processEvent(sessionId: string, event: any): void {
    let stat = this.stats.get(sessionId);

    if (!stat) {
      stat = {
        sessionId,
        cost: 0,
        messages: 0,
        toolCalls: new Map(),
        filesModified: new Set(),
        errors: 0,
        cacheHitRate: 0,
        started: new Date(event.timestamp),
        lastUpdate: new Date(event.timestamp)
      };
      this.stats.set(sessionId, stat);
    }

    // Update timestamp
    stat.lastUpdate = new Date(event.timestamp);

    // Count messages
    if (event.type === 'user' || event.type === 'assistant') {
      stat.messages++;
    }

    // Track cost and cache hits
    if (event.message?.usage) {
      stat.cost += this.calculateCost(event.message.usage);
      stat.cacheHitRate = this.calculateCacheHitRate(event.message.usage);
    }

    // Track tools and files
    if (event.message?.content) {
      for (const block of event.message.content) {
        if (block.type === 'tool_use') {
          const count = stat.toolCalls.get(block.name) || 0;
          stat.toolCalls.set(block.name, count + 1);

          // Track file operations
          if (block.input?.file_path) {
            stat.filesModified.add(block.input.file_path);
          }
        }

        // Track errors
        if (block.type === 'tool_result' && block.is_error) {
          stat.errors++;
        }
      }
    }
  }

  private calculateCost(usage: any): number {
    const OPUS_INPUT = 15 / 1_000_000;
    const OPUS_OUTPUT = 75 / 1_000_000;
    const CACHE_WRITE = 18.75 / 1_000_000;
    const CACHE_READ = 1.50 / 1_000_000;

    return (
      (usage.input_tokens || 0) * OPUS_INPUT +
      (usage.output_tokens || 0) * OPUS_OUTPUT +
      (usage.cache_creation_input_tokens || 0) * CACHE_WRITE +
      (usage.cache_read_input_tokens || 0) * CACHE_READ
    );
  }

  private calculateCacheHitRate(usage: any): number {
    const total = (usage.input_tokens || 0) + (usage.cache_read_input_tokens || 0);
    if (total === 0) return 0;
    return (usage.cache_read_input_tokens || 0) / total;
  }

  /**
   * Get stats for a session
   */
  get(sessionId: string): Stats | null {
    return this.stats.get(sessionId) || null;
  }

  /**
   * Get stats for all sessions
   */
  getAll(): Stats[] {
    return Array.from(this.stats.values());
  }

  /**
   * Clear stats for a session
   */
  clear(sessionId: string): void {
    this.stats.delete(sessionId);
  }

  /**
   * Format stats for display
   */
  format(stat: Stats): string {
    const duration = stat.lastUpdate.getTime() - stat.started.getTime();
    const durationMin = Math.floor(duration / 60000);

    return `
Session: ${stat.sessionId.slice(0, 8)}...
Cost:     $${stat.cost.toFixed(3)}
Messages: ${stat.messages}
Duration: ${durationMin} minutes
Cache hit rate: ${(stat.cacheHitRate * 100).toFixed(1)}%
Files modified: ${stat.filesModified.size}
Errors: ${stat.errors}

Tools used:
${Array.from(stat.toolCalls.entries())
  .sort((a, b) => b[1] - a[1])
  .map(([tool, count]) => `  ${tool}: ${count}`)
  .join('\n')}

Files:
${Array.from(stat.filesModified)
  .slice(0, 10)
  .map(f => `  ${f}`)
  .join('\n')}
`.trim();
  }
}
