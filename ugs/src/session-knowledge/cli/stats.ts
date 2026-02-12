#!/usr/bin/env bun
/**
 * Live stats CLI
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.2
 */

import { readFile } from 'fs/promises';
import { join } from 'path';

const SESSION_DIR = join(
  process.env.HOME!,
  '.claude/projects',
  process.cwd().replace(/\//g, '-').replace(/^-/, '-')
);

// Find current session (modified in last 5 minutes)
async function findCurrentSession(): Promise<string | null> {
  try {
    const { readdir, stat } = await import('fs/promises');
    const files = await readdir(SESSION_DIR);

    for (const file of files) {
      if (!file.endsWith('.jsonl') || file.includes('subagents')) continue;

      const filePath = join(SESSION_DIR, file);
      const stats = await stat(filePath);

      if (Date.now() - stats.mtimeMs < 5 * 60 * 1000) {
        return file.replace('.jsonl', '');
      }
    }
  } catch {}

  return null;
}

// Calculate stats from session file
async function calculateStats(sessionId: string) {
  const filePath = join(SESSION_DIR, `${sessionId}.jsonl`);
  const content = await Bun.file(filePath).text();

  const lines = content.trim().split('\n');
  const events = lines.map(l => {
    try {
      return JSON.parse(l);
    } catch {
      return null;
    }
  }).filter(Boolean);

  // Calculate stats
  let cost = 0;
  let messages = 0;
  const toolCalls = new Map<string, number>();
  const filesModified = new Set<string>();
  let errors = 0;
  let cacheTotal = 0;
  let cacheHits = 0;

  for (const event of events) {
    if (event.type === 'user' || event.type === 'assistant') {
      messages++;
    }

    if (event.message?.usage) {
      const usage = event.message.usage;
      cost += (
        (usage.input_tokens || 0) * 15 / 1_000_000 +
        (usage.output_tokens || 0) * 75 / 1_000_000 +
        (usage.cache_creation_input_tokens || 0) * 18.75 / 1_000_000 +
        (usage.cache_read_input_tokens || 0) * 1.50 / 1_000_000
      );

      cacheTotal += (usage.input_tokens || 0) + (usage.cache_read_input_tokens || 0);
      cacheHits += (usage.cache_read_input_tokens || 0);
    }

    if (event.message?.content) {
      for (const block of event.message.content) {
        if (block.type === 'tool_use') {
          toolCalls.set(block.name, (toolCalls.get(block.name) || 0) + 1);

          if (block.input?.file_path) {
            filesModified.add(block.input.file_path);
          }
        }

        if (block.type === 'tool_result' && block.is_error) {
          errors++;
        }
      }
    }
  }

  const timestamps = events.map(e => new Date(e.timestamp).getTime()).filter(t => !isNaN(t));
  const duration = timestamps.length > 0 ? Math.max(...timestamps) - Math.min(...timestamps) : 0;
  const cacheHitRate = cacheTotal > 0 ? cacheHits / cacheTotal : 0;

  return {
    sessionId,
    cost,
    messages,
    toolCalls,
    filesModified,
    errors,
    duration,
    cacheHitRate
  };
}

// Main
const sessionId = await findCurrentSession();

if (!sessionId) {
  console.log('No active session found (no sessions modified in last 5 minutes)');
  process.exit(0);
}

const stats = await calculateStats(sessionId);

console.log(`
📊 Current Session Stats

Session: ${stats.sessionId.slice(0, 8)}...
Cost:    $${stats.cost.toFixed(3)}
Messages: ${stats.messages}
Duration: ${Math.floor(stats.duration / 60000)} minutes
Cache hit rate: ${(stats.cacheHitRate * 100).toFixed(1)}%
Files modified: ${stats.filesModified.size}
Errors: ${stats.errors}

Tools used:
${Array.from(stats.toolCalls.entries())
  .sort((a, b) => b[1] - a[1])
  .map(([tool, count]) => `  ${tool}: ${count}`)
  .join('\n') || '  (none)'}

Files:
${Array.from(stats.filesModified)
  .slice(0, 10)
  .map(f => `  ${f}`)
  .join('\n') || '  (none)'}
`.trim());
