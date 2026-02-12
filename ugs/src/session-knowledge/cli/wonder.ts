#!/usr/bin/env bun
/**
 * /wonder - Add open question or exploration
 * Epic: Knowledge Architecture Phase 1 MVP
 *
 * Creates a knowledge entry at "wonder" epistemic level (40-60% confidence).
 * Use for open questions, things to explore, or uncertain hypotheses.
 */

import { createClient } from '@libsql/client';
import { join } from 'path';
import { randomUUID } from 'crypto';
import { epistemicLevelToConfidence, type EvidenceLink, serializeEvidence } from '../epistemic-types';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

interface WonderOptions {
  type: 'decision' | 'learning' | 'error';
  text: string;
  context?: string;
  evidence?: string;
  confidence?: number;
  sessionId?: string;
}

function showHelp() {
  console.log(`
/wonder - Add open question or exploration (40-60% confidence)

Usage:
  wonder <type> "<text>" [options]

Types:
  decision  - A decision point that needs exploration
  learning  - A pattern you're noticing but unsure about
  error     - A potential issue that needs investigation

Options:
  --context "<text>"         What triggered this question
  --evidence "<description>" Initial observations (if any)
  --confidence <0.4-0.6>     Specific confidence level (default: 0.50)
  --session <id>             Associate with a specific session

Examples:
  wonder decision "Should we use DiskANN vs HNSW?" --context "Need fast vector search"
  wonder learning "Users might prefer dark mode" --evidence "3 mentions in support"
  wonder error "Possible memory leak?" --context "Memory usage climbing slowly"

Epistemic Level: wonder (40-60% confidence)
- Use for open questions and exploration
- Represents uncertainty and curiosity
- Starting point for investigation
- Can evolve to 'suspect', 'believe', or 'know' as evidence accumulates
- Or be rejected/doubted if evidence contradicts
  `);
}

async function addWonder(options: WonderOptions) {
  const client = createClient({ url: `file:${DB_PATH}` });

  try {
    const id = randomUUID();
    const timestamp = Date.now();
    const sessionId = options.sessionId || process.env.CLAUDE_SESSION_ID || 'manual';
    const confidence = options.confidence ?? epistemicLevelToConfidence('wonder'); // Default: 0.50

    // Validate confidence is in wonder range
    if (confidence < 0.4 || confidence > 0.6) {
      throw new Error(`Confidence ${confidence} is outside wonder range (0.4-0.6). Use /doubt or /suspect instead.`);
    }

    // Build evidence array if provided
    const evidenceLinks: EvidenceLink[] = [];
    if (options.evidence) {
      evidenceLinks.push({
        type: 'SPECULATION',
        description: options.evidence,
        confidence,
        timestamp
      });
    }

    const evidenceJson = evidenceLinks.length > 0 ? serializeEvidence(evidenceLinks) : null;

    switch (options.type) {
      case 'decision': {
        await client.execute({
          sql: `INSERT INTO session_decisions
                (id, session_id, timestamp, decision, reasoning, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'wonder', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.context || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'learning': {
        await client.execute({
          sql: `INSERT INTO session_learnings
                (id, session_id, timestamp, learning, context, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'wonder', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.context || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'error': {
        await client.execute({
          sql: `INSERT INTO session_errors
                (id, session_id, timestamp, error_type, root_cause, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'wonder', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.context || null, confidence, evidenceJson, timestamp]
        });
        break;
      }
    }

    console.log(`✓ Added wonder ${options.type}: ${options.text}`);
    console.log(`  Confidence: ${Math.round(confidence * 100)}%`);
    if (options.context) console.log(`  Context: ${options.context}`);
    if (options.evidence) console.log(`  Evidence: ${options.evidence}`);
    console.log(`  ID: ${id.slice(0, 8)}...`);

  } finally {
    await client.close();
  }
}

function parseArgs(args: string[]): WonderOptions {
  if (args.length < 2) {
    showHelp();
    process.exit(1);
  }

  const type = args[0] as 'decision' | 'learning' | 'error';
  if (!['decision', 'learning', 'error'].includes(type)) {
    console.error(`Invalid type: ${type}. Must be decision, learning, or error.`);
    process.exit(1);
  }

  const text = args[1];
  const options: WonderOptions = { type, text };

  // Parse optional flags
  for (let i = 2; i < args.length; i++) {
    const arg = args[i];

    if (arg === '--context' && i + 1 < args.length) {
      options.context = args[++i];
    } else if (arg === '--evidence' && i + 1 < args.length) {
      options.evidence = args[++i];
    } else if (arg === '--confidence' && i + 1 < args.length) {
      options.confidence = parseFloat(args[++i]);
    } else if (arg === '--session' && i + 1 < args.length) {
      options.sessionId = args[++i];
    }
  }

  return options;
}

const args = process.argv.slice(2);

if (args.length === 0 || args[0] === '--help' || args[0] === '-h') {
  showHelp();
  process.exit(0);
}

try {
  const options = parseArgs(args);
  await addWonder(options);
} catch (err: any) {
  console.error(`Error: ${err.message}`);
  process.exit(1);
}
