#!/usr/bin/env bun
/**
 * /suspect - Add hypothesis with evidence
 * Epic: Knowledge Architecture Phase 1 MVP
 *
 * Creates a knowledge entry at "suspect" epistemic level (60-80% confidence).
 * Use for hypotheses that have some supporting evidence but aren't validated.
 */

import { createClient } from '@libsql/client';
import { join } from 'path';
import { randomUUID } from 'crypto';
import { epistemicLevelToConfidence, type EvidenceLink, serializeEvidence } from '../epistemic-types';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

interface SuspectOptions {
  type: 'decision' | 'learning' | 'error';
  text: string;
  reasoning?: string;
  evidence?: string;
  confidence?: number;
  sessionId?: string;
}

function showHelp() {
  console.log(`
/suspect - Add hypothesis with evidence (60-80% confidence)

Usage:
  suspect <type> "<text>" [options]

Types:
  decision  - A decision or choice that seems right but unvalidated
  learning  - A pattern or insight that appears true based on limited data
  error     - A suspected error or issue that needs investigation

Options:
  --reasoning "<text>"       Why you suspect this is true
  --evidence "<description>" Evidence supporting this hypothesis
  --confidence <0.6-0.8>     Specific confidence level (default: 0.70)
  --session <id>             Associate with a specific session

Examples:
  suspect decision "Batch embeddings reduce API costs" --reasoning "Tested with 100 items" --evidence "Measured 50% reduction"
  suspect learning "Users prefer functional style" --evidence "Observed in 3/5 sessions"
  suspect error "Memory leak in watcher" --confidence 0.65

Epistemic Level: suspect (60-80% confidence)
- Use when you have some evidence but haven't fully validated
- Requires supporting evidence or reasoning
- Can be promoted to 'believe' or 'know' after validation
  `);
}

async function addSuspect(options: SuspectOptions) {
  const client = createClient({ url: `file:${DB_PATH}` });

  try {
    const id = randomUUID();
    const timestamp = Date.now();
    const sessionId = options.sessionId || process.env.CLAUDE_SESSION_ID || 'manual';
    const confidence = options.confidence ?? epistemicLevelToConfidence('suspect'); // Default: 0.70

    // Validate confidence is in suspect range
    if (confidence < 0.6 || confidence > 0.8) {
      throw new Error(`Confidence ${confidence} is outside suspect range (0.6-0.8). Use /believe or /wonder instead.`);
    }

    // Build evidence array if provided
    const evidenceLinks: EvidenceLink[] = [];
    if (options.evidence) {
      evidenceLinks.push({
        type: 'HYPOTHESIS',
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
                VALUES (?, ?, ?, ?, ?, 'suspect', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'learning': {
        await client.execute({
          sql: `INSERT INTO session_learnings
                (id, session_id, timestamp, learning, context, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'suspect', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'error': {
        await client.execute({
          sql: `INSERT INTO session_errors
                (id, session_id, timestamp, error_type, root_cause, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'suspect', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }
    }

    console.log(`✓ Added suspect ${options.type}: ${options.text}`);
    console.log(`  Confidence: ${Math.round(confidence * 100)}%`);
    if (options.reasoning) console.log(`  Reasoning: ${options.reasoning}`);
    if (options.evidence) console.log(`  Evidence: ${options.evidence}`);
    console.log(`  ID: ${id.slice(0, 8)}...`);

  } finally {
    await client.close();
  }
}

function parseArgs(args: string[]): SuspectOptions {
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
  const options: SuspectOptions = { type, text };

  // Parse optional flags
  for (let i = 2; i < args.length; i++) {
    const arg = args[i];

    if (arg === '--reasoning' && i + 1 < args.length) {
      options.reasoning = args[++i];
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
  await addSuspect(options);
} catch (err: any) {
  console.error(`Error: ${err.message}`);
  process.exit(1);
}
