#!/usr/bin/env bun
/**
 * /know - Add validated knowledge
 * Epic: Knowledge Architecture Phase 1 MVP
 *
 * Creates a knowledge entry at "know" epistemic level (95-100% confidence).
 * Use for validated, proven knowledge with strong evidence.
 */

import { createClient } from '@libsql/client';
import { join } from 'path';
import { randomUUID } from 'crypto';
import { epistemicLevelToConfidence, type EvidenceLink, serializeEvidence } from '../epistemic-types';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

interface KnowOptions {
  type: 'decision' | 'learning' | 'error';
  text: string;
  reasoning?: string;
  evidence?: string;
  source?: string;
  confidence?: number;
  sessionId?: string;
}

function showHelp() {
  console.log(`
/know - Add validated knowledge (95-100% confidence)

Usage:
  know <type> "<text>" [options]

Types:
  decision  - A proven, validated decision
  learning  - A confirmed pattern or insight
  error     - A fully diagnosed and confirmed error

Options:
  --reasoning "<text>"       Why this is known to be true
  --evidence "<description>" Validation evidence (measurements, tests, etc.)
  --source "<reference>"     Source of validation (file path, URL, experiment)
  --confidence <0.95-1.0>    Specific confidence level (default: 0.975)
  --session <id>             Associate with a specific session

Examples:
  know decision "Batch embeddings save 75% API cost" --evidence "Measured over 1000 sessions" --source "MEASUREMENT_LOG.md"
  know learning "Users abandon checkout after 3 seconds" --evidence "Analytics from 10k sessions" --source "analytics.db"
  know error "Race condition in line 234 of watcher.ts" --evidence "Reproduced 10/10 times with unit test"

Epistemic Level: know (95-100% confidence)
- Use ONLY for validated, proven knowledge
- Requires measurement, testing, or formal validation
- Should include evidence and/or source reference
- Represents the highest certainty level
  `);
}

async function addKnow(options: KnowOptions) {
  const client = createClient({ url: `file:${DB_PATH}` });

  try {
    const id = randomUUID();
    const timestamp = Date.now();
    const sessionId = options.sessionId || process.env.CLAUDE_SESSION_ID || 'manual';
    const confidence = options.confidence ?? epistemicLevelToConfidence('know'); // Default: 0.975

    // Validate confidence is in know range
    if (confidence < 0.95 || confidence > 1.0) {
      throw new Error(`Confidence ${confidence} is outside know range (0.95-1.0). Use /believe instead.`);
    }

    // Build evidence array
    const evidenceLinks: EvidenceLink[] = [];
    if (options.evidence) {
      evidenceLinks.push({
        type: options.source ? 'VALIDATED' : 'MEASURED',
        description: options.evidence,
        source: options.source,
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
                VALUES (?, ?, ?, ?, ?, 'know', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'learning': {
        await client.execute({
          sql: `INSERT INTO session_learnings
                (id, session_id, timestamp, learning, context, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'know', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'error': {
        await client.execute({
          sql: `INSERT INTO session_errors
                (id, session_id, timestamp, error_type, root_cause, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'know', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }
    }

    console.log(`✓ Added know ${options.type}: ${options.text}`);
    console.log(`  Confidence: ${Math.round(confidence * 100)}%`);
    if (options.reasoning) console.log(`  Reasoning: ${options.reasoning}`);
    if (options.evidence) console.log(`  Evidence: ${options.evidence}`);
    if (options.source) console.log(`  Source: ${options.source}`);
    console.log(`  ID: ${id.slice(0, 8)}...`);

  } finally {
    await client.close();
  }
}

function parseArgs(args: string[]): KnowOptions {
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
  const options: KnowOptions = { type, text };

  // Parse optional flags
  for (let i = 2; i < args.length; i++) {
    const arg = args[i];

    if (arg === '--reasoning' && i + 1 < args.length) {
      options.reasoning = args[++i];
    } else if (arg === '--evidence' && i + 1 < args.length) {
      options.evidence = args[++i];
    } else if (arg === '--source' && i + 1 < args.length) {
      options.source = args[++i];
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
  await addKnow(options);
} catch (err: any) {
  console.error(`Error: ${err.message}`);
  process.exit(1);
}
