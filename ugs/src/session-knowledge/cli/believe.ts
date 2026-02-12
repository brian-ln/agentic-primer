#!/usr/bin/env bun
/**
 * /believe - Add high-confidence claim
 * Epic: Knowledge Architecture Phase 1 MVP
 *
 * Creates a knowledge entry at "believe" epistemic level (80-95% confidence).
 * Use for claims you're highly confident in but haven't formally validated.
 */

import { createClient } from '@libsql/client';
import { join } from 'path';
import { randomUUID } from 'crypto';
import { epistemicLevelToConfidence, type EvidenceLink, serializeEvidence } from '../epistemic-types';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

interface BelieveOptions {
  type: 'decision' | 'learning' | 'error';
  text: string;
  reasoning?: string;
  evidence?: string;
  confidence?: number;
  sessionId?: string;
}

function showHelp() {
  console.log(`
/believe - Add high-confidence claim (80-95% confidence)

Usage:
  believe <type> "<text>" [options]

Types:
  decision  - A strongly justified decision
  learning  - A pattern or insight backed by substantial evidence
  error     - A confirmed error with clear root cause

Options:
  --reasoning "<text>"       Why you believe this strongly
  --evidence "<description>" Supporting evidence
  --confidence <0.8-0.95>    Specific confidence level (default: 0.875)
  --session <id>             Associate with a specific session

Examples:
  believe decision "TypeScript improves codebase quality" --evidence "6 months production use, 40% fewer runtime errors"
  believe learning "Users need offline mode" --evidence "25 support requests, 3 surveys"
  believe error "Race condition in file watcher" --reasoning "Reproduced 3 times, clear stack traces"

Epistemic Level: believe (80-95% confidence)
- Use for well-supported claims that haven't been formally validated
- Should have substantial evidence or strong reasoning
- Can be promoted to 'know' after validation
- Can be demoted to 'suspect' if evidence weakens
  `);
}

async function addBelieve(options: BelieveOptions) {
  const client = createClient({ url: `file:${DB_PATH}` });

  try {
    const id = randomUUID();
    const timestamp = Date.now();
    const sessionId = options.sessionId || process.env.CLAUDE_SESSION_ID || 'manual';
    const confidence = options.confidence ?? epistemicLevelToConfidence('believe'); // Default: 0.875

    // Validate confidence is in believe range
    if (confidence < 0.8 || confidence > 0.95) {
      throw new Error(`Confidence ${confidence} is outside believe range (0.8-0.95). Use /suspect or /know instead.`);
    }

    // Build evidence array
    const evidenceLinks: EvidenceLink[] = [];
    if (options.evidence) {
      evidenceLinks.push({
        type: 'INFERRED',
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
                VALUES (?, ?, ?, ?, ?, 'believe', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'learning': {
        await client.execute({
          sql: `INSERT INTO session_learnings
                (id, session_id, timestamp, learning, context, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'believe', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }

      case 'error': {
        await client.execute({
          sql: `INSERT INTO session_errors
                (id, session_id, timestamp, error_type, root_cause, epistemic_level, confidence, evidence, last_validated)
                VALUES (?, ?, ?, ?, ?, 'believe', ?, ?, ?)`,
          args: [id, sessionId, timestamp, options.text, options.reasoning || null, confidence, evidenceJson, timestamp]
        });
        break;
      }
    }

    console.log(`✓ Added believe ${options.type}: ${options.text}`);
    console.log(`  Confidence: ${Math.round(confidence * 100)}%`);
    if (options.reasoning) console.log(`  Reasoning: ${options.reasoning}`);
    if (options.evidence) console.log(`  Evidence: ${options.evidence}`);
    console.log(`  ID: ${id.slice(0, 8)}...`);

  } finally {
    await client.close();
  }
}

function parseArgs(args: string[]): BelieveOptions {
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
  const options: BelieveOptions = { type, text };

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
  await addBelieve(options);
} catch (err: any) {
  console.error(`Error: ${err.message}`);
  process.exit(1);
}
