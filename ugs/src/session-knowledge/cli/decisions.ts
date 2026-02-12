#!/usr/bin/env bun
/**
 * Decisions query CLI
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.1
 */

import { Database } from 'bun:sqlite';
import { join } from 'path';
import { sanitizeLikePattern } from '@agentic-primer/knowledge';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

interface DecisionResult {
  id: string;
  session_id: string;
  timestamp: number;
  decision: string;
  reasoning: string | null;
  alternatives: string | null;
  context: string | null;
}

function formatDate(timestamp: number | Date): string {
  const d = new Date(timestamp);
  return d.toISOString().replace('T', ' ').slice(0, 19);
}

function displayDecision(d: DecisionResult) {
  console.log(`${formatDate(d.timestamp)}`);
  console.log(`  Decision: ${d.decision}`);
  if (d.reasoning) {
    console.log(`  Reasoning: ${d.reasoning}`);
  }
  if (d.alternatives) {
    console.log(`  Alternatives: ${d.alternatives}`);
  }
  if (d.context) {
    console.log(`  Context: ${d.context}`);
  }
  console.log(`  Session: ${d.session_id.slice(0, 8)}... | ID: ${d.id.slice(0, 8)}...\n`);
}

const db = new Database(DB_PATH, { readonly: true });
const command = process.argv[2];
const args = process.argv.slice(3);

try {
  switch (command) {
    case 'today': {
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      const tomorrow = new Date(today);
      tomorrow.setDate(tomorrow.getDate() + 1);

      const decisions = db.query(`
        SELECT *
        FROM session_decisions
        WHERE timestamp >= ? AND timestamp < ?
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(today.getTime(), tomorrow.getTime()) as DecisionResult[];

      console.log(`\n📅 Today's Decisions (${decisions.length})\n`);
      decisions.forEach(displayDecision);
      break;
    }

    case 'yesterday': {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);
      yesterday.setHours(0, 0, 0, 0);
      const tomorrow = new Date(yesterday);
      tomorrow.setDate(tomorrow.getDate() + 1);

      const decisions = db.query(`
        SELECT *
        FROM session_decisions
        WHERE timestamp >= ? AND timestamp < ?
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(yesterday.getTime(), tomorrow.getTime()) as DecisionResult[];

      console.log(`\n📅 Yesterday's Decisions (${decisions.length})\n`);
      decisions.forEach(displayDecision);
      break;
    }

    case 'recent': {
      const limit = parseInt(args[0]) || 10;
      const decisions = db.query(`
        SELECT *
        FROM session_decisions
        ORDER BY timestamp DESC
        LIMIT ?
      `).all(limit) as DecisionResult[];

      console.log(`\n🕐 Recent ${limit} Decisions\n`);
      decisions.forEach(displayDecision);
      break;
    }

    case 'session': {
      const sessionId = args[0];
      if (!sessionId) {
        console.error('Usage: decisions session <session-id>');
        process.exit(1);
      }

      const sanitizedSessionId = sanitizeLikePattern(sessionId);
      const decisions = db.query(`
        SELECT *
        FROM session_decisions
        WHERE session_id LIKE ? ESCAPE '\\'
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(`${sanitizedSessionId}%`) as DecisionResult[];

      console.log(`\n📋 Decisions for Session ${sessionId.slice(0, 8)}... (${decisions.length})\n`);
      decisions.forEach(displayDecision);
      break;
    }

    case 'range': {
      if (args.length < 2) {
        console.error('Usage: decisions range <start-date> <end-date>');
        console.error('Date format: YYYY-MM-DD');
        process.exit(1);
      }

      const startDate = new Date(args[0]);
      const endDate = new Date(args[1]);
      endDate.setHours(23, 59, 59, 999);

      const decisions = db.query(`
        SELECT *
        FROM session_decisions
        WHERE timestamp >= ? AND timestamp <= ?
        ORDER BY timestamp DESC
        LIMIT 500
      `).all(startDate.getTime(), endDate.getTime()) as DecisionResult[];

      console.log(`\n📅 Decisions from ${args[0]} to ${args[1]} (${decisions.length})\n`);
      decisions.forEach(displayDecision);
      break;
    }

    default: {
      console.log(`
Decision Knowledge System - Query CLI

Usage:
  decisions today              List today's decisions
  decisions yesterday          List yesterday's decisions
  decisions recent [N]         List recent N decisions (default: 10)
  decisions session <id>       List decisions for specific session
  decisions range <start> <end> List decisions in date range (YYYY-MM-DD)

Examples:
  decisions today
  decisions recent 20
  decisions session abc12345
  decisions range 2026-01-01 2026-01-31
      `);
    }
  }
} finally {
  db.close();
}
