#!/usr/bin/env bun
/**
 * Learnings query CLI
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.1
 */

import { Database } from 'bun:sqlite';
import { join } from 'path';
import { sanitizeLikePattern } from '@agentic-primer/knowledge';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

interface LearningResult {
  id: string;
  session_id: string;
  timestamp: number;
  learning: string;
  context: string | null;
  actionable: string | null;
  message_id: string | null;
}

function formatDate(timestamp: number | Date): string {
  const d = new Date(timestamp);
  return d.toISOString().replace('T', ' ').slice(0, 19);
}

function displayLearning(l: LearningResult) {
  console.log(`${formatDate(l.timestamp)}`);
  console.log(`  Learning: ${l.learning}`);
  if (l.context) {
    console.log(`  Context: ${l.context}`);
  }
  if (l.actionable) {
    console.log(`  Actionable: ${l.actionable}`);
  }
  if (l.message_id) {
    console.log(`  Message: ${l.message_id.slice(0, 8)}...`);
  }
  console.log(`  Session: ${l.session_id.slice(0, 8)}... | ID: ${l.id.slice(0, 8)}...\n`);
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

      const learnings = db.query(`
        SELECT *
        FROM session_learnings
        WHERE timestamp >= ? AND timestamp < ?
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(today.getTime(), tomorrow.getTime()) as LearningResult[];

      console.log(`\n📅 Today's Learnings (${learnings.length})\n`);
      learnings.forEach(displayLearning);
      break;
    }

    case 'yesterday': {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);
      yesterday.setHours(0, 0, 0, 0);
      const tomorrow = new Date(yesterday);
      tomorrow.setDate(tomorrow.getDate() + 1);

      const learnings = db.query(`
        SELECT *
        FROM session_learnings
        WHERE timestamp >= ? AND timestamp < ?
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(yesterday.getTime(), tomorrow.getTime()) as LearningResult[];

      console.log(`\n📅 Yesterday's Learnings (${learnings.length})\n`);
      learnings.forEach(displayLearning);
      break;
    }

    case 'recent': {
      const limit = parseInt(args[0]) || 10;
      const learnings = db.query(`
        SELECT *
        FROM session_learnings
        ORDER BY timestamp DESC
        LIMIT ?
      `).all(limit) as LearningResult[];

      console.log(`\n🕐 Recent ${limit} Learnings\n`);
      learnings.forEach(displayLearning);
      break;
    }

    case 'session': {
      const sessionId = args[0];
      if (!sessionId) {
        console.error('Usage: learnings session <session-id>');
        process.exit(1);
      }

      const sanitizedSessionId = sanitizeLikePattern(sessionId);
      const learnings = db.query(`
        SELECT *
        FROM session_learnings
        WHERE session_id LIKE ? ESCAPE '\\'
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(`${sanitizedSessionId}%`) as LearningResult[];

      console.log(`\n📋 Learnings for Session ${sessionId.slice(0, 8)}... (${learnings.length})\n`);
      learnings.forEach(displayLearning);
      break;
    }

    case 'search': {
      const term = args[0];
      if (!term) {
        console.error('Usage: learnings search <term>');
        process.exit(1);
      }

      const sanitizedTerm = sanitizeLikePattern(term);
      const learnings = db.query(`
        SELECT *
        FROM session_learnings
        WHERE learning LIKE ? ESCAPE '\\' OR context LIKE ? ESCAPE '\\' OR actionable LIKE ? ESCAPE '\\'
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(`%${sanitizedTerm}%`, `%${sanitizedTerm}%`, `%${sanitizedTerm}%`) as LearningResult[];

      console.log(`\n🔍 Learnings matching "${term}" (${learnings.length})\n`);
      learnings.forEach(displayLearning);
      break;
    }

    case 'range': {
      if (args.length < 2) {
        console.error('Usage: learnings range <start-date> <end-date>');
        console.error('Date format: YYYY-MM-DD');
        process.exit(1);
      }

      const startDate = new Date(args[0]);
      const endDate = new Date(args[1]);
      endDate.setHours(23, 59, 59, 999);

      const learnings = db.query(`
        SELECT *
        FROM session_learnings
        WHERE timestamp >= ? AND timestamp <= ?
        ORDER BY timestamp DESC
        LIMIT 500
      `).all(startDate.getTime(), endDate.getTime()) as LearningResult[];

      console.log(`\n📅 Learnings from ${args[0]} to ${args[1]} (${learnings.length})\n`);
      learnings.forEach(displayLearning);
      break;
    }

    default: {
      console.log(`
Learning Knowledge System - Query CLI

Usage:
  learnings today              List today's learnings
  learnings yesterday          List yesterday's learnings
  learnings recent [N]         List recent N learnings (default: 10)
  learnings session <id>       List learnings for specific session
  learnings search <term>      Search learnings by text
  learnings range <start> <end> List learnings in date range (YYYY-MM-DD)

Examples:
  learnings today
  learnings recent 20
  learnings session abc12345
  learnings search "vector"
  learnings range 2026-01-01 2026-01-31
      `);
    }
  }
} finally {
  db.close();
}
