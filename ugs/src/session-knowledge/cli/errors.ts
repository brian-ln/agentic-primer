#!/usr/bin/env bun
/**
 * Errors query CLI
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.1
 */

import { Database } from 'bun:sqlite';
import { join } from 'path';
import { sanitizeLikePattern } from '@agentic-primer/knowledge';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

interface ErrorResult {
  id: string;
  session_id: string;
  timestamp: number;
  tool_name: string | null;
  error_type: string | null;
  error_message: string | null;
  root_cause: string | null;
  suggested_fix: string | null;
  message_id: string | null;
}

function formatDate(timestamp: number | Date): string {
  const d = new Date(timestamp);
  return d.toISOString().replace('T', ' ').slice(0, 19);
}

function displayError(e: ErrorResult) {
  const errorType = e.error_type || 'unknown';
  const tool = e.tool_name ? ` [${e.tool_name}]` : '';
  console.log(`${formatDate(e.timestamp)} | ${errorType}${tool}`);
  if (e.error_message) {
    console.log(`  Error: ${e.error_message}`);
  }
  if (e.root_cause) {
    console.log(`  Root Cause: ${e.root_cause}`);
  }
  if (e.suggested_fix) {
    console.log(`  Suggested Fix: ${e.suggested_fix}`);
  }
  if (e.message_id) {
    console.log(`  Message: ${e.message_id.slice(0, 8)}...`);
  }
  console.log(`  Session: ${e.session_id.slice(0, 8)}... | ID: ${e.id.slice(0, 8)}...\n`);
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

      const errors = db.query(`
        SELECT *
        FROM session_errors
        WHERE timestamp >= ? AND timestamp < ?
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(today.getTime(), tomorrow.getTime()) as ErrorResult[];

      console.log(`\n📅 Today's Errors (${errors.length})\n`);
      errors.forEach(displayError);
      break;
    }

    case 'yesterday': {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);
      yesterday.setHours(0, 0, 0, 0);
      const tomorrow = new Date(yesterday);
      tomorrow.setDate(tomorrow.getDate() + 1);

      const errors = db.query(`
        SELECT *
        FROM session_errors
        WHERE timestamp >= ? AND timestamp < ?
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(yesterday.getTime(), tomorrow.getTime()) as ErrorResult[];

      console.log(`\n📅 Yesterday's Errors (${errors.length})\n`);
      errors.forEach(displayError);
      break;
    }

    case 'recent': {
      const limit = parseInt(args[0]) || 10;
      const errors = db.query(`
        SELECT *
        FROM session_errors
        ORDER BY timestamp DESC
        LIMIT ?
      `).all(limit) as ErrorResult[];

      console.log(`\n🕐 Recent ${limit} Errors\n`);
      errors.forEach(displayError);
      break;
    }

    case 'session': {
      const sessionId = args[0];
      if (!sessionId) {
        console.error('Usage: errors session <session-id>');
        process.exit(1);
      }

      const sanitizedSessionId = sanitizeLikePattern(sessionId);
      const errors = db.query(`
        SELECT *
        FROM session_errors
        WHERE session_id LIKE ? ESCAPE '\\'
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(`${sanitizedSessionId}%`) as ErrorResult[];

      console.log(`\n📋 Errors for Session ${sessionId.slice(0, 8)}... (${errors.length})\n`);
      errors.forEach(displayError);
      break;
    }

    case 'type': {
      const errorType = args[0];
      if (!errorType) {
        console.error('Usage: errors type <error-type>');
        process.exit(1);
      }

      const errors = db.query(`
        SELECT *
        FROM session_errors
        WHERE error_type = ?
        ORDER BY timestamp DESC
        LIMIT 100
      `).all(errorType) as ErrorResult[];

      console.log(`\n🚨 Errors of Type: ${errorType} (${errors.length})\n`);
      errors.forEach(displayError);
      break;
    }

    case 'types': {
      const stats = db.query(`
        SELECT
          error_type,
          COUNT(*) as count
        FROM session_errors
        WHERE error_type IS NOT NULL
        GROUP BY error_type
        ORDER BY count DESC
      `).all() as Array<{ error_type: string; count: number }>;

      console.log(`\n📊 Error Types\n`);
      stats.forEach(({ error_type, count }) => {
        console.log(`${error_type.padEnd(30)} ${count.toString().padStart(6)} occurrences`);
      });
      console.log();
      break;
    }

    case 'tools': {
      const stats = db.query(`
        SELECT
          tool_name,
          COUNT(*) as count
        FROM session_errors
        WHERE tool_name IS NOT NULL
        GROUP BY tool_name
        ORDER BY count DESC
      `).all() as Array<{ tool_name: string; count: number }>;

      console.log(`\n🔧 Errors by Tool\n`);
      stats.forEach(({ tool_name, count }) => {
        console.log(`${tool_name.padEnd(20)} ${count.toString().padStart(6)} errors`);
      });
      console.log();
      break;
    }

    case 'range': {
      if (args.length < 2) {
        console.error('Usage: errors range <start-date> <end-date>');
        console.error('Date format: YYYY-MM-DD');
        process.exit(1);
      }

      const startDate = new Date(args[0]);
      const endDate = new Date(args[1]);
      endDate.setHours(23, 59, 59, 999);

      const errors = db.query(`
        SELECT *
        FROM session_errors
        WHERE timestamp >= ? AND timestamp <= ?
        ORDER BY timestamp DESC
        LIMIT 500
      `).all(startDate.getTime(), endDate.getTime()) as ErrorResult[];

      console.log(`\n📅 Errors from ${args[0]} to ${args[1]} (${errors.length})\n`);
      errors.forEach(displayError);
      break;
    }

    default: {
      console.log(`
Error Knowledge System - Query CLI

Usage:
  errors today              List today's errors
  errors yesterday          List yesterday's errors
  errors recent [N]         List recent N errors (default: 10)
  errors session <id>       List errors for specific session
  errors type <name>        List errors by type
  errors types              Show all error types with counts
  errors tools              Show errors grouped by tool
  errors range <start> <end> List errors in date range (YYYY-MM-DD)

Examples:
  errors today
  errors recent 20
  errors session abc12345
  errors type "file_not_found"
  errors types
  errors tools
  errors range 2026-01-01 2026-01-31
      `);
    }
  }
} finally {
  db.close();
}
