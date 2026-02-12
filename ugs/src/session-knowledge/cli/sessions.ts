#!/usr/bin/env bun
/**
 * Session query CLI
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.1
 */

import { QueryEngine } from '../index/QueryEngine';

const query = new QueryEngine();
const command = process.argv[2];
const args = process.argv.slice(3);

function formatDate(timestamp: number | Date): string {
  const d = new Date(timestamp);
  return d.toISOString().replace('T', ' ').slice(0, 19);
}

function formatCost(cost: number): string {
  return `$${cost.toFixed(3)}`;
}

try {
  switch (command) {
    case 'yesterday': {
      const sessions = query.yesterday();
      console.log(`\n📅 Yesterday (${sessions.length} sessions)\n`);
      sessions.forEach(s => {
        console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
        console.log(`  ID: ${s.id.slice(0, 8)}... | Messages: ${s.messageCount}\n`);
      });
      break;
    }

    case 'today': {
      const sessions = query.today();
      console.log(`\n📅 Today (${sessions.length} sessions)\n`);
      sessions.forEach(s => {
        console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
        console.log(`  ID: ${s.id.slice(0, 8)}... | Messages: ${s.messageCount}\n`);
      });
      break;
    }

    case 'recent': {
      const limit = parseInt(args[0]) || 10;
      const sessions = query.recent(limit);
      console.log(`\n🕐 Recent ${limit} sessions\n`);
      sessions.forEach(s => {
        console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
        console.log(`  ID: ${s.id.slice(0, 8)}... | Messages: ${s.messageCount}\n`);
      });
      break;
    }

    case 'file': {
      const filePath = args[0];
      if (!filePath) {
        console.error('Usage: sessions file <path>');
        process.exit(1);
      }
      const sessions = query.byFile(filePath);
      console.log(`\n📝 Sessions that modified "${filePath}" (${sessions.length})\n`);
      sessions.forEach(s => {
        console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
        console.log(`  ID: ${s.id.slice(0, 8)}...\n`);
      });
      break;
    }

    case 'search': {
      const searchQuery = args.join(' ');
      if (!searchQuery) {
        console.error('Usage: sessions search <keywords>');
        process.exit(1);
      }
      const sessions = query.search(searchQuery);
      console.log(`\n🔍 Search: "${searchQuery}" (${sessions.length} results)\n`);
      sessions.forEach(s => {
        console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
        console.log(`  ID: ${s.id.slice(0, 8)}...\n`);
      });
      break;
    }

    case 'cost': {
      const days = parseInt(args[0]) || 7;
      const stats = query.costSummary(days);
      console.log(`\n💰 Cost Summary (last ${days} days)\n`);
      console.log(`Sessions:        ${stats.sessionCount}`);
      console.log(`Total cost:      ${formatCost(stats.totalCost)}`);
      console.log(`Average/session: ${formatCost(stats.avgCost)}`);
      console.log(`Total messages:  ${stats.totalMessages}`);
      console.log();
      break;
    }

    case 'tools': {
      const stats = query.toolStats({ limit: 20 });
      console.log(`\n🔧 Tool Usage Statistics\n`);
      stats.forEach(({ tool, uses, sessions }) => {
        console.log(`${tool.padEnd(20)} ${uses.toString().padStart(6)} uses in ${sessions} sessions`);
      });
      console.log();
      break;
    }

    case 'agents': {
      const stats = query.agentStats({ limit: 20 });
      console.log(`\n🤖 Agent Type Statistics\n`);
      stats.forEach(({ type, count }) => {
        console.log(`${type.padEnd(20)} ${count.toString().padStart(6)} spawned`);
      });
      console.log();
      break;
    }

    case 'files': {
      const limit = parseInt(args[0]) || 50;
      const files = query.filesModified(limit);
      console.log(`\n📂 Most Modified Files (top ${limit})\n`);
      files.forEach(({ file, sessions }) => {
        console.log(`${sessions.toString().padStart(4)} sessions | ${file}`);
      });
      console.log();
      break;
    }

    default: {
      console.log(`
Session Knowledge System - Query CLI

Usage:
  sessions yesterday           List yesterday's sessions
  sessions today               List today's sessions
  sessions recent [N]          List recent N sessions (default: 10)
  sessions file <path>         Find sessions that modified file
  sessions search <keywords>   Search sessions by keywords
  sessions cost [days]         Cost summary (default: 7 days)
  sessions tools               Tool usage statistics
  sessions agents              Agent type statistics
  sessions files [N]           Most modified files (default: 50)

Examples:
  sessions yesterday
  sessions recent 20
  sessions file src/auth.ts
  sessions search "authentication bug"
  sessions cost 30
  sessions tools
      `);
    }
  }
} finally {
  query.close();
}
