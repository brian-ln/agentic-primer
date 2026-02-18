#!/usr/bin/env bun
/**
 * migrate-beads.ts
 *
 * Reads beads issues from stdin (JSONL or JSON array) or a file path argument.
 * Transforms each issue to ugs ApplyRecord format.
 * Outputs JSONL to stdout (one record per line).
 *
 * Usage:
 *   cat /Users/bln/.claude/.beads/issues.jsonl | bun run scripts/migrate-beads.ts | ./ugs task apply
 *   bd list --all --json | bun run scripts/migrate-beads.ts | ./ugs task apply
 *   bun run scripts/migrate-beads.ts /path/to/issues.jsonl | ./ugs task apply
 */

import { readFileSync } from 'fs';

// ---- Types ----------------------------------------------------------------

type BeadsStatus =
  | 'open'
  | 'in_progress'
  | 'closed'
  | 'deferred'
  | string;

type UgsLifecycle = 'pending' | 'in_progress' | 'completed' | 'failed';
type UgsPriority = 'P0' | 'P1' | 'P2' | 'P3' | 'P4';

interface BeadsDependency {
  issue_id: string;
  depends_on_id: string;
  type: string;
  created_at?: string;
  created_by?: string;
  metadata?: string;
}

interface BeadsIssue {
  id: string;
  title: string;
  description?: string;
  status?: BeadsStatus;
  priority?: number | null;
  issue_type?: string;
  owner?: string;
  created_at?: string;
  created_by?: string;
  updated_at?: string;
  closed_at?: string;
  close_reason?: string;
  dependencies?: BeadsDependency[];
  dependency_count?: number;
  dependent_count?: number;
  comment_count?: number;
}

interface ApplyRecord {
  id: string;
  title?: string;
  priority?: UgsPriority;
  lifecycle?: UgsLifecycle;
  description?: string;
  'depends-on'?: string[];
}

// ---- Mapping helpers -------------------------------------------------------

function mapStatus(status: BeadsStatus | undefined): UgsLifecycle {
  switch (status) {
    case 'open':
      return 'pending';
    case 'in_progress':
      return 'in_progress';
    case 'closed':
      return 'completed';
    case 'deferred':
      // No direct ugs equivalent — treat as pending; note appended to description
      return 'pending';
    default:
      return 'pending';
  }
}

function mapPriority(priority: number | null | undefined): UgsPriority | undefined {
  if (priority === null || priority === undefined) return undefined;
  const p = Number(priority);
  if (p >= 0 && p <= 4) return `P${p}` as UgsPriority;
  return undefined;
}

function buildDescription(issue: BeadsIssue): string | undefined {
  const parts: string[] = [];

  if (issue.description) {
    parts.push(issue.description);
  }

  // Append extra metadata that has no ugs equivalent
  const notes: string[] = [];

  if (issue.issue_type && issue.issue_type !== 'task') {
    notes.push(`issue_type: ${issue.issue_type}`);
  }

  if (issue.status === 'deferred') {
    notes.push('status: deferred (migrated as pending)');
  }

  if (issue.close_reason) {
    notes.push(`close_reason: ${issue.close_reason}`);
  }

  if (issue.owner) {
    notes.push(`owner: ${issue.owner}`);
  }

  if (issue.created_by) {
    notes.push(`created_by: ${issue.created_by}`);
  }

  if (issue.created_at) {
    notes.push(`created_at: ${issue.created_at}`);
  }

  if (notes.length > 0) {
    parts.push(`\n[Migrated from beads]\n${notes.join('\n')}`);
  }

  return parts.length > 0 ? parts.join('\n\n') : undefined;
}

function extractDependsOn(issue: BeadsIssue): string[] | undefined {
  if (!issue.dependencies || issue.dependencies.length === 0) return undefined;

  const deps = issue.dependencies
    .filter(dep => dep.depends_on_id)
    .map(dep => dep.depends_on_id);

  return deps.length > 0 ? deps : undefined;
}

function transformIssue(issue: BeadsIssue): ApplyRecord {
  const record: ApplyRecord = {
    id: issue.id,
    title: issue.title,
    lifecycle: mapStatus(issue.status),
  };

  const priority = mapPriority(issue.priority);
  if (priority !== undefined) record.priority = priority;

  const description = buildDescription(issue);
  if (description !== undefined) record.description = description;

  const dependsOn = extractDependsOn(issue);
  if (dependsOn !== undefined) record['depends-on'] = dependsOn;

  return record;
}

// ---- Input parsing --------------------------------------------------------

function parseInput(raw: string): BeadsIssue[] {
  const trimmed = raw.trim();
  if (!trimmed) return [];

  // JSON array (from bd list --json / bd list --all --json)
  if (trimmed.startsWith('[')) {
    try {
      const arr = JSON.parse(trimmed);
      if (Array.isArray(arr)) return arr;
    } catch (e) {
      process.stderr.write(`Failed to parse JSON array: ${e}\n`);
      return [];
    }
  }

  // JSONL (one JSON object per line)
  const issues: BeadsIssue[] = [];
  const lines = trimmed.split('\n');
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    if (!line) continue;
    try {
      issues.push(JSON.parse(line));
    } catch (e) {
      process.stderr.write(`Line ${i + 1}: Failed to parse: ${e}\n`);
    }
  }
  return issues;
}

// ---- Main -----------------------------------------------------------------

async function main() {
  let raw: string;

  // If a file path is provided as argument, read from file
  const fileArg = process.argv[2];
  if (fileArg) {
    try {
      raw = readFileSync(fileArg, 'utf-8');
    } catch (e) {
      process.stderr.write(`Cannot read file ${fileArg}: ${e}\n`);
      process.exit(1);
    }
  } else {
    // Read from stdin
    const chunks: string[] = [];
    for await (const chunk of process.stdin) {
      chunks.push(chunk.toString());
    }
    raw = chunks.join('');
  }

  const issues = parseInput(raw);

  if (issues.length === 0) {
    process.stderr.write('No issues found in input.\n');
    process.exit(1);
  }

  process.stderr.write(`Transforming ${issues.length} beads issues...\n`);

  let transformed = 0;
  for (const issue of issues) {
    const record = transformIssue(issue);
    process.stdout.write(JSON.stringify(record) + '\n');
    transformed++;
  }

  process.stderr.write(`Done. ${transformed} records written to stdout.\n`);
}

main().catch(err => {
  process.stderr.write(`Fatal error: ${err}\n`);
  process.exit(1);
});
