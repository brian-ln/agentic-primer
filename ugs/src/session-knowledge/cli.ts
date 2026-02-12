#!/usr/bin/env bun
/**
 * Session Knowledge System - Unified CLI
 * Epic: agentic-primer-9ad
 *
 * Single entry point for all knowledge system commands
 */

import { PrototypeGenerator } from './classification/PrototypeGenerator';
import { KnowledgeExtractor } from './extraction/KnowledgeExtractor';
import { QueryEngine } from './index/QueryEngine';
import { EmbeddingGenerator } from './embeddings/EmbeddingGenerator';
import { SessionMetadataExtractorLibSQL } from './index/SessionMetadataExtractorLibSQL';
import { SessionEmbeddingIndexerLibSQL } from './embeddings/SessionEmbeddingIndexerLibSQL';
import { TemporalQueries } from './temporal/TemporalQueries';
import { ArcDetector } from './temporal/ArcDetector';
import { createClient } from '@libsql/client';
import { join, resolve } from 'path';
import {
  ConfidenceDecay,
  validateLength,
  validateSessionId,
  validateDateString,
  sanitizeErrorMessage,
  sanitizeStackTrace,
  INPUT_LIMITS,
} from '@agentic-primer/knowledge';
import { validateProjectName } from './security/input-validation';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');

const COMMANDS = {
  index: 'Index session metadata (step 1 of 3)',
  embed: 'Generate embeddings (step 2 of 3)',
  prototypes: 'Manage prototype embeddings',
  extract: 'Extract knowledge from sessions (step 3 of 3)',
  add: 'Manually add knowledge (decisions, learnings, errors)',
  search: 'Semantic search across all knowledge',
  decisions: 'Query extracted decisions',
  learnings: 'Query extracted learnings',
  errors: 'Query extracted errors',
  workflows: 'Query work organization patterns',
  sessions: 'Query session metadata',
  stats: 'Show session statistics',
  discover: 'Discover available knowledge in the system',
  temporal: 'Query knowledge at a specific point in time',
  decay: 'Show confidence decay for knowledge items',
  arcs: 'Detect thinking arcs in sessions',
  relationships: 'Query knowledge relationships'
};

// Output mode detection
function getOutputMode(args: string[]): 'human' | 'json' {
  // Explicit --json flag
  if (args.includes('--json')) return 'json';

  // Auto-detect: if output is piped (not a TTY), use JSON
  if (!Bun.stdout.isTTY) return 'json';

  // Default to human-friendly
  return 'human';
}

// Strip --json flag from args
function stripJsonFlag(args: string[]): string[] {
  return args.filter(a => a !== '--json');
}

function showHelp() {
  console.log(`
Session Knowledge System - Unified CLI

Usage:
  know <command> [args] [--json]

Commands:
  index [--force]                       ${COMMANDS.index}
  embed [session-id|all] [--force]      ${COMMANDS.embed}
  prototypes [list|delete <cat>|clear]  ${COMMANDS.prototypes}
  extract <id|all|today|yesterday|since DATE> ${COMMANDS.extract}
  add <type> "<text>" [options]         ${COMMANDS.add}
  search "<query>" [category]           ${COMMANDS.search}
  decisions [filters...]                ${COMMANDS.decisions}
  learnings [filters...]                ${COMMANDS.learnings}
  errors [filters...]                   ${COMMANDS.errors}
  workflows [filters...]                ${COMMANDS.workflows}
  sessions [filters...]                 ${COMMANDS.sessions}
  stats [filters...]                    ${COMMANDS.stats}
  discover                              ${COMMANDS.discover}
  temporal "<query>" --as-of=DATE       ${COMMANDS.temporal}
  decay [--domain=<domain>] [--show]    ${COMMANDS.decay}
  arcs <session-id>                     ${COMMANDS.arcs}
  relationships <knowledge-id>          ${COMMANDS.relationships}

Flags:
  --json     Output machine-readable JSON (auto-enabled when piped)

Examples:
  know index                    # Index all sessions (step 1)
  know embed all                # Generate embeddings (step 2)
  know prototypes               # Generate prototype embeddings
  know extract all              # Extract knowledge (step 3)
  know add decision "Always use bun.js" --reasoning "Faster than Node"
  know search "prototype theory" --json
  know search "vector database" decisions
  know decisions today --json
  know learnings category technical | jq '.results'
  know errors type NetworkError
  know sessions yesterday
  know stats

For command-specific help:
  know <command> --help
  `);
}

function showPrototypesHelp() {
  console.log(`
Prototype Embeddings Management

Usage:
  know prototypes [command]

Commands:
  list              List all prototype embeddings (default)
  delete <category> Delete specific category prototype
  clear             Delete all prototypes

Examples:
  know prototypes
  know prototypes list
  know prototypes delete decision
  know prototypes clear

Description:
  Prototypes are averaged embeddings from curated examples that represent
  each knowledge category (decisions, learnings, errors). They're used in
  Stage 1 candidate detection via cosine similarity.

  Run this once before extraction:
  /generate-prototypes
  `);
}

async function runIndex(args: string[]) {
  const force = args.includes('--force');

  console.log('\n📊 Step 1/3: Indexing session metadata...\n');

  try {
    const extractor = await SessionMetadataExtractorLibSQL.create();
    const count = await extractor.indexAllSessions({ force });
    console.log(`\n✓ Indexed ${count} sessions`);
    console.log('\nNext step: know embed all');
  } catch (error) {
    console.error('Error:', sanitizeErrorMessage(error));
    if (process.env.NODE_ENV !== 'production' && error instanceof Error) {
      console.error('Stack:', sanitizeStackTrace(error.stack));
    }
    process.exit(1);
  }
}

async function runEmbed(args: string[]) {
  const target = args[0] || 'all';
  const force = args.includes('--force');

  console.log('\n🔮 Step 2/3: Generating embeddings...\n');

  try {
    const indexer = new SessionEmbeddingIndexerLibSQL();

    if (target === 'all') {
      // First embed session summaries
      const sessionCount = await indexer.embedAllSessions({ force });
      console.log(`✓ Embedded ${sessionCount} session summaries`);

      // Then embed messages for all sessions
      const client = createClient({ url: `file:${DB_PATH}` });
      const result = await client.execute('SELECT id FROM sessions');
      const sessionIds = result.rows.map(r => r.id as string);

      let totalMessages = 0;
      for (let i = 0; i < sessionIds.length; i++) {
        const sessionId = sessionIds[i];
        try {
          const msgCount = await indexer.embedSessionMessages(sessionId);
          totalMessages += msgCount;
          console.log(`✓ Session ${i+1}/${sessionIds.length} (${sessionId.slice(0, 8)}): ${msgCount} messages`);
        } catch (error) {
          console.log(`⚠️  Session ${i+1}/${sessionIds.length} (${sessionId.slice(0, 8)}): ${sanitizeErrorMessage(error)}`);
        }

        // Delay between sessions to avoid overwhelming API
        if (i < sessionIds.length - 1) {
          await new Promise(resolve => setTimeout(resolve, 200));
        }
      }

      console.log(`\n✓ Total: ${totalMessages} messages embedded across ${sessionIds.length} sessions`);
      await client.close();
    } else {
      validateSessionId(target);
      const msgCount = await indexer.embedSessionMessages(target);
      console.log(`✓ Embedded ${msgCount} messages from session ${target.slice(0, 8)}`);
    }

    console.log('\nNext step: know extract all');
  } catch (error) {
    console.error('Error:', sanitizeErrorMessage(error));
    if (process.env.NODE_ENV !== 'production' && error instanceof Error) {
      console.error('Stack:', sanitizeStackTrace(error.stack));
    }
    process.exit(1);
  }
}

async function runPrototypes(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showPrototypesHelp();
    return;
  }

  const generator = new PrototypeGenerator();

  try {
    const command = args[0];

    switch (command) {
      case 'list': {
        const prototypes = await generator.listPrototypes();
        console.log('\n📋 Prototype Embeddings\n');
        prototypes.forEach(p => {
          console.log(`  ${p.category.padEnd(10)} ${p.exampleCount} examples, updated ${p.updatedAt.toISOString()}`);
        });
        console.log();
        break;
      }

      case 'delete': {
        const category = args[1];
        if (!category) {
          console.error('Usage: know prototypes delete <category>');
          process.exit(1);
        }
        await generator.deletePrototype(category);
        console.log(`✓ Deleted ${category} prototype`);
        break;
      }

      case 'clear': {
        await generator.deleteAll();
        console.log('✓ Deleted all prototypes');
        break;
      }

      default: {
        // Generate all prototypes
        await generator.generateAll();
        const prototypes = await generator.listPrototypes();
        console.log('Stored prototypes:');
        prototypes.forEach(p => {
          console.log(`  ✓ ${p.category}: ${p.exampleCount} examples`);
        });
      }
    }
  } finally {
    await generator.close();
  }
}

function showExtractHelp() {
  console.log(`
Knowledge Extraction from Sessions

Usage:
  know extract <target>

Targets:
  <session-id>  Extract from specific session (full UUID)
  all           Process all unprocessed sessions
  today         Process today's sessions only
  yesterday     Process yesterday's sessions only
  since DATE    Process sessions since DATE (YYYY-MM-DD)

Examples:
  know extract f03b3b54-ca47-46d3-be1f-20ccfc82f9de
  know extract all
  know extract today
  know extract since 2026-02-01

Description:
  Two-stage semantic classification pipeline:

  Stage 1: Candidate Detection (fast, embedding-based)
  - Cosine similarity to prototype embeddings
  - Threshold: 0.65
  - Filters ~90% of messages

  Stage 2: LLM Classification (accurate, structured extraction)
  - Model: @cf/meta/llama-3.2-3b-instruct
  - Extracts decisions, learnings, errors with metadata
  - Stores with confidence scores

Prerequisites:
  1. Message embeddings must exist
  2. Prototype embeddings generated (/generate-prototypes)
  3. Cloudflare credentials in .env

Cost: ~$0.024 per session (200 messages)
  `);
}

async function runExtract(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showExtractHelp();
    return;
  }

  const extractor = new KnowledgeExtractor();

  try {
    const target = args[0];

    if (!target) {
      console.error('Usage: know extract <session-id|all|today|yesterday|since DATE>');
      process.exit(1);
    }

    if (target === 'all') {
      const count = await extractor.extractAll();
      console.log(`\n✓ Processed all sessions, extracted ${count} items`);
    } else if (target === 'today' || target === 'yesterday') {
      const result = await extractor.extractRange(
        target === 'today' ? new Date() : new Date(Date.now() - 86400000),
        new Date()
      );
      console.log(`\n✓ Processed ${target}'s sessions, extracted ${result} items`);
    } else if (target === 'since') {
      const dateArg = args[1];
      if (!dateArg) {
        console.error('Usage: know extract since <YYYY-MM-DD>');
        process.exit(1);
      }
      validateDateString(dateArg);
      const sinceDate = new Date(dateArg);
      if (isNaN(sinceDate.getTime())) {
        console.error('Invalid date format. Use YYYY-MM-DD');
        process.exit(1);
      }
      const result = await extractor.extractRange(sinceDate, new Date());
      console.log(`\n✓ Processed sessions since ${dateArg}, extracted ${result} items`);
    } else {
      // Validate session ID format
      validateSessionId(target);
      const result = await extractor.extractSession(target);
      console.log(`\n✓ Extracted: ${result.decisionsExtracted} decisions, ${result.learningsExtracted} learnings, ${result.errorsExtracted} errors`);
    }
  } catch (error) {
    console.error('Error:', sanitizeErrorMessage(error));
    if (process.env.NODE_ENV !== 'production' && error instanceof Error) {
      console.error('Stack:', sanitizeStackTrace(error.stack));
    }
    process.exit(1);
  } finally {
    await extractor.close();
  }
}

function showAddHelp() {
  console.log(`
Manually Add Knowledge

Usage:
  know add <type> "<text>" [options]

Types:
  decision   Add an architectural or technical decision
  learning   Add a technical insight or discovery
  error      Add a known error and its solution

Options:
  --reasoning "<text>"      Why this decision was made (decisions only)
  --alternatives "<text>"   Other options considered (decisions only)
  --context "<text>"        Additional context (decisions, learnings)
  --actionable "<text>"     How this can be applied (learnings only)
  --type "<ErrorType>"      Error type: NetworkError, TypeError, etc. (errors only)
  --tool "<name>"           Tool that generated error (errors only)
  --root-cause "<text>"     Why the error occurred (errors only)
  --suggested-fix "<text>"  How to fix the error (errors only)

Examples:
  # Add a decision
  know add decision "Always use bun.js for JavaScript runtime" \\
    --reasoning "Faster than Node, TypeScript native" \\
    --alternatives "Node.js (slower), Deno (less ecosystem)"

  # Add a learning
  know add learning "libSQL has native vector support" \\
    --context "Discovered while evaluating vector databases" \\
    --actionable "Use F32_BLOB(768) for embeddings"

  # Add an error solution
  know add error "Import path resolution fails in Bun" \\
    --type "ImportError" \\
    --root-cause "Bun requires explicit file extensions" \\
    --suggested-fix "Add .ts/.js to all import paths"

Description:
  Manually add knowledge to the system without extracting from sessions.
  Useful for team conventions, external decisions, or seeding the knowledge base.

  All manually added entries are marked with source="manual" and confidence=1.0.
  `);
}

async function runAdd(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showAddHelp();
    return;
  }

  const db = createClient({ url: `file:${DB_PATH}` });

  try {
    // Get the current session ID from the filesystem (most recently modified .jsonl file)
    let sessionId = 'manual'; // Fallback if we can't determine current session

    try {
      // Derive project directory from current working directory
      const cwd = process.cwd();
      const homeDir = process.env.HOME || '';

      // Validate and sanitize project name to prevent directory traversal
      const projectName = validateProjectName(cwd);
      const projectDir = resolve(homeDir, '.claude/projects', projectName);

      // Find most recently modified .jsonl file
      const { readdirSync, statSync } = await import('fs');
      const files = readdirSync(projectDir)
        .filter(f => f.endsWith('.jsonl'))
        .map(f => ({ name: f, mtime: statSync(join(projectDir, f)).mtime }))
        .sort((a, b) => b.mtime.getTime() - a.mtime.getTime());

      if (files.length > 0) {
        // Extract session ID from filename (remove .jsonl extension)
        sessionId = files[0].name.replace('.jsonl', '');
      }
    } catch (err) {
      // If we can't determine the current session, fall back to querying database
      const currentSessionResult = await db.execute({
        sql: `SELECT id FROM sessions
              WHERE id != 'manual'
              ORDER BY modified DESC
              LIMIT 1`
      });

      if (currentSessionResult.rows.length > 0) {
        sessionId = currentSessionResult.rows[0].id as string;
      } else {
        // No sessions exist - create the manual session as fallback
        await db.execute({
          sql: `INSERT INTO sessions (id, created, modified, summary, message_count, agent_count, cost, duration)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
          args: ['manual', Date.now(), Date.now(), 'Manually added knowledge entries', 0, 0, 0.0, 0]
        });
      }
    }

    const type = args[0];
    if (!type || !['decision', 'learning', 'error'].includes(type)) {
      console.error('Usage: know add <decision|learning|error> "<text>" [options]');
      console.error('Run "know add --help" for more information');
      process.exit(1);
    }

    // Find the main text (first non-flag argument after type)
    let text = '';
    for (let i = 1; i < args.length; i++) {
      if (!args[i].startsWith('--')) {
        text = args[i];
        break;
      }
    }

    if (!text) {
      console.error(`Error: No text provided for ${type}`);
      console.error(`Example: know add ${type} "Your ${type} text here"`);
      process.exit(1);
    }

    // Validate input length
    text = validateLength(text, INPUT_LIMITS.MAX_TEXT_LENGTH, `${type} text`);

    // Parse optional flags
    function getFlag(name: string): string | null {
      const idx = args.indexOf(`--${name}`);
      if (idx === -1 || idx + 1 >= args.length) return null;
      return args[idx + 1];
    }

    // Ensure session exists in database (create if needed)
    const checkSession = await db.execute({
      sql: 'SELECT id FROM sessions WHERE id = ?',
      args: [sessionId]
    });

    if (checkSession.rows.length === 0) {
      // Create session entry for current session
      await db.execute({
        sql: `INSERT INTO sessions (id, created, modified, summary, message_count, agent_count, cost, duration)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
        args: [sessionId, Date.now(), Date.now(), `Session ${sessionId.slice(0, 8)}`, 0, 0, 0.0, 0]
      });
    }

    const timestamp = Date.now();
    const id = `${type.slice(0, 3)}_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`;
    // Use special message_id prefix to mark as manually entered
    const messageId = `manual_${timestamp}_${Math.random().toString(36).slice(2, 9)}`;

    if (type === 'decision') {
      const reasoning = getFlag('reasoning') || null;
      const alternatives = getFlag('alternatives') || null;
      const context = getFlag('context') || null;

      await db.execute({
        sql: `INSERT INTO session_decisions (id, session_id, message_id, timestamp, decision, reasoning, alternatives, context)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
        args: [id, sessionId, messageId, timestamp, text, reasoning, alternatives, context]
      });

      console.log(`✓ Added decision: ${text}`);
      if (reasoning) console.log(`  Reasoning: ${reasoning}`);
      if (alternatives) console.log(`  Alternatives: ${alternatives}`);
      console.log(`  Session: ${sessionId.slice(0, 8)}... | ID: ${id}`);

    } else if (type === 'learning') {
      const context = getFlag('context') || null;
      const actionable = getFlag('actionable') || null;

      await db.execute({
        sql: `INSERT INTO session_learnings (id, session_id, message_id, timestamp, learning, context, actionable)
              VALUES (?, ?, ?, ?, ?, ?, ?)`,
        args: [id, sessionId, messageId, timestamp, text, context, actionable]
      });

      console.log(`✓ Added learning: ${text}`);
      if (context) console.log(`  Context: ${context}`);
      if (actionable) console.log(`  Actionable: ${actionable}`);
      console.log(`  Session: ${sessionId.slice(0, 8)}... | ID: ${id}`);

    } else if (type === 'error') {
      const errorType = getFlag('type') || null;
      const tool = getFlag('tool') || null;
      const rootCause = getFlag('root-cause') || null;
      const suggestedFix = getFlag('suggested-fix') || null;

      await db.execute({
        sql: `INSERT INTO session_errors (id, session_id, message_id, timestamp, tool_name, error_type, error_message, root_cause, suggested_fix)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
        args: [id, sessionId, messageId, timestamp, tool, errorType, text, rootCause, suggestedFix]
      });

      console.log(`✓ Added error: ${text}`);
      if (errorType) console.log(`  Type: ${errorType}`);
      if (rootCause) console.log(`  Root cause: ${rootCause}`);
      if (suggestedFix) console.log(`  Suggested fix: ${suggestedFix}`);
      console.log(`  Session: ${sessionId.slice(0, 8)}... | ID: ${id}`);
    }

    console.log(`  ID: ${id}`);
    console.log(`  Source: manual`);
    console.log();

  } finally {
    db.close();
  }
}

function showSearchHelp() {
  console.log(`
Semantic Search Across Knowledge

Usage:
  know search "<query>" [category] [--json]

Categories (optional):
  decisions   Search only decisions
  learnings   Search only learnings
  errors      Search only errors
  workflows   Search only workflows
  (no category = search all)

Examples:
  know search "prototype theory"
  know search "vector database" decisions
  know search "network timeout" errors
  know search "delegation patterns" workflows --json

Output Fields:
  - Category: Type of knowledge (decision, learning, error, workflow)
  - Content: The knowledge item text
  - Distance: Similarity score (0.0 = identical, higher = less similar)
  - Session ID: Source session
  - Timestamp: When extracted

Description:
  Uses semantic similarity (vector embeddings) to find relevant knowledge
  regardless of exact keyword matches. Powered by libSQL native vectors
  and DiskANN indexes for fast approximate nearest neighbor search.

  Results are ranked by similarity score (lower = more relevant).
  Returns top 10 results by default.
  `);
}

async function runSearch(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showSearchHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  let query = cleanArgs[0];
  if (!query) {
    console.error('Usage: know search "<query>" [category]');
    console.error('Run "know search --help" for more information');
    process.exit(1);
  }

  // Validate query length
  query = validateLength(query, INPUT_LIMITS.MAX_QUERY_LENGTH, 'search query');

  const category = cleanArgs[1]; // Optional: decisions, learnings, errors, workflows

  const db = createClient({ url: `file:${DB_PATH}` });
  const embedder = new EmbeddingGenerator();

  try {
    // Generate embedding for search query
    const queryEmbedding = await embedder.embed(query);
    const vectorJson = `[${Array.from(queryEmbedding).join(',')}]`;

    interface SearchResult {
      category: string;
      id: string;
      session_id: string;
      timestamp: number;
      content: string;
      metadata: string | null;
      distance: number;
    }

    const results: SearchResult[] = [];

    // Helper to search a specific table
    async function searchTable(
      table: string,
      categoryName: string,
      contentField: string,
      metadataFields: string[]
    ) {
      const metadataSelect = metadataFields.length > 0
        ? `, ${metadataFields.join(', ')}`
        : '';

      const sql = `
        SELECT
          '${categoryName}' as category,
          id,
          session_id,
          timestamp,
          ${contentField} as content,
          ${metadataFields.length > 0 ? `json_object(${metadataFields.map(f => `'${f}', ${f}`).join(', ')})` : 'NULL'} as metadata,
          vector_distance_cos(embedding, vector(?)) as distance
        FROM ${table}
        WHERE embedding IS NOT NULL
        ORDER BY distance ASC
        LIMIT 10
      `;

      const result = await db.execute({ sql, args: [vectorJson] });

      return result.rows.map(row => ({
        category: row.category as string,
        id: row.id as string,
        session_id: row.session_id as string,
        timestamp: row.timestamp as number,
        content: row.content as string,
        metadata: row.metadata as string | null,
        distance: row.distance as number
      }));
    }

    // Search based on category filter
    if (!category || category === 'decisions') {
      const decisionsResults = await searchTable(
        'session_decisions',
        'decision',
        'decision',
        ['reasoning', 'alternatives', 'context']
      );
      results.push(...decisionsResults);
    }

    if (!category || category === 'learnings') {
      const learningsResults = await searchTable(
        'session_learnings',
        'learning',
        'learning',
        ['category', 'evidence', 'application']
      );
      results.push(...learningsResults);
    }

    if (!category || category === 'errors') {
      const errorsResults = await searchTable(
        'session_errors',
        'error',
        'error_message',
        ['error_type', 'tool_name', 'resolution', 'prevention']
      );
      results.push(...errorsResults);
    }

    if (!category || category === 'workflows') {
      const workflowsResults = await searchTable(
        'session_workflows',
        'workflow',
        'description',
        ['workflow_type', 'effectiveness', 'outcome', 'lessons']
      );
      results.push(...workflowsResults);
    }

    // Sort all results by distance (best matches first)
    results.sort((a, b) => a.distance - b.distance);

    // Take top 10 overall
    const topResults = results.slice(0, 10);

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'search',
        query: query,
        category: category || 'all',
        count: topResults.length,
        results: topResults.map(r => ({
          category: r.category,
          id: r.id,
          session_id: r.session_id,
          timestamp: r.timestamp,
          content: r.content,
          metadata: r.metadata ? JSON.parse(r.metadata) : null,
          distance: r.distance,
          similarity: (1 - r.distance) * 100 // Convert to percentage
        }))
      }, null, 2));
    } else {
      console.log(`\n🔍 Search Results for "${query}" (${topResults.length} found)\n`);

      if (topResults.length === 0) {
        console.log('  No results found. Try a different query or check if embeddings exist.\n');
      }

      for (const result of topResults) {
        const time = new Date(result.timestamp).toLocaleTimeString();
        const similarity = ((1 - result.distance) * 100).toFixed(1);

        console.log(`[${time}] ${result.category.toUpperCase()} | Similarity: ${similarity}%`);
        console.log(`  ${result.content}`);

        if (result.metadata) {
          const meta = JSON.parse(result.metadata);
          for (const [key, value] of Object.entries(meta)) {
            if (value) {
              console.log(`  ${key}: ${value}`);
            }
          }
        }

        console.log(`  Session: ${result.session_id.slice(0, 8)}... | ID: ${result.id.slice(0, 8)}...\n`);
      }
    }
  } finally {
    db.close();
  }
}

function showDecisionsHelp() {
  console.log(`
Query Extracted Decisions

Usage:
  know decisions [filter] [--json]

Filters:
  search "<query>"           Semantic search for decisions
  today                      Decisions made today
  yesterday                  Decisions made yesterday
  recent [N]                 Most recent N decisions (default: 10)
  session <session-id>       Decisions from specific session
  range <start> <end>        Date range (YYYY-MM-DD format)

Examples:
  know decisions search "vector database choice"
  know decisions recent 20
  know decisions today
  know decisions session f03b3b54-ca47-46d3-be1f-20ccfc82f9de
  know decisions range 2026-01-01 2026-01-31
  know decisions recent --json | jq '.results[] | .decision'

Output Fields:
  - Choice: What was decided
  - Reasoning: Why this choice was made
  - Alternatives: Other options considered
  - Confidence: Classification confidence (0.0-1.0)
  - Session ID: Source session
  - Timestamp: When decision was made

Description:
  Displays architectural and technical decisions extracted from sessions.
  Useful for maintaining consistency and understanding past trade-offs.
  `);
}

async function runDecisions(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showDecisionsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  // Handle search subcommand
  if (cleanArgs[0] === 'search') {
    const query = cleanArgs[1];
    if (!query) {
      console.error('Usage: know decisions search "<query>"');
      process.exit(1);
    }
    // Delegate to main search with category filter
    await runSearch([query, 'decisions', ...(args.includes('--json') ? ['--json'] : [])]);
    return;
  }

  const db = createClient({ url: `file:${DB_PATH}` });
  const command = cleanArgs[0] || 'recent';
  const arg = cleanArgs[1];

  try {
    let sql = '';
    let queryArgs: any[] = [];

    switch (command) {
      case 'today': {
        const startOfDay = new Date().setHours(0, 0, 0, 0);
        sql = 'SELECT * FROM session_decisions WHERE timestamp >= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay];
        break;
      }
      case 'yesterday': {
        const yesterday = new Date(Date.now() - 86400000);
        const startOfDay = yesterday.setHours(0, 0, 0, 0);
        const endOfDay = yesterday.setHours(23, 59, 59, 999);
        sql = 'SELECT * FROM session_decisions WHERE timestamp >= ? AND timestamp <= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay, endOfDay];
        break;
      }
      case 'recent': {
        const limit = parseInt(arg) || 10;
        sql = 'SELECT * FROM session_decisions ORDER BY timestamp DESC LIMIT ?';
        queryArgs = [limit];
        break;
      }
      case 'session': {
        if (!arg) {
          console.error('Usage: know decisions session <session-id>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_decisions WHERE session_id = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      default: {
        console.error('Usage: know decisions [today|yesterday|recent [N]|session <id>]');
        process.exit(1);
      }
    }

    const result = await db.execute({ sql, args: queryArgs });

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'decisions',
        filter: command,
        count: result.rows.length,
        results: result.rows.map(row => ({
          id: row.id,
          session_id: row.session_id,
          message_id: row.message_id,
          timestamp: row.timestamp,
          decision: row.decision,
          reasoning: row.reasoning,
          alternatives: row.alternatives,
          context: row.context,
          confidence: row.confidence
        }))
      }, null, 2));
    } else {
      console.log(`\n📋 Decisions (${result.rows.length} found)\n`);

      for (const row of result.rows) {
        const time = new Date(row.timestamp as number).toLocaleTimeString();
        console.log(`[${time}] ${row.decision}`);
        if (row.reasoning) console.log(`  Reasoning: ${row.reasoning}`);
        if (row.alternatives) console.log(`  Alternatives: ${row.alternatives}`);
        console.log(`  Confidence: ${Math.round((row.confidence as number) * 100)}%`);
        console.log(`  Session: ${(row.session_id as string).slice(0, 8)}... | ID: ${(row.id as string).slice(0, 8)}...\n`);
      }
    }
  } finally {
    db.close();
  }
}

function showLearningsHelp() {
  console.log(`
Query Extracted Learnings

Usage:
  know learnings [filter] [--json]

Filters:
  search "<query>"           Semantic search for learnings
  today                      Learnings from today
  yesterday                  Learnings from yesterday
  recent [N]                 Most recent N learnings (default: 10)
  session <session-id>       Learnings from specific session
  category <name>            Filter by category
  categories                 List all categories with counts
  range <start> <end>        Date range (YYYY-MM-DD format)

Categories:
  technical      Implementation details, APIs, code patterns
  architectural  Design decisions, system structure
  tooling        Tool usage, configurations, workflows
  process        Development processes, best practices
  performance    Optimization insights, benchmarks

Examples:
  know learnings search "libSQL vectors"
  know learnings recent 20
  know learnings category technical
  know learnings categories
  know learnings today
  know learnings session f03b3b54-ca47-46d3-be1f-20ccfc82f9de

Output Fields:
  - Insight: The key discovery or learning
  - Category: Type of learning
  - Evidence: Supporting context or data
  - Application: How this can be applied
  - Confidence: Classification confidence (0.0-1.0)
  - Session ID: Source session
  - Timestamp: When learning occurred

Description:
  Displays insights and discoveries extracted from sessions.
  Useful for avoiding redundant learning and applying proven patterns.
  `);
}

async function runLearnings(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showLearningsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  // Handle search subcommand
  if (cleanArgs[0] === 'search') {
    const query = cleanArgs[1];
    if (!query) {
      console.error('Usage: know learnings search "<query>"');
      process.exit(1);
    }
    // Delegate to main search with category filter
    await runSearch([query, 'learnings', ...(args.includes('--json') ? ['--json'] : [])]);
    return;
  }

  const db = createClient({ url: `file:${DB_PATH}` });
  const command = cleanArgs[0] || 'recent';
  const arg = cleanArgs[1];

  try {
    let sql = '';
    let queryArgs: any[] = [];

    switch (command) {
      case 'today': {
        const startOfDay = new Date().setHours(0, 0, 0, 0);
        sql = 'SELECT * FROM session_learnings WHERE timestamp >= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay];
        break;
      }
      case 'yesterday': {
        const yesterday = new Date(Date.now() - 86400000);
        const startOfDay = yesterday.setHours(0, 0, 0, 0);
        const endOfDay = yesterday.setHours(23, 59, 59, 999);
        sql = 'SELECT * FROM session_learnings WHERE timestamp >= ? AND timestamp <= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay, endOfDay];
        break;
      }
      case 'recent': {
        const limit = parseInt(arg) || 10;
        sql = 'SELECT * FROM session_learnings ORDER BY timestamp DESC LIMIT ?';
        queryArgs = [limit];
        break;
      }
      case 'session': {
        if (!arg) {
          console.error('Usage: know learnings session <session-id>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_learnings WHERE session_id = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      case 'category': {
        if (!arg) {
          console.error('Usage: know learnings category <name>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_learnings WHERE category = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      case 'categories': {
        sql = 'SELECT category, COUNT(*) as count FROM session_learnings GROUP BY category ORDER BY count DESC';
        const result = await db.execute({ sql, args: [] });
        console.log('\n📚 Learning Categories\n');
        for (const row of result.rows) {
          console.log(`  ${(row.category as string || 'uncategorized').padEnd(20)} ${row.count} learnings`);
        }
        console.log();
        db.close();
        return;
      }
      default: {
        console.error('Usage: know learnings [today|yesterday|recent [N]|session <id>|category <name>|categories]');
        process.exit(1);
      }
    }

    const result = await db.execute({ sql, args: queryArgs });

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'learnings',
        filter: command,
        count: result.rows.length,
        results: result.rows.map(row => ({
          id: row.id,
          session_id: row.session_id,
          message_id: row.message_id,
          timestamp: row.timestamp,
          category: row.category,
          learning: row.learning,
          evidence: row.evidence,
          application: row.application,
          confidence: row.confidence
        }))
      }, null, 2));
    } else {
      console.log(`\n📚 Learnings (${result.rows.length} found)\n`);

      for (const row of result.rows) {
        const time = new Date(row.timestamp as number).toLocaleTimeString();
        console.log(`[${time}] ${row.category || 'general'}`);
        console.log(`  Insight: ${row.learning}`);
        if (row.evidence) console.log(`  Evidence: ${row.evidence}`);
        if (row.application) console.log(`  Application: ${row.application}`);
        console.log(`  Confidence: ${Math.round((row.confidence as number) * 100)}%`);
        console.log(`  Session: ${(row.session_id as string).slice(0, 8)}... | ID: ${(row.id as string).slice(0, 8)}...\n`);
      }
    }
  } finally {
    db.close();
  }
}

function showErrorsHelp() {
  console.log(`
Query Extracted Errors

Usage:
  know errors [filter] [--json]

Filters:
  search "<query>"           Semantic search for errors
  today                      Errors from today
  yesterday                  Errors from yesterday
  recent [N]                 Most recent N errors (default: 10)
  session <session-id>       Errors from specific session
  type <error-type>          Filter by error type
  types                      List all error types with counts
  tools                      Group errors by tool that generated them
  range <start> <end>        Date range (YYYY-MM-DD format)

Error Types:
  TypeError         Type-related errors
  NetworkError      Connection, timeout, API errors
  ImportError       Module import issues
  DatabaseError     SQL, query errors
  ValidationError   Input validation failures
  AuthError         Authentication, authorization issues

Examples:
  know errors search "timeout connection"
  know errors recent 20
  know errors type NetworkError
  know errors types
  know errors tools
  know errors today
  know errors session f03b3b54-ca47-46d3-be1f-20ccfc82f9de

Output Fields:
  - Type: Error classification
  - Message: Error description
  - Tool: Tool that generated the error (if applicable)
  - Resolution: How the error was fixed
  - Prevention: How to avoid in the future
  - Confidence: Classification confidence (0.0-1.0)
  - Session ID: Source session
  - Timestamp: When error occurred

Description:
  Displays errors and their resolutions extracted from sessions.
  Useful for avoiding repeated mistakes and applying known fixes.
  `);
}

async function runErrors(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showErrorsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  // Handle search subcommand
  if (cleanArgs[0] === 'search') {
    const query = cleanArgs[1];
    if (!query) {
      console.error('Usage: know errors search "<query>"');
      process.exit(1);
    }
    // Delegate to main search with category filter
    await runSearch([query, 'errors', ...(args.includes('--json') ? ['--json'] : [])]);
    return;
  }

  const db = createClient({ url: `file:${DB_PATH}` });
  const command = cleanArgs[0] || 'recent';
  const arg = cleanArgs[1];

  try {
    let sql = '';
    let queryArgs: any[] = [];

    switch (command) {
      case 'today': {
        const startOfDay = new Date().setHours(0, 0, 0, 0);
        sql = 'SELECT * FROM session_errors WHERE timestamp >= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay];
        break;
      }
      case 'yesterday': {
        const yesterday = new Date(Date.now() - 86400000);
        const startOfDay = yesterday.setHours(0, 0, 0, 0);
        const endOfDay = yesterday.setHours(23, 59, 59, 999);
        sql = 'SELECT * FROM session_errors WHERE timestamp >= ? AND timestamp <= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay, endOfDay];
        break;
      }
      case 'recent': {
        const limit = parseInt(arg) || 10;
        sql = 'SELECT * FROM session_errors ORDER BY timestamp DESC LIMIT ?';
        queryArgs = [limit];
        break;
      }
      case 'session': {
        if (!arg) {
          console.error('Usage: know errors session <session-id>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_errors WHERE session_id = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      case 'type': {
        if (!arg) {
          console.error('Usage: know errors type <name>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_errors WHERE error_type = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      case 'types': {
        sql = 'SELECT error_type, COUNT(*) as count FROM session_errors GROUP BY error_type ORDER BY count DESC';
        const result = await db.execute({ sql, args: [] });
        console.log('\n🚨 Error Types\n');
        for (const row of result.rows) {
          console.log(`  ${(row.error_type as string || 'unknown').padEnd(20)} ${row.count} errors`);
        }
        console.log();
        db.close();
        return;
      }
      case 'tools': {
        sql = 'SELECT tool_name, COUNT(*) as count FROM session_errors GROUP BY tool_name ORDER BY count DESC';
        const result = await db.execute({ sql, args: [] });
        console.log('\n🔧 Errors by Tool\n');
        for (const row of result.rows) {
          console.log(`  ${(row.tool_name as string || 'unknown').padEnd(20)} ${row.count} errors`);
        }
        console.log();
        db.close();
        return;
      }
      default: {
        console.error('Usage: know errors [today|yesterday|recent [N]|session <id>|type <name>|types|tools]');
        process.exit(1);
      }
    }

    const result = await db.execute({ sql, args: queryArgs });

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'errors',
        filter: command,
        count: result.rows.length,
        results: result.rows.map(row => ({
          id: row.id,
          session_id: row.session_id,
          message_id: row.message_id,
          timestamp: row.timestamp,
          error_type: row.error_type,
          error_message: row.error_message,
          tool_name: row.tool_name,
          resolution: row.resolution,
          prevention: row.prevention,
          confidence: row.confidence
        }))
      }, null, 2));
    } else {
      console.log(`\n🚨 Errors (${result.rows.length} found)\n`);

      for (const row of result.rows) {
        const time = new Date(row.timestamp as number).toLocaleTimeString();
        const tool = row.tool_name ? ` | Tool: ${row.tool_name}` : '';
        console.log(`[${time}] ${row.error_type || 'Error'}${tool}`);
        if (row.error_message) console.log(`  Message: ${row.error_message}`);
        if (row.resolution) console.log(`  Resolution: ${row.resolution}`);
        if (row.prevention) console.log(`  Prevention: ${row.prevention}`);
        console.log(`  Confidence: ${Math.round((row.confidence as number) * 100)}%`);
        console.log(`  Session: ${(row.session_id as string).slice(0, 8)}... | ID: ${(row.id as string).slice(0, 8)}...\n`);
      }
    }
  } finally {
    db.close();
  }
}

async function runWorkflows(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showWorkflowsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  // Handle search subcommand
  if (cleanArgs[0] === 'search') {
    const query = cleanArgs[1];
    if (!query) {
      console.error('Usage: know workflows search "<query>"');
      process.exit(1);
    }
    // Delegate to main search with category filter
    await runSearch([query, 'workflows', ...(args.includes('--json') ? ['--json'] : [])]);
    return;
  }

  const db = createClient({ url: `file:${DB_PATH}` });
  const command = cleanArgs[0] || 'recent';
  const arg = cleanArgs[1];

  try {
    let sql = '';
    let queryArgs: any[] = [];

    switch (command) {
      case 'today': {
        const startOfDay = new Date().setHours(0, 0, 0, 0);
        sql = 'SELECT * FROM session_workflows WHERE timestamp >= ? ORDER BY timestamp DESC';
        queryArgs = [startOfDay];
        break;
      }
      case 'recent': {
        const limit = parseInt(arg) || 10;
        sql = 'SELECT * FROM session_workflows ORDER BY timestamp DESC LIMIT ?';
        queryArgs = [limit];
        break;
      }
      case 'type': {
        if (!arg) {
          console.error('Usage: know workflows type <workflow-type>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_workflows WHERE workflow_type = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      case 'types': {
        sql = `SELECT workflow_type, COUNT(*) as count
               FROM session_workflows
               GROUP BY workflow_type
               ORDER BY count DESC`;
        break;
      }
      case 'effective': {
        sql = `SELECT * FROM session_workflows
               WHERE effectiveness = 'effective'
               ORDER BY confidence DESC, timestamp DESC
               LIMIT ${parseInt(arg) || 10}`;
        break;
      }
      case 'session': {
        if (!arg) {
          console.error('Usage: know workflows session <session-id>');
          process.exit(1);
        }
        sql = 'SELECT * FROM session_workflows WHERE session_id = ? ORDER BY timestamp DESC';
        queryArgs = [arg];
        break;
      }
      default: {
        console.error('Usage: know workflows [today|recent [N]|type <type>|types|effective|session <id>]');
        process.exit(1);
      }
    }

    const result = await db.execute({ sql, args: queryArgs });

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'workflows',
        filter: command,
        count: result.rows.length,
        results: result.rows.map(row => ({
          id: row.id,
          session_id: row.session_id,
          timestamp: row.timestamp,
          workflow_type: row.workflow_type,
          description: row.description,
          effectiveness: row.effectiveness,
          context: row.context,
          tools_involved: JSON.parse(row.tools_involved || '[]'),
          outcome: row.outcome,
          lessons: row.lessons,
          confidence: row.confidence
        }))
      }, null, 2));
    } else {
      if (command === 'types') {
        console.log(`\n📊 Workflow Types (${result.rows.length} found)\n`);
        for (const row of result.rows) {
          console.log(`  ${(row.workflow_type as string || 'unknown').padEnd(20)} ${row.count} workflows`);
        }
        console.log();
      } else {
        console.log(`\n📊 Workflows (${result.rows.length} found)\n`);

        result.rows.forEach(row => {
          const time = new Date(row.timestamp as number).toISOString().slice(11, 19);
          const effectivenessEmoji = row.effectiveness === 'effective' ? '✅' :
                                     row.effectiveness === 'ineffective' ? '❌' : '⚠️';

          console.log(`[${time}] ${effectivenessEmoji} ${row.description}`);
          console.log(`  Type: ${row.workflow_type} | Confidence: ${Math.round((row.confidence as number) * 100)}%`);

          if (row.tools_involved) {
            const tools = JSON.parse(row.tools_involved as string);
            if (tools.length > 0) {
              console.log(`  Tools: ${tools.join(', ')}`);
            }
          }

          if (row.outcome) {
            console.log(`  Outcome: ${row.outcome}`);
          }

          if (row.lessons) {
            console.log(`  Lessons: ${row.lessons}`);
          }

          console.log(`  Session: ${(row.session_id as string).slice(0, 8)}... | ID: ${(row.id as string).slice(0, 12)}...`);
          console.log();
        });
      }
    }
  } finally {
    db.close();
  }
}

function showWorkflowsHelp() {
  console.log(`
Query Extracted Workflows

Usage:
  know workflows [filter] [--json]

Filters:
  search "<query>"     Semantic search for workflows
  today                Workflows from today
  recent [N]           Most recent N workflows (default: 10)
  type <type>          Filter by workflow type
  types                List all workflow types with counts
  effective            Most effective workflows
  session <id>         Workflows from specific session

Workflow Types:
  delegation     Using /bg, Task, or agents to handle work
  organization   Breaking down, structuring, consolidating
  planning       Planning/designing before implementation
  collaboration  Multi-agent coordination, parallel work
  tooling        Effective tool/skill usage patterns

Examples:
  know workflows search "parallel task execution"
  know workflows recent 20
  know workflows type delegation
  know workflows effective
  know workflows types

Output Fields:
  - Description: What workflow/approach was used
  - Type: Category of workflow
  - Effectiveness: Whether it worked well
  - Tools Involved: Skills/tools used
  - Outcome: What resulted
  - Lessons: Key takeaways
  - Confidence: Classification confidence (0.0-1.0)

Description:
  Displays workflow patterns and ways of working extracted from sessions.
  Useful for learning effective work organization strategies.
  `);
}

function showSessionsHelp() {
  console.log(`
Query Session Metadata

Usage:
  know sessions [filter] [--json]

Filters:
  today                      Sessions from today
  yesterday                  Sessions from yesterday
  recent [N]                 Most recent N sessions (default: 10)
  range <start> <end>        Date range (YYYY-MM-DD format)

Examples:
  know sessions recent 20
  know sessions today
  know sessions yesterday
  know sessions range 2026-01-01 2026-01-31

Output Fields:
  - ID: Session UUID
  - Created: Session start timestamp
  - Model: Model used (sonnet, opus, haiku)
  - Input/Output Tokens: Token usage
  - Cost: Estimated session cost
  - Message Count: Number of messages in session

Description:
  Displays session metadata including usage statistics and costs.
  Useful for understanding session patterns and resource usage.
  `);
}

async function runSessions(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showSessionsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);
  const command = cleanArgs[0] || 'recent';
  const arg = cleanArgs[1];

  // Validate command BEFORE attempting DB connection
  const validCommands = ['today', 'yesterday', 'recent'];
  if (!validCommands.includes(command)) {
    console.error(`Error: Invalid filter '${command}'`);
    console.error('\nUsage: know sessions [today|yesterday|recent [N]]');
    console.error('\nValid filters:');
    console.error('  today      - Sessions from today');
    console.error('  yesterday  - Sessions from yesterday');
    console.error('  recent [N] - Recent N sessions (default: 10)');
    process.exit(1);
  }

  // Check if database exists before attempting connection
  const fs = require('fs');
  if (!fs.existsSync(DB_PATH)) {
    console.error(`Error: Database not found at ${DB_PATH}`);
    console.error('\nThe knowledge database needs to be initialized.');
    console.error('\nTo initialize:');
    console.error('  1. Run a Claude Code session to create the database automatically');
    console.error('  2. Or manually create the database with: sqlite3 ${DB_PATH} < schema.sql');
    console.error('\nNote: The database is created automatically when you use /wonder, /suspect, /believe, or /know commands.');
    process.exit(1);
  }

  const query = new QueryEngine();

  function formatDate(timestamp: number | Date): string {
    const d = new Date(timestamp);
    return d.toISOString().replace('T', ' ').slice(0, 19);
  }

  function formatCost(cost: number): string {
    return `$${cost.toFixed(3)}`;
  }

  try {
    let sessions: any[] = [];

    switch (command) {
      case 'yesterday': {
        sessions = await query.yesterday();
        if (outputMode === 'json') {
          console.log(JSON.stringify({
            command: 'sessions',
            filter: 'yesterday',
            count: sessions.length,
            results: sessions.map(s => ({
              id: s.id,
              created: s.created,
              summary: s.summary,
              messageCount: s.messageCount,
              cost: s.cost
            }))
          }, null, 2));
        } else {
          console.log(`\n📅 Yesterday (${sessions.length} sessions)\n`);
          sessions.forEach(s => {
            console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
            console.log(`  ID: ${s.id.slice(0, 8)}... | Messages: ${s.messageCount}\n`);
          });
        }
        break;
      }
      case 'today': {
        sessions = await query.today();
        if (outputMode === 'json') {
          console.log(JSON.stringify({
            command: 'sessions',
            filter: 'today',
            count: sessions.length,
            results: sessions.map(s => ({
              id: s.id,
              created: s.created,
              summary: s.summary,
              messageCount: s.messageCount,
              cost: s.cost
            }))
          }, null, 2));
        } else {
          console.log(`\n📅 Today (${sessions.length} sessions)\n`);
          sessions.forEach(s => {
            console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
            console.log(`  ID: ${s.id.slice(0, 8)}... | Messages: ${s.messageCount}\n`);
          });
        }
        break;
      }
      case 'recent': {
        const limit = parseInt(arg) || 10;
        sessions = await query.recent(limit);
        if (outputMode === 'json') {
          console.log(JSON.stringify({
            command: 'sessions',
            filter: 'recent',
            limit: limit,
            count: sessions.length,
            results: sessions.map(s => ({
              id: s.id,
              created: s.created,
              summary: s.summary,
              messageCount: s.messageCount,
              cost: s.cost
            }))
          }, null, 2));
        } else {
          console.log(`\n🕐 Recent ${limit} sessions\n`);
          sessions.forEach(s => {
            console.log(`${formatDate(s.created)} | ${formatCost(s.cost)} | ${s.summary}`);
            console.log(`  ID: ${s.id.slice(0, 8)}... | Messages: ${s.messageCount}\n`);
          });
        }
        break;
      }
      default: {
        console.error('Usage: know sessions [today|yesterday|recent [N]]');
        process.exit(1);
      }
    }
  } finally {
    query.close();
  }
}

function showStatsHelp() {
  console.log(`
Session Knowledge System Statistics

Usage:
  know stats [days] [--json]

Arguments:
  days    Number of days to include (default: 7)

Examples:
  know stats           # Last 7 days
  know stats 30        # Last 30 days
  know stats --json    # JSON output

Output:
  - Session Count: Number of sessions
  - Total Cost: Combined session costs
  - Average Cost: Mean cost per session
  - Total Messages: Combined message count
  - Sessions by Model: Breakdown of model usage

Description:
  Displays aggregate statistics for the session knowledge system.
  Useful for understanding system usage patterns and costs.
  `);
}

function showDiscoverHelp() {
  console.log(`
Discover Available Knowledge

Usage:
  know discover [--json]

Description:
  Shows a summary of all available knowledge in the system:
  - Categories and counts for each knowledge type
  - Most common error types
  - Learning categories
  - Recent activity summary
  - Database statistics

  Useful for understanding what knowledge exists and where to look.

Examples:
  know discover
  know discover --json
  `);
}

async function runDiscover(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showDiscoverHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const db = createClient({ url: `file:${DB_PATH}` });

  try {
    // Get counts for each knowledge type
    const decisionsCount = await db.execute('SELECT COUNT(*) as count FROM session_decisions');
    const learningsCount = await db.execute('SELECT COUNT(*) as count FROM session_learnings');
    const errorsCount = await db.execute('SELECT COUNT(*) as count FROM session_errors');

    // Get learning actionability (current schema doesn't have categories yet)
    const actionability = await db.execute(`
      SELECT actionable, COUNT(*) as count
      FROM session_learnings
      WHERE actionable IS NOT NULL
      GROUP BY actionable
      ORDER BY count DESC
    `);

    // Get error types
    const errorTypes = await db.execute(`
      SELECT error_type, COUNT(*) as count
      FROM session_errors
      WHERE error_type IS NOT NULL
      GROUP BY error_type
      ORDER BY count DESC
    `);

    // Get recent activity (last 7 days)
    const sevenDaysAgo = Date.now() - (7 * 24 * 60 * 60 * 1000);
    const recentDecisions = await db.execute(
      'SELECT COUNT(*) as count FROM session_decisions WHERE timestamp >= ?',
      [sevenDaysAgo]
    );
    const recentLearnings = await db.execute(
      'SELECT COUNT(*) as count FROM session_learnings WHERE timestamp >= ?',
      [sevenDaysAgo]
    );
    const recentErrors = await db.execute(
      'SELECT COUNT(*) as count FROM session_errors WHERE timestamp >= ?',
      [sevenDaysAgo]
    );

    // Get prototype status
    const prototypes = await db.execute('SELECT category FROM prototype_embeddings');

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'discover',
        knowledge: {
          decisions: {
            total: decisionsCount.rows[0].count,
            recent: recentDecisions.rows[0].count
          },
          learnings: {
            total: learningsCount.rows[0].count,
            recent: recentLearnings.rows[0].count,
            actionability: actionability.rows.map(r => ({ actionable: r.actionable, count: r.count }))
          },
          errors: {
            total: errorsCount.rows[0].count,
            recent: recentErrors.rows[0].count,
            types: errorTypes.rows.map(r => ({ type: r.error_type, count: r.count }))
          }
        },
        prototypes: prototypes.rows.map(r => r.category)
      }, null, 2));
    } else {
      console.log('\n🔍 Session Knowledge Discovery\n');

      console.log('📊 Total Knowledge Items:');
      console.log(`  Decisions: ${decisionsCount.rows[0].count} (${recentDecisions.rows[0].count} in last 7 days)`);
      console.log(`  Learnings: ${learningsCount.rows[0].count} (${recentLearnings.rows[0].count} in last 7 days)`);
      console.log(`  Errors:    ${errorsCount.rows[0].count} (${recentErrors.rows[0].count} in last 7 days)`);
      console.log();

      if (actionability.rows.length > 0) {
        console.log('📚 Learning Actionability:');
        actionability.rows.forEach(r => {
          console.log(`  ${String(r.actionable).padEnd(15)} ${r.count} items`);
        });
        console.log();
      }

      if (errorTypes.rows.length > 0) {
        console.log('🚨 Error Types:');
        errorTypes.rows.forEach(r => {
          console.log(`  ${String(r.error_type).padEnd(15)} ${r.count} occurrences`);
        });
        console.log();
      }

      if (prototypes.rows.length > 0) {
        console.log('🎯 Prototype Embeddings:');
        prototypes.rows.forEach(r => {
          console.log(`  ✓ ${r.category}`);
        });
        console.log();
      } else {
        console.log('⚠️  No prototype embeddings found. Run /generate-prototypes first.\n');
      }

      console.log('💡 Suggested Queries:');
      if (errorTypes.rows.length > 0) {
        const topError = errorTypes.rows[0].error_type;
        console.log(`  ./know errors type ${topError}`);
      }
      console.log('  ./know decisions recent 10');
      console.log('  ./know learnings recent 10');
      console.log('  ./know stats\n');
    }
  } finally {
    db.close();
  }
}

async function runStats(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showStatsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);
  const query = new QueryEngine();

  try {
    const days = parseInt(cleanArgs[0]) || 7;
    const stats = await query.costSummary(days);

    function formatCost(cost: number): string {
      return `$${cost.toFixed(3)}`;
    }

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'stats',
        days: days,
        sessionCount: stats.sessionCount,
        totalCost: stats.totalCost,
        avgCost: stats.avgCost,
        totalMessages: stats.totalMessages
      }, null, 2));
    } else {
      console.log(`\n💰 Session Statistics (last ${days} days)\n`);
      console.log(`Sessions:        ${stats.sessionCount}`);
      console.log(`Total cost:      ${formatCost(stats.totalCost)}`);
      console.log(`Average/session: ${formatCost(stats.avgCost)}`);
      console.log(`Total messages:  ${stats.totalMessages}`);
      console.log();
    }
  } finally {
    query.close();
  }
}

function showTemporalHelp() {
  console.log(`
Temporal Queries - Query Knowledge at Specific Points in Time

Usage:
  know temporal "<query>" --as-of="YYYY-MM-DD" [--json]

Examples:
  know temporal "auth decisions" --as-of="2026-01-15"
  know temporal "vector database" --as-of="2026-01-30"
  know temporal "libSQL" --as-of="2026-02-01" --json

Description:
  Query knowledge as it was known at a specific point in time using bi-temporal tracking.
  This shows you what decisions, learnings, and errors existed at that date.

  Bi-temporal tracking means:
  - valid_time: When the fact was true in the real world
  - transaction_time: When we learned about it

  A knowledge item appears in results if:
  - It was valid at the requested time
  - We knew about it at the requested time
  `);
}

async function runTemporal(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showTemporalHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  // Parse query and as-of date
  let query = cleanArgs[0];
  const asOfArg = cleanArgs.find(arg => arg.startsWith('--as-of='));

  if (!query) {
    console.error('Usage: know temporal "<query>" --as-of="YYYY-MM-DD"');
    process.exit(1);
  }

  if (!asOfArg) {
    console.error('Error: --as-of date is required');
    console.error('Usage: know temporal "<query>" --as-of="YYYY-MM-DD"');
    process.exit(1);
  }

  // Validate query length
  query = validateLength(query, INPUT_LIMITS.MAX_QUERY_LENGTH, 'temporal query');

  // Validate date format
  const asOfDateStr = asOfArg.split('=')[1];
  const asOfDate = validateDateString(asOfDateStr);

  const temporal = new TemporalQueries();

  try {
    const results = await temporal.queryAtTime(query, asOfDate);

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'temporal',
        query: query,
        asOf: asOfDate.toISOString(),
        count: results.length,
        results: results
      }, null, 2));
    } else {
      console.log(`\n🕐 Knowledge as of ${asOfDate.toISOString().split('T')[0]}\n`);
      console.log(`Query: "${query}"\n`);

      if (results.length === 0) {
        console.log('No results found for this time period.\n');
      } else {
        for (const result of results) {
          const time = new Date(result.timestamp).toLocaleTimeString();
          console.log(`[${time}] ${result.type.toUpperCase()}`);
          console.log(`  ${result.content}`);
          console.log(`  Confidence: ${Math.round(result.baseConfidence * 100)}%`);
          console.log(`  Session: ${result.sessionId.slice(0, 8)}...`);
          console.log();
        }
      }
    }
  } finally {
    temporal.close();
  }
}

function showDecayHelp() {
  console.log(`
Confidence Decay - Show Knowledge Aging

Usage:
  know decay [--domain=<domain>] [--show] [--json]

Domains:
  tech      Technology knowledge (frameworks, APIs) - 9 month half-life
  science   Scientific knowledge - 7.5 year half-life
  news      Current events - 2 month half-life
  core      Core programming concepts - 5 year half-life (stepped)

Examples:
  know decay --domain=tech
  know decay --show
  know decay --domain=science --json

Description:
  Shows how knowledge confidence decays over time based on domain.
  Different domains age differently:

  - Tech: Exponential decay (frameworks change fast)
  - Science: Power law decay (fundamentals are stable)
  - News: Exponential decay (events become history)
  - Core: Stepped decay (stable until paradigm shifts)
  `);
}

async function runDecay(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showDecayHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const decay = new ConfidenceDecay();

  // Parse domain filter
  const domainArg = args.find(arg => arg.startsWith('--domain='));
  const domain = domainArg ? domainArg.split('=')[1] : undefined;

  // Show decay curves
  if (args.includes('--show')) {
    const domains = domain ? [domain] : decay.getDomains();

    if (outputMode === 'json') {
      const curves = domains.map(d => {
        const config = decay.getConfig(d);
        const ages = [0, 30, 90, 180, 365, 730]; // days
        return {
          domain: d,
          config: config,
          curve: ages.map(days => ({
            days,
            confidence: decay.calculateDecay(1.0, days * 24 * 60 * 60 * 1000, d)
          }))
        };
      });
      console.log(JSON.stringify({ command: 'decay', curves }, null, 2));
    } else {
      console.log('\n📉 Confidence Decay Curves\n');
      for (const d of domains) {
        const config = decay.getConfig(d);
        console.log(`${d.toUpperCase()} (${config?.decayFunction})`);
        console.log(`  Half-life: ${Math.round((config?.halfLifeMs || 0) / (24 * 60 * 60 * 1000))} days`);
        console.log(`  Min confidence: ${config?.minConfidence}`);
        console.log();
        console.log('  Age        Confidence');
        console.log('  -------------------------');

        const ages = [0, 30, 90, 180, 365, 730]; // days
        for (const days of ages) {
          const conf = decay.calculateDecay(1.0, days * 24 * 60 * 60 * 1000, d);
          const bar = '█'.repeat(Math.round(conf * 20));
          console.log(`  ${days.toString().padStart(4)}d     ${(conf * 100).toFixed(1)}% ${bar}`);
        }
        console.log();
      }
    }
  } else {
    // Show knowledge with current decay
    const temporal = new TemporalQueries();
    try {
      const results = await temporal.getWithDecay('', domain);

      if (outputMode === 'json') {
        console.log(JSON.stringify({
          command: 'decay',
          domain: domain || 'all',
          count: results.length,
          results: results.map(r => ({
            id: r.id,
            type: r.type,
            content: r.content,
            baseConfidence: r.baseConfidence,
            currentConfidence: r.currentConfidence,
            domain: r.domain,
            age: Date.now() - (r.validFrom || r.timestamp)
          }))
        }, null, 2));
      } else {
        console.log(`\n📉 Knowledge with Decay Applied${domain ? ` (${domain})` : ''}\n`);

        for (const result of results.slice(0, 20)) {
          const age = Math.round((Date.now() - (result.validFrom || result.timestamp)) / (24 * 60 * 60 * 1000));
          const decayPct = result.currentConfidence && result.baseConfidence
            ? Math.round((1 - result.currentConfidence / result.baseConfidence) * 100)
            : 0;

          console.log(`${result.type.toUpperCase()} | ${result.domain || 'unknown'}`);
          console.log(`  ${result.content.slice(0, 80)}...`);
          console.log(`  Base: ${Math.round(result.baseConfidence * 100)}% → Current: ${Math.round((result.currentConfidence || 0) * 100)}% (${decayPct}% decay)`);
          console.log(`  Age: ${age} days`);
          console.log();
        }
      }
    } finally {
      temporal.close();
    }
  }
}

function showArcsHelp() {
  console.log(`
Thinking Arcs - Detect Conceptual Evolution

Usage:
  know arcs <session-id> [--json]

Arc Types:
  breakthrough         Sudden insight or realization
  pattern_discovery    Recognizing recurring structures
  concrete_to_abstract Moving from specific to general
  refinement           Iterative improvement of ideas
  synthesis            Combining multiple concepts

Examples:
  know arcs f03b3b54-ca47-46d3-be1f-20ccfc82f9de
  know arcs abc123 --json

Description:
  Detects patterns of conceptual evolution in sessions by analyzing
  decisions and learnings for indicators like:
  - "realized", "the pattern is", "this means"
  - Abstraction shifts (concrete → general)
  - Refinement language ("actually", "more precisely")
  `);
}

async function runArcs(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showArcsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  const sessionId = cleanArgs[0];
  if (!sessionId) {
    console.error('Usage: know arcs <session-id>');
    process.exit(1);
  }

  const detector = new ArcDetector();

  try {
    const arcs = await detector.detectArcs(sessionId);

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'arcs',
        sessionId: sessionId,
        count: arcs.length,
        arcs: arcs
      }, null, 2));
    } else {
      console.log(`\n🎯 Thinking Arcs for Session ${sessionId.slice(0, 8)}...\n`);

      if (arcs.length === 0) {
        console.log('No thinking arcs detected in this session.\n');
      } else {
        for (const arc of arcs) {
          const time = new Date(arc.createdAt).toLocaleTimeString();
          console.log(`[${time}] ${arc.arcType.toUpperCase().replace(/_/g, ' ')}`);
          console.log(`  ${arc.description}`);
          if (arc.breakthroughMoment) {
            console.log(`  Moment: "${arc.breakthroughMoment.slice(0, 100)}..."`);
          }
          console.log(`  Confidence: ${Math.round(arc.confidence * 100)}%`);
          console.log();
        }
      }
    }
  } finally {
    detector.close();
  }
}

function showRelationshipsHelp() {
  console.log(`
Knowledge Relationships - How Knowledge Connects

Usage:
  know relationships <knowledge-id> [--json]

Relationship Types:
  supports      This knowledge supports another
  contradicts   This knowledge contradicts another
  supersedes    This replaces older knowledge
  evolves_from  This builds on previous knowledge
  depends_on    This requires other knowledge

Examples:
  know relationships decision-abc123
  know relationships learning-xyz789 --json

Description:
  Shows how knowledge items relate to each other.
  Helps understand dependencies, contradictions, and evolution.

  Note: Relationships are detected automatically or can be added manually.
  `);
}

async function runRelationships(args: string[]) {
  if (args.includes('--help') || args.includes('-h')) {
    showRelationshipsHelp();
    return;
  }

  const outputMode = getOutputMode(args);
  const cleanArgs = stripJsonFlag(args);

  const knowledgeId = cleanArgs[0];
  if (!knowledgeId) {
    console.error('Usage: know relationships <knowledge-id>');
    process.exit(1);
  }

  const db = createClient({ url: `file:${DB_PATH}` });

  try {
    // Get outgoing relationships (from this knowledge)
    const outgoing = await db.execute({
      sql: `
        SELECT * FROM knowledge_relationships
        WHERE from_id = ?
        ORDER BY created_at DESC
      `,
      args: [knowledgeId]
    });

    // Get incoming relationships (to this knowledge)
    const incoming = await db.execute({
      sql: `
        SELECT * FROM knowledge_relationships
        WHERE to_id = ?
        ORDER BY created_at DESC
      `,
      args: [knowledgeId]
    });

    if (outputMode === 'json') {
      console.log(JSON.stringify({
        command: 'relationships',
        knowledgeId: knowledgeId,
        outgoing: outgoing.rows,
        incoming: incoming.rows
      }, null, 2));
    } else {
      console.log(`\n🔗 Relationships for ${knowledgeId}\n`);

      if (outgoing.rows.length === 0 && incoming.rows.length === 0) {
        console.log('No relationships found for this knowledge item.\n');
      } else {
        if (outgoing.rows.length > 0) {
          console.log('Outgoing:');
          for (const row of outgoing.rows) {
            console.log(`  ${row.relationship_type} → ${row.to_type}:${row.to_id}`);
            if (row.evidence) console.log(`    Evidence: ${row.evidence}`);
            console.log(`    Confidence: ${Math.round((row.confidence as number) * 100)}%`);
          }
          console.log();
        }

        if (incoming.rows.length > 0) {
          console.log('Incoming:');
          for (const row of incoming.rows) {
            console.log(`  ${row.from_type}:${row.from_id} → ${row.relationship_type}`);
            if (row.evidence) console.log(`    Evidence: ${row.evidence}`);
            console.log(`    Confidence: ${Math.round((row.confidence as number) * 100)}%`);
          }
          console.log();
        }
      }
    }
  } finally {
    db.close();
  }
}

// Main CLI router
const command = process.argv[2];
const args = process.argv.slice(3);

if (!command || command === '--help' || command === '-h') {
  showHelp();
  process.exit(0);
}

try {
  switch (command) {
    case 'index':
      await runIndex(args);
      break;
    case 'embed':
      await runEmbed(args);
      break;
    case 'prototypes':
      await runPrototypes(args);
      break;
    case 'extract':
      await runExtract(args);
      break;
    case 'add':
      await runAdd(args);
      break;
    case 'search':
      await runSearch(args);
      break;
    case 'decisions':
      await runDecisions(args);
      break;
    case 'learnings':
      await runLearnings(args);
      break;
    case 'errors':
      await runErrors(args);
      break;
    case 'workflows':
      await runWorkflows(args);
      break;
    case 'sessions':
      await runSessions(args);
      break;
    case 'stats':
      await runStats(args);
      break;
    case 'discover':
      await runDiscover(args);
      break;
    case 'temporal':
      await runTemporal(args);
      break;
    case 'decay':
      await runDecay(args);
      break;
    case 'arcs':
      await runArcs(args);
      break;
    case 'relationships':
      await runRelationships(args);
      break;
    default:
      console.error(`Unknown command: ${command}`);
      showHelp();
      process.exit(1);
  }
} catch (error) {
  console.error('Error:', sanitizeErrorMessage(error));
  if (process.env.NODE_ENV !== 'production' && error instanceof Error) {
    console.error('Stack:', sanitizeStackTrace(error.stack));
  }
  process.exit(1);
}
