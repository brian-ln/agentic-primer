#!/usr/bin/env bun
/**
 * KnowledgeExtractor - Batch processing pipeline for session knowledge extraction
 * Epic: agentic-primer-9ad
 * Phase: agentic-primer-9ad.3
 *
 * Two-stage pipeline:
 * 1. Stage 1: Fast candidate detection (parallel, heuristic-based)
 * 2. Stage 2: LLM classification of candidates only (rate-limited, parallel)
 *
 * Stores results in:
 * - session_decisions table
 * - session_learnings table
 * - session_errors table
 */

import { createClient, type Client } from '@libsql/client';
import { join } from 'path';
import { randomUUID } from 'crypto';
import { EmbeddingGenerator } from '../embeddings/EmbeddingGenerator';
import { SemanticMessageClassifier } from '../classification/SemanticMessageClassifier';
import { llmRateLimiter } from '@agentic-primer/knowledge';

const INDEX_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(INDEX_DIR, 'sessions-libsql.db');
const SESSION_DIR_PREFIX = join(process.env.HOME!, '.claude/projects');

export interface ExtractionResult {
  sessionId: string;
  messageCount: number;
  candidatesDetected: number;
  decisionsExtracted: number;
  learningsExtracted: number;
  errorsExtracted: number;
  workflowsExtracted: number;
  processingTimeMs: number;
}

export interface MessageWithEmbedding {
  messageId: string;
  sessionId: string;
  content: string;
  embedding: Float32Array;
  timestamp: number;
}

export interface CandidateMessage {
  messageId: string;
  content: string;
  timestamp: number;
  categories: string[]; // ['decision', 'learning', 'error']
}

export interface Decision {
  id: string;
  sessionId: string;
  messageId: string;
  timestamp: number;
  decision: string;
  reasoning: string;
  alternatives: string;
  context: string;
  confidence: number;
}

export interface Learning {
  id: string;
  sessionId: string;
  messageId: string;
  timestamp: number;
  learning: string;
  category: string;
  evidence: string;
  application: string;
  confidence: number;
}

export interface SessionError {
  id: string;
  sessionId: string;
  messageId: string;
  timestamp: number;
  toolName: string | null;
  errorType: string | null;
  errorMessage: string;
  resolution: string;
  prevention: string;
  confidence: number;
}

export interface Workflow {
  id: string;
  sessionId: string;
  messageId: string;
  timestamp: number;
  workflowType: string;
  description: string;
  effectiveness: string;
  context: string;
  toolsInvolved: string;
  outcome: string;
  lessons: string;
  confidence: number;
}

/**
 * Stage 1: Fast candidate detection using heuristics
 * Filters messages to find potential decisions, learnings, and errors, and workflows
 */
class MessageCandidateDetector {
  // Decision indicators
  private static DECISION_PATTERNS = [
    /\b(decided|chosen|selected|opted|going with|will use|chose)\b/i,
    /\b(instead of|rather than|over|vs\.?)\b/i,
    /\b(decision|choice|option|approach)\b/i,
  ];

  // Learning indicators
  private static LEARNING_PATTERNS = [
    /\b(learned|discovered|found|realized|insight|key finding)\b/i,
    /\b(turns out|it appears|we see that)\b/i,
    /\b(important to note|worth noting)\b/i,
  ];

  // Error indicators
  private static ERROR_PATTERNS = [
    /\b(error|failed|failure|exception|crash)\b/i,
    /\b(fix|resolution|workaround|solved)\b/i,
    /\btool_use_error\b/i,
  ];

  // Workflow indicators
  private static WORKFLOW_PATTERNS = [
    /\/(bg|reflect|know|skillsmith)\b/i,
    /\b(background|parallel|sequential|delegat)\w*/i,
    /\b(broke down|split into|organized|structured|consolidated)\b/i,
    /\b(workflow|approach|strategy|pattern|process)\b/i,
    /\b(plan\w*|design\w*|architect\w*)\s+(first|before|ahead)/i,
    /\b(iterative|incremental|phased)\b/i,
    /\b(agent|task|subagent|spawn\w*)\b/i,
    /\b(collaborated|coordinated|distributed)\b/i,
    /\b(worked well|effective|efficient|faster|saved time)\b/i,
    /\b(didn't work|ineffective|slower|wasted)\b/i,
    /\b(learned that|discovered|realized|found that)\b.*\b(approach|method|way)\b/i,
  ];

  /**
   * Detect if a message is a candidate for any category
   */
  static detectCandidates(message: MessageWithEmbedding): CandidateMessage | null {
    const categories: string[] = [];
    const content = message.content.toLowerCase();

    // Check for decision indicators
    if (this.DECISION_PATTERNS.some(pattern => pattern.test(content))) {
      categories.push('decision');
    }

    // Check for learning indicators
    if (this.LEARNING_PATTERNS.some(pattern => pattern.test(content))) {
      categories.push('learning');
    }

    // Check for error indicators
    if (this.ERROR_PATTERNS.some(pattern => pattern.test(content))) {
      categories.push('error');
    }

    // Check for workflow indicators
    if (this.WORKFLOW_PATTERNS.some(pattern => pattern.test(content))) {
      categories.push('workflow');
    }

    if (categories.length === 0) {
      return null;
    }

    return {
      messageId: message.messageId,
      content: message.content,
      timestamp: message.timestamp,
      categories,
    };
  }

  /**
   * Batch detect candidates from messages
   */
  static detectBatch(messages: MessageWithEmbedding[]): CandidateMessage[] {
    return messages
      .map(msg => this.detectCandidates(msg))
      .filter((candidate): candidate is CandidateMessage => candidate !== null);
  }
}


/**
 * Main extractor - orchestrates two-stage pipeline
 */
export class KnowledgeExtractor {
  private client: Client;
  private classifier: SemanticMessageClassifier;

  constructor(dbPath: string = DB_PATH) {
    this.client = createClient({ url: `file:${dbPath}` });
    this.classifier = new SemanticMessageClassifier();
  }

  /**
   * Extract knowledge from a single session
   */
  async extractSession(sessionId: string): Promise<ExtractionResult> {
    const startTime = Date.now();

    console.log(`\n🔍 Extracting knowledge from session ${sessionId.slice(0, 8)}...`);

    // Load messages with embeddings
    const messages = await this.loadSessionMessages(sessionId);
    console.log(`  Found ${messages.length} messages with embeddings`);

    if (messages.length === 0) {
      return {
        sessionId,
        messageCount: 0,
        candidatesDetected: 0,
        decisionsExtracted: 0,
        learningsExtracted: 0,
        errorsExtracted: 0,
        workflowsExtracted: 0,
        processingTimeMs: Date.now() - startTime,
      };
    }

    // Stage 1: Fast candidate detection (parallel, heuristic-based)
    const candidates = MessageCandidateDetector.detectBatch(messages);
    console.log(`  Stage 1: Detected ${candidates.length} candidates (${((candidates.length / messages.length) * 100).toFixed(1)}% of messages)`);

    if (candidates.length === 0) {
      return {
        sessionId,
        messageCount: messages.length,
        candidatesDetected: 0,
        decisionsExtracted: 0,
        learningsExtracted: 0,
        errorsExtracted: 0,
        workflowsExtracted: 0,
        processingTimeMs: Date.now() - startTime,
      };
    }

    // Stage 2: LLM classification (batch processing)
    console.log(`  Stage 2: Classifying ${candidates.length} candidates...`);

    let decisionsExtracted = 0;
    let learningsExtracted = 0;
    let errorsExtracted = 0;
    let workflowsExtracted = 0;

    // Group candidates by category
    const decisionCandidates = candidates.filter(c => c.categories.includes('decision'));
    const learningCandidates = candidates.filter(c => c.categories.includes('learning'));
    const errorCandidates = candidates.filter(c => c.categories.includes('error'));
    const workflowCandidates = candidates.filter(c => c.categories.includes('workflow'));

    // Batch classify each category (4 LLM calls total, not N×4)
    const [decisionResults, learningResults, errorResults, workflowResults] = await Promise.all([
      decisionCandidates.length > 0
        ? (await llmRateLimiter.throttle(),
           this.classifier.classifyDecisionBatch(decisionCandidates.map(c => c.content)).then(r => (llmRateLimiter.recordSuccess(), r)))
        : [],
      learningCandidates.length > 0
        ? (await llmRateLimiter.throttle(),
           this.classifier.classifyLearningBatch(learningCandidates.map(c => c.content)).then(r => (llmRateLimiter.recordSuccess(), r)))
        : [],
      errorCandidates.length > 0
        ? (await llmRateLimiter.throttle(),
           this.classifier.classifyErrorBatch(errorCandidates.map(c => c.content)).then(r => (llmRateLimiter.recordSuccess(), r)))
        : [],
      workflowCandidates.length > 0
        ? (await llmRateLimiter.throttle(),
           this.classifier.classifyWorkflowBatch(workflowCandidates.map(c => c.content)).then(r => (llmRateLimiter.recordSuccess(), r)))
        : [],
    ]);

    // Store decision results
    for (let i = 0; i < decisionResults.length; i++) {
      const result = decisionResults[i];
      if (result) {
        const candidate = decisionCandidates[i];
        const decision: Decision = {
          id: randomUUID(),
          sessionId,
          messageId: candidate.messageId,
          timestamp: candidate.timestamp,
          decision: result.choice,
          reasoning: result.reasoning || '',
          alternatives: result.alternatives?.join(', ') || '',
          context: '',
          confidence: result.confidence,
        };
        await this.storeDecision(decision);
        decisionsExtracted++;
      }
    }

    // Store learning results
    for (let i = 0; i < learningResults.length; i++) {
      const result = learningResults[i];
      if (result) {
        const candidate = learningCandidates[i];
        const learning: Learning = {
          id: randomUUID(),
          sessionId,
          messageId: candidate.messageId,
          timestamp: candidate.timestamp,
          learning: result.insight,
          category: result.category || '',
          evidence: result.evidence || '',
          application: result.application || '',
          confidence: result.confidence,
        };
        await this.storeLearning(learning);
        learningsExtracted++;
      }
    }

    // Store error results
    for (let i = 0; i < errorResults.length; i++) {
      const result = errorResults[i];
      if (result) {
        const candidate = errorCandidates[i];
        const error: SessionError = {
          id: randomUUID(),
          sessionId,
          messageId: candidate.messageId,
          timestamp: candidate.timestamp,
          toolName: null,
          errorType: result.type || null,
          errorMessage: result.message,
          resolution: result.resolution || '',
          prevention: result.prevention || '',
          confidence: result.confidence,
        };
        await this.storeError(error);
        errorsExtracted++;
      }
    }

    // Store workflow results
    for (let i = 0; i < workflowResults.length; i++) {
      const result = workflowResults[i];
      if (result) {
        const candidate = workflowCandidates[i];
        const workflow: Workflow = {
          id: randomUUID(),
          sessionId,
          messageId: candidate.messageId,
          timestamp: candidate.timestamp,
          workflowType: result.workflow_type,
          description: result.description,
          effectiveness: result.effectiveness,
          context: result.context || '',
          toolsInvolved: JSON.stringify(result.tools_involved || []),
          outcome: result.outcome || '',
          lessons: result.lessons || '',
          confidence: result.confidence,
        };
        await this.storeWorkflow(workflow);
        workflowsExtracted++;
      }
    }

    console.log(`    Batch classified: ${decisionCandidates.length} decision, ${learningCandidates.length} learning, ${errorCandidates.length} error, ${workflowCandidates.length} workflow candidates`);


    const processingTimeMs = Date.now() - startTime;

    console.log(`  ✓ Extracted: ${decisionsExtracted} decisions, ${learningsExtracted} learnings, ${errorsExtracted} errors, ${workflowsExtracted} workflows (${processingTimeMs}ms)`);

    return {
      sessionId,
      messageCount: messages.length,
      candidatesDetected: candidates.length,
      decisionsExtracted,
      learningsExtracted,
      errorsExtracted,
      workflowsExtracted,
      processingTimeMs,
    };
  }

  /**
   * Extract knowledge from all unprocessed sessions
   */
  async extractAll(): Promise<number> {
    const sessions = await this.client.execute(`
      SELECT DISTINCT s.id
      FROM sessions s
      INNER JOIN message_embeddings me ON s.id = me.session_id
      WHERE NOT EXISTS (
        SELECT 1 FROM session_decisions sd WHERE sd.session_id = s.id
      )
      AND NOT EXISTS (
        SELECT 1 FROM session_learnings sl WHERE sl.session_id = s.id
      )
      AND NOT EXISTS (
        SELECT 1 FROM session_errors se WHERE se.session_id = s.id
      )
      ORDER BY s.modified DESC
    `);

    const sessionIds = sessions.rows.map(row => row.id as string);
    console.log(`\n📚 Extracting knowledge from ${sessionIds.length} unprocessed sessions...\n`);

    let totalExtracted = 0;

    for (const sessionId of sessionIds) {
      const result = await this.extractSession(sessionId);
      totalExtracted += result.decisionsExtracted + result.learningsExtracted + result.errorsExtracted + result.workflowsExtracted;
    }

    return totalExtracted;
  }

  /**
   * Extract knowledge from sessions in a date range
   */
  async extractRange(startDate: Date, endDate: Date): Promise<number> {
    const sessions = await this.client.execute({
      sql: `
        SELECT DISTINCT s.id
        FROM sessions s
        INNER JOIN message_embeddings me ON s.id = me.session_id
        WHERE s.created >= ? AND s.created <= ?
        ORDER BY s.modified DESC
      `,
      args: [startDate.getTime(), endDate.getTime()],
    });

    const sessionIds = sessions.rows.map(row => row.id as string);
    console.log(`\n📚 Extracting knowledge from ${sessionIds.length} sessions in date range...\n`);

    let totalExtracted = 0;

    for (const sessionId of sessionIds) {
      const result = await this.extractSession(sessionId);
      totalExtracted += result.decisionsExtracted + result.learningsExtracted + result.errorsExtracted + result.workflowsExtracted;
    }

    return totalExtracted;
  }

  /**
   * Load messages with embeddings for a session
   */
  private async loadSessionMessages(sessionId: string): Promise<MessageWithEmbedding[]> {
    const result = await this.client.execute({
      sql: `
        SELECT message_id, session_id, content, embedding, timestamp
        FROM message_embeddings
        WHERE session_id = ?
        ORDER BY timestamp ASC
      `,
      args: [sessionId],
    });

    return result.rows.map((row: any) => ({
      messageId: row.message_id,
      sessionId: row.session_id,
      content: row.content,
      embedding: row.embedding as Float32Array,
      timestamp: row.timestamp,
    }));
  }

  /**
   * Store a decision in the database
   */
  private async storeDecision(decision: Decision): Promise<void> {
    await this.client.execute({
      sql: `
        INSERT INTO session_decisions
        (id, session_id, message_id, timestamp, decision, reasoning, alternatives, context, confidence)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
          decision = excluded.decision,
          reasoning = excluded.reasoning,
          alternatives = excluded.alternatives,
          context = excluded.context,
          confidence = excluded.confidence
      `,
      args: [
        decision.id,
        decision.sessionId,
        decision.messageId,
        decision.timestamp,
        decision.decision,
        decision.reasoning,
        decision.alternatives,
        decision.context,
        decision.confidence,
      ],
    });
  }

  /**
   * Store a learning in the database
   */
  private async storeLearning(learning: Learning): Promise<void> {
    await this.client.execute({
      sql: `
        INSERT INTO session_learnings
        (id, session_id, message_id, timestamp, learning, category, evidence, application, confidence)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
          learning = excluded.learning,
          category = excluded.category,
          evidence = excluded.evidence,
          application = excluded.application,
          confidence = excluded.confidence
      `,
      args: [
        learning.id,
        learning.sessionId,
        learning.messageId,
        learning.timestamp,
        learning.learning,
        learning.category,
        learning.evidence,
        learning.application,
        learning.confidence,
      ],
    });
  }

  /**
   * Store an error in the database
   */
  private async storeError(error: SessionError): Promise<void> {
    await this.client.execute({
      sql: `
        INSERT INTO session_errors
        (id, session_id, message_id, timestamp, tool_name, error_type, error_message, resolution, prevention, confidence)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
          tool_name = excluded.tool_name,
          error_type = excluded.error_type,
          error_message = excluded.error_message,
          resolution = excluded.resolution,
          prevention = excluded.prevention,
          confidence = excluded.confidence
      `,
      args: [
        error.id,
        error.sessionId,
        error.messageId,
        error.timestamp,
        error.toolName,
        error.errorType,
        error.errorMessage,
        error.resolution,
        error.prevention,
        error.confidence,
      ],
    });
  }

  /**
   * Store a workflow in the database
   */
  private async storeWorkflow(workflow: Workflow): Promise<void> {
    await this.client.execute({
      sql: `
        INSERT INTO session_workflows
        (id, session_id, message_id, timestamp, workflow_type, description, effectiveness, context, tools_involved, outcome, lessons, confidence)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
          workflow_type = excluded.workflow_type,
          description = excluded.description,
          effectiveness = excluded.effectiveness,
          context = excluded.context,
          tools_involved = excluded.tools_involved,
          outcome = excluded.outcome,
          lessons = excluded.lessons,
          confidence = excluded.confidence
      `,
      args: [
        workflow.id,
        workflow.sessionId,
        workflow.messageId,
        workflow.timestamp,
        workflow.workflowType,
        workflow.description,
        workflow.effectiveness,
        workflow.context,
        workflow.toolsInvolved,
        workflow.outcome,
        workflow.lessons,
        workflow.confidence,
      ],
    });
  }

  async close(): Promise<void> {
    this.client.close();
  }
}

// CLI entry point
if (import.meta.main) {
  const extractor = new KnowledgeExtractor();

  try {
    const arg = process.argv[2];

    if (!arg) {
      console.error('Usage: extract-knowledge [session-id | today | yesterday | all]');
      process.exit(1);
    }

    if (arg === 'all') {
      const count = await extractor.extractAll();
      console.log(`\n✓ Extracted ${count} total items across all sessions\n`);
    } else if (arg === 'today') {
      const start = new Date();
      start.setHours(0, 0, 0, 0);
      const end = new Date();
      end.setHours(23, 59, 59, 999);
      const count = await extractor.extractRange(start, end);
      console.log(`\n✓ Extracted ${count} total items from today's sessions\n`);
    } else if (arg === 'yesterday') {
      const start = new Date();
      start.setDate(start.getDate() - 1);
      start.setHours(0, 0, 0, 0);
      const end = new Date();
      end.setDate(end.getDate() - 1);
      end.setHours(23, 59, 59, 999);
      const count = await extractor.extractRange(start, end);
      console.log(`\n✓ Extracted ${count} total items from yesterday's sessions\n`);
    } else {
      // Treat as session ID
      const result = await extractor.extractSession(arg);
      console.log(`
📊 Extraction Summary

Session: ${result.sessionId}
Messages: ${result.messageCount}
Candidates: ${result.candidatesDetected}
Decisions: ${result.decisionsExtracted}
Learnings: ${result.learningsExtracted}
Errors: ${result.errorsExtracted}
Time: ${result.processingTimeMs}ms
      `.trim());
    }
  } catch (error) {
    console.error('Error:', error instanceof Error ? error.message : error);
    process.exit(1);
  } finally {
    await extractor.close();
  }
}
