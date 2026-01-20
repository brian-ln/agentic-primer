#!/usr/bin/env bun

/**
 * Session context extraction utilities - TIME-BASED
 *
 * Extracts context from Claude Code session logs using time ranges
 * instead of line counts for more accurate recent context.
 *
 * Future: Integrate with session logs as actors in graph
 */

export interface SessionMessage {
  type: "user" | "assistant";
  timestamp: string; // ISO-8601
  content: string;
  toolCalls?: ToolCall[];
}

export interface ToolCall {
  name: string;
  input: Record<string, any>;
}

export interface SessionContext {
  recentMessages: SessionMessage[];
  recentDecisions: string[];
  activeFiles: string[];
  userGoals: string[];
  technicalConstraints: string[];
  projectPatterns: string[];
  timeRange: {
    start: string;
    end: string;
  };
}

/**
 * Extract context from session log using time-based windows
 */
export async function extractSessionContext(
  sessionLogPath: string,
  options: {
    minutesBack?: number; // Default: 30 minutes
    minMessages?: number; // Minimum messages even if outside time window
  } = {}
): Promise<SessionContext> {
  const { minutesBack = 30, minMessages = 20 } = options;

  // Calculate time range
  const now = new Date();
  const startTime = new Date(now.getTime() - minutesBack * 60 * 1000);
  const startISO = startTime.toISOString();
  const endISO = now.toISOString();

  // Read session log
  const content = await Bun.file(sessionLogPath).text();
  const lines = content.trim().split("\n").filter(Boolean);

  // Parse messages within time range
  const messages: SessionMessage[] = [];
  const allMessages: SessionMessage[] = []; // Fallback if time window too small

  for (const line of lines) {
    try {
      const entry = JSON.parse(line);
      const message: SessionMessage = {
        type: entry.type,
        timestamp: entry.timestamp,
        content: extractTextContent(entry),
        toolCalls: extractToolCalls(entry),
      };

      allMessages.push(message);

      // Time-based filtering
      if (message.timestamp >= startISO && message.timestamp <= endISO) {
        messages.push(message);
      }
    } catch (err) {
      // Skip malformed lines
      continue;
    }
  }

  // Ensure minimum message count
  const recentMessages =
    messages.length >= minMessages
      ? messages
      : allMessages.slice(-minMessages);

  // Extract specific context types
  const recentDecisions = extractDecisions(recentMessages);
  const activeFiles = extractFiles(recentMessages);
  const userGoals = extractGoals(recentMessages);
  const technicalConstraints = extractConstraints(recentMessages);
  const projectPatterns = extractPatterns(recentMessages);

  return {
    recentMessages,
    recentDecisions,
    activeFiles,
    userGoals,
    technicalConstraints,
    projectPatterns,
    timeRange: {
      start: startISO,
      end: endISO,
    },
  };
}

/**
 * Extract text content from session entry
 */
function extractTextContent(entry: any): string {
  if (!entry.message?.content) return "";

  const textParts = entry.message.content
    .filter((c: any) => c.type === "text")
    .map((c: any) => c.text);

  return textParts.join("\n");
}

/**
 * Extract tool calls from session entry
 */
function extractToolCalls(entry: any): ToolCall[] {
  if (!entry.message?.content) return [];

  return entry.message.content
    .filter((c: any) => c.type === "tool_use")
    .map((c: any) => ({
      name: c.name,
      input: c.input,
    }));
}

/**
 * Extract recent decisions from messages
 */
function extractDecisions(messages: SessionMessage[]): string[] {
  const decisions: string[] = [];
  const decisionKeywords = /\b(decided|will|approach|chose|selected)\b/i;

  for (const msg of messages) {
    if (msg.type === "assistant" && decisionKeywords.test(msg.content)) {
      // Extract sentences containing decision keywords
      const sentences = msg.content.split(/[.!?]+/);
      for (const sentence of sentences) {
        if (decisionKeywords.test(sentence)) {
          decisions.push(sentence.trim());
        }
      }
    }
  }

  return decisions.slice(-10); // Last 10 decisions
}

/**
 * Extract active files from tool calls
 */
function extractFiles(messages: SessionMessage[]): string[] {
  const files = new Set<string>();

  for (const msg of messages) {
    if (msg.toolCalls) {
      for (const tool of msg.toolCalls) {
        if (
          (tool.name === "Edit" || tool.name === "Write") &&
          tool.input.file_path
        ) {
          files.add(tool.input.file_path);
        }
      }
    }
  }

  return Array.from(files);
}

/**
 * Extract user goals from user messages
 */
function extractGoals(messages: SessionMessage[]): string[] {
  const goals: string[] = [];
  const goalKeywords = /\b(want|need|implement|build|create|add)\b/i;

  for (const msg of messages) {
    if (msg.type === "user" && goalKeywords.test(msg.content)) {
      goals.push(msg.content.trim());
    }
  }

  return goals;
}

/**
 * Extract technical constraints
 */
function extractConstraints(messages: SessionMessage[]): string[] {
  const constraints: string[] = [];
  const constraintKeywords = /\b(must|should|cannot|don't|constraint|requirement)\b/i;

  for (const msg of messages) {
    if (constraintKeywords.test(msg.content)) {
      const sentences = msg.content.split(/[.!?]+/);
      for (const sentence of sentences) {
        if (constraintKeywords.test(sentence)) {
          constraints.push(sentence.trim());
        }
      }
    }
  }

  return constraints.slice(-10);
}

/**
 * Extract project patterns and conventions
 */
function extractPatterns(messages: SessionMessage[]): string[] {
  const patterns: string[] = [];
  const patternKeywords = /\b(pattern|convention|style|follows|actor\s+model|worldview)\b/i;

  for (const msg of messages) {
    if (patternKeywords.test(msg.content)) {
      const sentences = msg.content.split(/[.!?]+/);
      for (const sentence of sentences) {
        if (patternKeywords.test(sentence)) {
          patterns.push(sentence.trim());
        }
      }
    }
  }

  return patterns.slice(-10);
}

/**
 * CLI usage
 */
if (import.meta.main) {
  const sessionLogPath = process.argv[2];
  const minutesBack = parseInt(process.argv[3] || "30");

  if (!sessionLogPath) {
    console.error("Usage: bun session-context.ts <session-log-path> [minutes-back]");
    console.error("Example: bun session-context.ts ~/.claude/projects/.../session.jsonl 30");
    process.exit(1);
  }

  const context = await extractSessionContext(sessionLogPath, { minutesBack });

  console.log("📊 SESSION CONTEXT");
  console.log("─".repeat(80));
  console.log(`Time Range: ${context.timeRange.start} → ${context.timeRange.end}`);
  console.log(`Messages: ${context.recentMessages.length}`);
  console.log();

  console.log("🎯 Recent Decisions:");
  context.recentDecisions.forEach((d, i) => {
    console.log(`  ${i + 1}. ${d}`);
  });
  console.log();

  console.log("📁 Active Files:");
  context.activeFiles.forEach((f) => {
    console.log(`  - ${f}`);
  });
  console.log();

  console.log("🎯 User Goals:");
  context.userGoals.forEach((g) => {
    console.log(`  - ${g.substring(0, 100)}...`);
  });
  console.log();

  console.log("⚠️  Technical Constraints:");
  context.technicalConstraints.forEach((c) => {
    console.log(`  - ${c}`);
  });
  console.log();

  console.log("🏗️  Project Patterns:");
  context.projectPatterns.forEach((p) => {
    console.log(`  - ${p}`);
  });
}
