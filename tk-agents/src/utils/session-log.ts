/**
 * Session Log Utilities
 *
 * Helper functions for background agents to extract context from Claude Code session logs.
 * Enables agents to autonomously gather context without parent agent validation.
 */

import { readFileSync, existsSync } from "fs";
import { resolve, dirname } from "path";

export interface SessionContext {
  recentDecisions: string[];
  activeFiles: string[];
  userGoals: string[];
  technicalConstraints: string[];
  projectPatterns: string[];
  recentToolCalls: ToolCallSummary[];
}

export interface ToolCallSummary {
  name: string;
  count: number;
  lastUsed: string;
}

export interface SessionMessage {
  type: "user" | "assistant";
  message: {
    role: string;
    content: Array<{
      type: string;
      text?: string;
      name?: string;
      input?: any;
    }>;
  };
  timestamp: string;
}

/**
 * Get the session log path for the current project
 * Converts working directory to Claude's project hash format
 */
export function getCurrentSessionLogPath(): string {
  const cwd = process.cwd();

  // Convert /Users/bln/path/to/project -> bln-path-to-project
  const projectHash = cwd
    .split("/")
    .filter(p => p !== "" && p !== "Users")
    .join("-");

  const projectDir = resolve(process.env.HOME!, ".claude", "projects", `-Users-${projectHash}`);

  if (!existsSync(projectDir)) {
    throw new Error(`Session directory not found: ${projectDir}`);
  }

  // Find most recent .jsonl file
  const fs = require("fs");
  const files = fs.readdirSync(projectDir)
    .filter((f: string) => f.endsWith(".jsonl"))
    .map((f: string) => ({
      name: f,
      path: resolve(projectDir, f),
      mtime: fs.statSync(resolve(projectDir, f)).mtime
    }))
    .sort((a: any, b: any) => b.mtime - a.mtime);

  if (files.length === 0) {
    throw new Error(`No session logs found in ${projectDir}`);
  }

  return files[0].path;
}

/**
 * Extract context from session log JSONL file
 * Reads recent messages and extracts actionable context
 */
export function extractSessionContext(
  sessionLogPath: string,
  options: {
    recentMessageCount?: number;
    keywords?: string[];
  } = {}
): SessionContext {
  const { recentMessageCount = 100, keywords = [] } = options;

  if (!existsSync(sessionLogPath)) {
    throw new Error(`Session log not found: ${sessionLogPath}`);
  }

  const content = readFileSync(sessionLogPath, "utf-8");
  const lines = content.trim().split("\n");

  // Parse recent messages
  const recentLines = lines.slice(-recentMessageCount);
  const messages: SessionMessage[] = recentLines
    .map(line => {
      try {
        return JSON.parse(line) as SessionMessage;
      } catch {
        return null;
      }
    })
    .filter(msg => msg !== null) as SessionMessage[];

  // Extract decisions (look for decision keywords)
  const decisionKeywords = ["decided", "will", "approach", "pattern", "convention"];
  const recentDecisions = messages
    .filter(msg => msg.type === "assistant")
    .flatMap(msg =>
      msg.message.content
        .filter(c => c.type === "text" && c.text)
        .map(c => c.text!)
    )
    .filter(text =>
      decisionKeywords.some(kw => text.toLowerCase().includes(kw))
    )
    .slice(-10); // Last 10 decisions

  // Extract user goals
  const goalKeywords = ["want", "need", "should", "implement", "create", "build"];
  const userGoals = messages
    .filter(msg => msg.type === "user")
    .flatMap(msg =>
      msg.message.content
        .filter(c => c.type === "text" && c.text)
        .map(c => c.text!)
    )
    .filter(text =>
      goalKeywords.some(kw => text.toLowerCase().includes(kw))
    )
    .slice(-10);

  // Extract active files from tool calls
  const activeFiles = new Set<string>();
  const toolCalls = new Map<string, number>();

  messages
    .filter(msg => msg.type === "assistant")
    .forEach(msg => {
      msg.message.content
        .filter(c => c.type === "tool_use" && c.name)
        .forEach(c => {
          // Count tool usage
          const count = toolCalls.get(c.name!) || 0;
          toolCalls.set(c.name!, count + 1);

          // Extract file paths from Edit/Write/Read tools
          if (["Edit", "Write", "Read"].includes(c.name!) && c.input?.file_path) {
            activeFiles.add(c.input.file_path);
          }
        });
    });

  // Extract technical constraints
  const constraintKeywords = ["must", "should", "constraint", "requirement", "cannot"];
  const technicalConstraints = messages
    .filter(msg => msg && msg.message && msg.message.content)
    .flatMap(msg =>
      msg.message.content
        .filter(c => c.type === "text" && c.text)
        .map(c => c.text!)
    )
    .filter(text =>
      constraintKeywords.some(kw => text.toLowerCase().includes(kw))
    )
    .slice(-10);

  // Extract project patterns
  const patternKeywords = ["pattern", "convention", "style", "format", "standard"];
  const projectPatterns = messages
    .filter(msg => msg && msg.message && msg.message.content)
    .flatMap(msg =>
      msg.message.content
        .filter(c => c.type === "text" && c.text)
        .map(c => c.text!)
    )
    .filter(text =>
      patternKeywords.some(kw => text.toLowerCase().includes(kw))
    )
    .slice(-10);

  // Convert tool calls to summary
  const recentToolCalls: ToolCallSummary[] = Array.from(toolCalls.entries())
    .map(([name, count]) => ({
      name,
      count,
      lastUsed: "recent" // Could extract actual timestamp if needed
    }))
    .sort((a, b) => b.count - a.count)
    .slice(0, 10);

  return {
    recentDecisions,
    activeFiles: Array.from(activeFiles).slice(-20),
    userGoals,
    technicalConstraints,
    projectPatterns,
    recentToolCalls
  };
}

/**
 * Search session log for specific keywords
 */
export function searchSessionLog(
  sessionLogPath: string,
  keywords: string[],
  options: {
    maxResults?: number;
    contextLines?: number;
  } = {}
): Array<{ text: string; timestamp: string; type: "user" | "assistant" }> {
  const { maxResults = 20, contextLines = 0 } = options;

  if (!existsSync(sessionLogPath)) {
    throw new Error(`Session log not found: ${sessionLogPath}`);
  }

  const content = readFileSync(sessionLogPath, "utf-8");
  const lines = content.trim().split("\n");

  const results: Array<{ text: string; timestamp: string; type: "user" | "assistant" }> = [];

  lines.forEach(line => {
    try {
      const msg = JSON.parse(line) as SessionMessage;

      msg.message.content
        .filter(c => c.type === "text" && c.text)
        .forEach(c => {
          const text = c.text!;
          const matchesKeyword = keywords.some(kw =>
            text.toLowerCase().includes(kw.toLowerCase())
          );

          if (matchesKeyword && results.length < maxResults) {
            results.push({
              text,
              timestamp: msg.timestamp,
              type: msg.type
            });
          }
        });
    } catch {
      // Skip invalid lines
    }
  });

  return results;
}

/**
 * Format session context for agent prompt
 */
export function formatContextForPrompt(context: SessionContext): string {
  const sections: string[] = [];

  if (context.userGoals.length > 0) {
    sections.push("## Recent User Goals\n\n" +
      context.userGoals.map((g, i) => `${i + 1}. ${g.substring(0, 200)}`).join("\n"));
  }

  if (context.recentDecisions.length > 0) {
    sections.push("## Recent Technical Decisions\n\n" +
      context.recentDecisions.map((d, i) => `${i + 1}. ${d.substring(0, 200)}`).join("\n"));
  }

  if (context.activeFiles.length > 0) {
    sections.push("## Active Files\n\n" +
      context.activeFiles.map(f => `- ${f}`).join("\n"));
  }

  if (context.projectPatterns.length > 0) {
    sections.push("## Project Patterns\n\n" +
      context.projectPatterns.map((p, i) => `${i + 1}. ${p.substring(0, 200)}`).join("\n"));
  }

  if (context.technicalConstraints.length > 0) {
    sections.push("## Technical Constraints\n\n" +
      context.technicalConstraints.map((c, i) => `${i + 1}. ${c.substring(0, 200)}`).join("\n"));
  }

  if (context.recentToolCalls.length > 0) {
    sections.push("## Recent Tool Usage\n\n" +
      context.recentToolCalls.map(tc => `- ${tc.name}: ${tc.count} calls`).join("\n"));
  }

  return sections.join("\n\n");
}
