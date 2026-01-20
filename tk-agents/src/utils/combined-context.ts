#!/usr/bin/env bun

/**
 * Combined context extraction: Parent session + Agent outputs
 *
 * When monitoring /bg workflow, you need BOTH:
 * 1. Parent session logs (conversation history)
 * 2. Agent output logs (what agents did)
 */

import { extractSessionContext, SessionContext } from "./session-context.ts";
import { existsSync, readdirSync } from "fs";
import { join } from "path";

export interface AgentActivity {
  agentId: string;
  outputPath: string;
  content: string;
  clarifications: ClarificationSignal[];
  completions: CompletionSignal[];
  stopWork: StopWorkSignal[];
  delegations: DelegationSignal[];
}

export interface ClarificationSignal {
  agentId: string;
  timestamp: string;
  blockedAt: string;
  reason: string;
  questions: any[];
}

export interface CompletionSignal {
  agentId: string;
  timestamp: string;
  status: string;
  deliverables: string[];
  summary: string;
}

export interface StopWorkSignal {
  agentId: string;
  timestamp: string;
  stopReason: string;
  blockerType: string;
}

export interface DelegationSignal {
  agentId: string;
  timestamp: string;
  newTaskDescription: string;
}

export interface CombinedContext {
  session: SessionContext;
  agents: AgentActivity[];
  timeRange: {
    start: string;
    end: string;
  };
}

/**
 * Extract combined context from session + agent outputs
 */
export async function extractCombinedContext(
  sessionLogPath: string,
  options: {
    minutesBack?: number;
    includeAgents?: boolean;
  } = {}
): Promise<CombinedContext> {
  const { minutesBack = 30, includeAgents = true } = options;

  // Extract session context
  const sessionContext = await extractSessionContext(sessionLogPath, {
    minutesBack,
  });

  // Find agent output files
  const agentActivities: AgentActivity[] = [];

  if (includeAgents) {
    const agentOutputDir = findAgentOutputDir();
    if (agentOutputDir && existsSync(agentOutputDir)) {
      const outputFiles = readdirSync(agentOutputDir).filter((f) =>
        f.endsWith(".output")
      );

      for (const file of outputFiles) {
        const agentId = file.replace(".output", "");
        const outputPath = join(agentOutputDir, file);
        const activity = await extractAgentActivity(agentId, outputPath);
        agentActivities.push(activity);
      }
    }
  }

  return {
    session: sessionContext,
    agents: agentActivities,
    timeRange: sessionContext.timeRange,
  };
}

/**
 * Find agent output directory
 */
function findAgentOutputDir(): string | null {
  const cwd = process.cwd();
  const projectHash = cwd
    .split("/")
    .filter((p) => p && p !== "Users")
    .join("-");

  const possiblePaths = [
    `/private/tmp/claude/-${projectHash}/tasks`,
    `/tmp/claude/-${projectHash}/tasks`,
    `${process.env.HOME}/.claude/agent-outputs/${projectHash}`,
  ];

  for (const path of possiblePaths) {
    if (existsSync(path)) {
      return path;
    }
  }

  return null;
}

/**
 * Extract activity from agent output file
 */
async function extractAgentActivity(
  agentId: string,
  outputPath: string
): Promise<AgentActivity> {
  const content = await Bun.file(outputPath).text();

  // Extract signals from output
  const clarifications = extractSignals(
    content,
    "CLARIFICATION_NEEDED"
  ) as ClarificationSignal[];
  const completions = extractSignals(
    content,
    "COMPLETION_REPORT"
  ) as CompletionSignal[];
  const stopWork = extractSignals(content, "STOP_WORK") as StopWorkSignal[];
  const delegations = extractSignals(
    content,
    "DELEGATE_WORK"
  ) as DelegationSignal[];

  return {
    agentId,
    outputPath,
    content,
    clarifications,
    completions,
    stopWork,
    delegations,
  };
}

/**
 * Extract protocol signals from agent output
 */
function extractSignals(content: string, signalType: string): any[] {
  const signals: any[] = [];
  const regex = new RegExp(
    `\\[${signalType}\\]([\\s\\S]*?)\\[\\/${signalType}\\]`,
    "g"
  );

  let match;
  while ((match = regex.exec(content)) !== null) {
    const yamlContent = match[1];
    try {
      // Parse YAML-like content (simplified)
      const parsed = parseSimpleYAML(yamlContent);
      signals.push(parsed);
    } catch (err) {
      console.warn(`Failed to parse ${signalType} signal:`, err);
    }
  }

  return signals;
}

/**
 * Simple YAML parser for signal blocks
 */
function parseSimpleYAML(yamlContent: string): any {
  const result: any = {};
  const lines = yamlContent.trim().split("\n");

  let currentKey: string | null = null;
  let currentArray: any[] = [];
  let inArray = false;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;

    // Array item
    if (trimmed.startsWith("- ")) {
      if (inArray) {
        currentArray.push(trimmed.substring(2));
      }
      continue;
    }

    // Key: value
    const colonIndex = trimmed.indexOf(":");
    if (colonIndex > 0) {
      const key = trimmed.substring(0, colonIndex).trim();
      const value = trimmed.substring(colonIndex + 1).trim();

      if (key.endsWith("s") && !value) {
        // Array key (e.g., "questions:")
        currentKey = key;
        currentArray = [];
        inArray = true;
      } else {
        // Simple key-value
        if (inArray && currentKey) {
          result[currentKey] = currentArray;
          inArray = false;
        }
        result[key] = value;
      }
    }
  }

  if (inArray && currentKey) {
    result[currentKey] = currentArray;
  }

  return result;
}

/**
 * CLI usage
 */
if (import.meta.main) {
  const sessionLogPath = process.argv[2];
  const minutesBack = parseInt(process.argv[3] || "30");

  if (!sessionLogPath) {
    console.error(
      "Usage: bun combined-context.ts <session-log-path> [minutes-back]"
    );
    console.error(
      "Example: bun combined-context.ts ~/.claude/projects/.../session.jsonl 30"
    );
    process.exit(1);
  }

  const context = await extractCombinedContext(sessionLogPath, {
    minutesBack,
  });

  console.log("📊 COMBINED CONTEXT (Session + Agents)");
  console.log("─".repeat(80));
  console.log(
    `Time Range: ${context.timeRange.start} → ${context.timeRange.end}`
  );
  console.log();

  // Session context summary
  console.log("📝 SESSION CONTEXT:");
  console.log(`  Messages: ${context.session.recentMessages.length}`);
  console.log(`  Decisions: ${context.session.recentDecisions.length}`);
  console.log(`  Active Files: ${context.session.activeFiles.length}`);
  console.log(`  User Goals: ${context.session.userGoals.length}`);
  console.log();

  // Agent activities
  console.log(`🤖 AGENT ACTIVITIES (${context.agents.length} agents):`);
  console.log();

  for (const agent of context.agents) {
    console.log(`  Agent: ${agent.agentId}`);
    console.log(`  Output: ${agent.outputPath}`);
    console.log(`    Clarifications: ${agent.clarifications.length}`);
    console.log(`    Completions: ${agent.completions.length}`);
    console.log(`    Stop Work: ${agent.stopWork.length}`);
    console.log(`    Delegations: ${agent.delegations.length}`);

    if (agent.clarifications.length > 0) {
      console.log("    ⚠️  Clarification Details:");
      for (const clarification of agent.clarifications) {
        console.log(`      - Blocked at: ${clarification.blockedAt}`);
        console.log(`      - Reason: ${clarification.reason}`);
      }
    }

    if (agent.completions.length > 0) {
      console.log("    ✅ Completion Details:");
      for (const completion of agent.completions) {
        console.log(`      - Status: ${completion.status}`);
        console.log(`      - Summary: ${completion.summary?.substring(0, 80)}...`);
      }
    }

    console.log();
  }

  // Show recent activity timeline
  console.log("⏱️  RECENT ACTIVITY:");
  console.log();

  // Combine session messages and agent signals into timeline
  const timeline: Array<{ time: string; type: string; desc: string }> = [];

  // Add session messages
  for (const msg of context.session.recentMessages.slice(-5)) {
    timeline.push({
      time: msg.timestamp,
      type: msg.type === "user" ? "👤 User" : "🤖 Assistant",
      desc: msg.content.substring(0, 60) + "...",
    });
  }

  // Add agent signals
  for (const agent of context.agents) {
    for (const clarification of agent.clarifications) {
      timeline.push({
        time: clarification.timestamp,
        type: `🔔 Agent ${agent.agentId.substring(0, 7)}`,
        desc: `Clarification: ${clarification.blockedAt}`,
      });
    }
    for (const completion of agent.completions) {
      timeline.push({
        time: completion.timestamp,
        type: `✅ Agent ${agent.agentId.substring(0, 7)}`,
        desc: `Completed: ${completion.status}`,
      });
    }
  }

  // Sort by time and display
  timeline.sort((a, b) => a.time.localeCompare(b.time));
  for (const event of timeline.slice(-10)) {
    const timeStr = new Date(event.time).toLocaleTimeString();
    console.log(`  ${timeStr} | ${event.type} | ${event.desc}`);
  }
}
