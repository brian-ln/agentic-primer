/**
 * Tests for Session Log Utilities
 */

import { test, expect, describe, beforeEach } from "bun:test";
import { writeFileSync, mkdirSync, rmSync, existsSync } from "fs";
import { resolve } from "path";
import {
  extractSessionContext,
  searchSessionLog,
  formatContextForPrompt,
  type SessionMessage
} from "./session-log.ts";

const TEST_DIR = resolve(process.cwd(), "test-data", "session-logs");
const TEST_SESSION_FILE = resolve(TEST_DIR, "test-session.jsonl");

describe("Session Log Utilities", () => {
  beforeEach(() => {
    // Clean up
    if (existsSync(TEST_DIR)) {
      rmSync(TEST_DIR, { recursive: true, force: true });
    }

    // Create test directory
    mkdirSync(TEST_DIR, { recursive: true });
  });

  test("extractSessionContext - extracts user goals", () => {
    // Create test session log
    const messages: SessionMessage[] = [
      {
        type: "user",
        message: {
          role: "user",
          content: [
            { type: "text", text: "I want to implement a background agent workflow" }
          ]
        },
        timestamp: "2026-01-18T08:00:00Z"
      },
      {
        type: "assistant",
        message: {
          role: "assistant",
          content: [
            { type: "text", text: "I'll help you implement that." }
          ]
        },
        timestamp: "2026-01-18T08:00:05Z"
      },
      {
        type: "user",
        message: {
          role: "user",
          content: [
            { type: "text", text: "We need to create a fast handoff system" }
          ]
        },
        timestamp: "2026-01-18T08:01:00Z"
      }
    ];

    writeFileSync(
      TEST_SESSION_FILE,
      messages.map(m => JSON.stringify(m)).join("\n"),
      "utf-8"
    );

    const context = extractSessionContext(TEST_SESSION_FILE, {
      recentMessageCount: 10
    });

    expect(context.userGoals.length).toBeGreaterThan(0);
    expect(context.userGoals.some(g => g.includes("implement"))).toBe(true);
    expect(context.userGoals.some(g => g.includes("create"))).toBe(true);
  });

  test("extractSessionContext - extracts decisions", () => {
    const messages: SessionMessage[] = [
      {
        type: "assistant",
        message: {
          role: "assistant",
          content: [
            { type: "text", text: "We decided to use the Task tool for background execution" }
          ]
        },
        timestamp: "2026-01-18T08:00:00Z"
      },
      {
        type: "assistant",
        message: {
          role: "assistant",
          content: [
            { type: "text", text: "The approach will be to read session logs autonomously" }
          ]
        },
        timestamp: "2026-01-18T08:01:00Z"
      }
    ];

    writeFileSync(
      TEST_SESSION_FILE,
      messages.map(m => JSON.stringify(m)).join("\n"),
      "utf-8"
    );

    const context = extractSessionContext(TEST_SESSION_FILE);

    expect(context.recentDecisions.length).toBeGreaterThan(0);
    expect(context.recentDecisions.some(d => d.includes("decided"))).toBe(true);
    expect(context.recentDecisions.some(d => d.includes("approach"))).toBe(true);
  });

  test("extractSessionContext - extracts active files", () => {
    const messages: SessionMessage[] = [
      {
        type: "assistant",
        message: {
          role: "assistant",
          content: [
            {
              type: "tool_use",
              name: "Edit",
              input: { file_path: "/path/to/file1.ts" }
            }
          ]
        },
        timestamp: "2026-01-18T08:00:00Z"
      },
      {
        type: "assistant",
        message: {
          role: "assistant",
          content: [
            {
              type: "tool_use",
              name: "Write",
              input: { file_path: "/path/to/file2.ts" }
            }
          ]
        },
        timestamp: "2026-01-18T08:01:00Z"
      }
    ];

    writeFileSync(
      TEST_SESSION_FILE,
      messages.map(m => JSON.stringify(m)).join("\n"),
      "utf-8"
    );

    const context = extractSessionContext(TEST_SESSION_FILE);

    expect(context.activeFiles).toContain("/path/to/file1.ts");
    expect(context.activeFiles).toContain("/path/to/file2.ts");
  });

  test("extractSessionContext - counts tool calls", () => {
    const messages: SessionMessage[] = [
      {
        type: "assistant",
        message: {
          role: "assistant",
          content: [
            { type: "tool_use", name: "Edit", input: {} },
            { type: "tool_use", name: "Edit", input: {} },
            { type: "tool_use", name: "Read", input: {} }
          ]
        },
        timestamp: "2026-01-18T08:00:00Z"
      }
    ];

    writeFileSync(
      TEST_SESSION_FILE,
      messages.map(m => JSON.stringify(m)).join("\n"),
      "utf-8"
    );

    const context = extractSessionContext(TEST_SESSION_FILE);

    const editCalls = context.recentToolCalls.find(tc => tc.name === "Edit");
    const readCalls = context.recentToolCalls.find(tc => tc.name === "Read");

    expect(editCalls?.count).toBe(2);
    expect(readCalls?.count).toBe(1);
  });

  test("searchSessionLog - finds matching keywords", () => {
    const messages: SessionMessage[] = [
      {
        type: "user",
        message: {
          role: "user",
          content: [
            { type: "text", text: "Implement the clarification protocol" }
          ]
        },
        timestamp: "2026-01-18T08:00:00Z"
      },
      {
        type: "assistant",
        message: {
          role: "assistant",
          content: [
            { type: "text", text: "The clarification protocol will use YAML format" }
          ]
        },
        timestamp: "2026-01-18T08:00:05Z"
      },
      {
        type: "user",
        message: {
          role: "user",
          content: [
            { type: "text", text: "What about session logs?" }
          ]
        },
        timestamp: "2026-01-18T08:01:00Z"
      }
    ];

    writeFileSync(
      TEST_SESSION_FILE,
      messages.map(m => JSON.stringify(m)).join("\n"),
      "utf-8"
    );

    const results = searchSessionLog(TEST_SESSION_FILE, ["clarification"]);

    expect(results.length).toBe(2);
    expect(results[0].text).toContain("clarification");
    expect(results[1].text).toContain("clarification");
  });

  test("formatContextForPrompt - creates readable output", () => {
    const context = {
      userGoals: ["Implement background workflow", "Make it fast"],
      recentDecisions: ["Use Task tool", "Read session logs"],
      activeFiles: ["/path/to/file1.ts", "/path/to/file2.ts"],
      projectPatterns: ["Use TypeScript", "Follow actor pattern"],
      technicalConstraints: ["Must be <10 seconds", "Cannot block parent"],
      recentToolCalls: [
        { name: "Edit", count: 5, lastUsed: "recent" },
        { name: "Read", count: 3, lastUsed: "recent" }
      ]
    };

    const formatted = formatContextForPrompt(context);

    expect(formatted).toContain("## Recent User Goals");
    expect(formatted).toContain("Implement background workflow");
    expect(formatted).toContain("## Recent Technical Decisions");
    expect(formatted).toContain("Use Task tool");
    expect(formatted).toContain("## Active Files");
    expect(formatted).toContain("/path/to/file1.ts");
    expect(formatted).toContain("## Recent Tool Usage");
    expect(formatted).toContain("Edit: 5 calls");
  });

  test("extractSessionContext - handles empty log", () => {
    writeFileSync(TEST_SESSION_FILE, "", "utf-8");

    const context = extractSessionContext(TEST_SESSION_FILE);

    expect(context.userGoals).toEqual([]);
    expect(context.recentDecisions).toEqual([]);
    expect(context.activeFiles).toEqual([]);
  });

  test("extractSessionContext - handles malformed JSON", () => {
    writeFileSync(
      TEST_SESSION_FILE,
      '{"valid": "json"}\n{invalid json}\n{"valid": "json"}',
      "utf-8"
    );

    const context = extractSessionContext(TEST_SESSION_FILE);

    // Should skip malformed line and process valid ones
    expect(context).toBeDefined();
  });
});
