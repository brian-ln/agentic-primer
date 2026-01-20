/**
 * Tests for CLARIFICATION_NEEDED Protocol
 */

import { test, expect, describe, beforeEach } from "bun:test";
import { rmSync, existsSync } from "fs";
import { resolve } from "path";
import {
  writeClarificationNeeded,
  readClarificationNeeded,
  writeClarificationResponse,
  readClarificationResponse,
  waitForClarificationResponse,
  getClarificationDir,
  type ClarificationNeeded,
  type ClarificationResponse
} from "./clarification-protocol.ts";

describe("CLARIFICATION_NEEDED Protocol", () => {
  const testAgentId = "test_agent_123";

  beforeEach(() => {
    // Clean up clarification files
    const clarDir = getClarificationDir();
    const neededPath = resolve(clarDir, `${testAgentId}.needed.yaml`);
    const responsePath = resolve(clarDir, `${testAgentId}.response.yaml`);

    if (existsSync(neededPath)) {
      rmSync(neededPath);
    }

    if (existsSync(responsePath)) {
      rmSync(responsePath);
    }
  });

  test("writeClarificationNeeded - creates valid YAML", () => {
    const clarification: ClarificationNeeded = {
      agent_id: testAgentId,
      timestamp: "2026-01-18T08:00:00Z",
      blocked_at: "Session context extraction",
      reason: "Need to know success metrics",
      questions: [
        {
          question_id: "Q1",
          text: "What is the primary optimization goal?",
          context: "Need to know whether to prioritize speed or accuracy",
          type: "blocking",
          options: ["Speed", "Accuracy", "Both"],
          current_assumption: "Prioritize speed"
        }
      ],
      can_resume_with: "Q1 answer",
      current_state: "Completed:\n- Session log discovery\nBlocked:\n- Implementation",
      work_continues: true,
      parallel_work_available: "Can continue with:\n- Documentation"
    };

    const filePath = writeClarificationNeeded(clarification);

    expect(existsSync(filePath)).toBe(true);
  });

  test("readClarificationNeeded - parses YAML correctly", () => {
    const original: ClarificationNeeded = {
      agent_id: testAgentId,
      timestamp: "2026-01-18T08:00:00Z",
      blocked_at: "Session context extraction",
      reason: "Need to know success metrics",
      questions: [
        {
          question_id: "Q1",
          text: "What is the primary optimization goal?",
          context: "Need to know whether to prioritize speed or accuracy",
          type: "blocking"
        },
        {
          question_id: "Q2",
          text: "Should I use TypeScript?",
          context: "Project uses TypeScript",
          current_assumption: "Yes, use TypeScript"
        }
      ],
      can_resume_with: "Q1 answer",
      current_state: "Completed:\n- Session log discovery\nBlocked:\n- Implementation",
      work_continues: false
    };

    writeClarificationNeeded(original);

    const parsed = readClarificationNeeded(testAgentId);

    expect(parsed).not.toBeNull();
    expect(parsed!.agent_id).toBe(testAgentId);
    expect(parsed!.blocked_at).toBe("Session context extraction");
    expect(parsed!.reason).toBe("Need to know success metrics");
    expect(parsed!.questions.length).toBe(2);
    expect(parsed!.questions[0].question_id).toBe("Q1");
    expect(parsed!.questions[0].text).toBe("What is the primary optimization goal?");
    expect(parsed!.questions[1].question_id).toBe("Q2");
    expect(parsed!.questions[1].current_assumption).toBe("Yes, use TypeScript");
    expect(parsed!.can_resume_with).toBe("Q1 answer");
    expect(parsed!.work_continues).toBe(false);
  });

  test("writeClarificationResponse - creates valid YAML", () => {
    const response: ClarificationResponse = {
      agent_id: testAgentId,
      timestamp: "2026-01-18T08:05:00Z",
      resume_signal: true,
      responses: [
        {
          question_id: "Q1",
          answer: "Prioritize speed",
          context: "User wants <10 second handoff",
          reasoning: "Fast handoff is critical"
        }
      ]
    };

    const filePath = writeClarificationResponse(response);

    expect(existsSync(filePath)).toBe(true);
  });

  test("readClarificationResponse - parses YAML correctly", () => {
    const original: ClarificationResponse = {
      agent_id: testAgentId,
      timestamp: "2026-01-18T08:05:00Z",
      resume_signal: true,
      responses: [
        {
          question_id: "Q1",
          answer: "Prioritize speed"
        },
        {
          question_id: "Q2",
          answer: "Yes, use TypeScript",
          context: "Project standard"
        }
      ]
    };

    writeClarificationResponse(original);

    const parsed = readClarificationResponse(testAgentId);

    expect(parsed).not.toBeNull();
    expect(parsed!.agent_id).toBe(testAgentId);
    expect(parsed!.resume_signal).toBe(true);
    expect(parsed!.responses.length).toBe(2);
    expect(parsed!.responses[0].question_id).toBe("Q1");
    expect(parsed!.responses[0].answer).toBe("Prioritize speed");
    expect(parsed!.responses[1].question_id).toBe("Q2");
    expect(parsed!.responses[1].context).toBe("Project standard");
  });

  test("readClarificationNeeded - returns null when file doesn't exist", () => {
    const result = readClarificationNeeded("nonexistent_agent");

    expect(result).toBeNull();
  });

  test("readClarificationResponse - returns null when file doesn't exist", () => {
    const result = readClarificationResponse("nonexistent_agent");

    expect(result).toBeNull();
  });

  test("waitForClarificationResponse - resolves when response arrives", async () => {
    const responsePromise = waitForClarificationResponse(testAgentId, {
      pollInterval: 100,
      timeout: 5000
    });

    // Simulate parent sending response after 500ms
    setTimeout(() => {
      const response: ClarificationResponse = {
        agent_id: testAgentId,
        timestamp: new Date().toISOString(),
        resume_signal: true,
        responses: [
          {
            question_id: "Q1",
            answer: "Test answer"
          }
        ]
      };

      writeClarificationResponse(response);
    }, 500);

    const response = await responsePromise;

    expect(response).toBeDefined();
    expect(response.agent_id).toBe(testAgentId);
    expect(response.responses[0].answer).toBe("Test answer");
  });

  test("waitForClarificationResponse - times out", async () => {
    const promise = waitForClarificationResponse(testAgentId, {
      pollInterval: 100,
      timeout: 500
    });

    await expect(promise).rejects.toThrow("timeout");
  });

  test("roundtrip - write needed, read needed, write response, read response", () => {
    // Agent writes clarification needed
    const clarification: ClarificationNeeded = {
      agent_id: testAgentId,
      timestamp: "2026-01-18T08:00:00Z",
      blocked_at: "Implementation",
      reason: "Need configuration",
      questions: [
        {
          question_id: "Q1",
          text: "Use TypeScript?",
          context: "Language choice"
        }
      ],
      can_resume_with: "Q1",
      current_state: "Blocked",
      work_continues: false
    };

    writeClarificationNeeded(clarification);

    // Parent reads clarification
    const readClarification = readClarificationNeeded(testAgentId);
    expect(readClarification).not.toBeNull();
    expect(readClarification!.questions[0].text).toBe("Use TypeScript?");

    // Parent writes response
    const response: ClarificationResponse = {
      agent_id: testAgentId,
      timestamp: "2026-01-18T08:01:00Z",
      resume_signal: true,
      responses: [
        {
          question_id: "Q1",
          answer: "Yes"
        }
      ]
    };

    writeClarificationResponse(response);

    // Agent reads response
    const readResponse = readClarificationResponse(testAgentId);
    expect(readResponse).not.toBeNull();
    expect(readResponse!.responses[0].answer).toBe("Yes");
  });
});
