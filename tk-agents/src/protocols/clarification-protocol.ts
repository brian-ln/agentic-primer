/**
 * CLARIFICATION_NEEDED Protocol
 *
 * Structured protocol for background agents to request clarification from parent agents/users.
 * Enables async, non-blocking context gathering when session logs are insufficient.
 */

import { writeFileSync, readFileSync, existsSync, mkdirSync, watch } from "fs";
import { resolve, dirname } from "path";

export interface ClarificationQuestion {
  question_id: string; // Q1, Q2, etc.
  text: string;
  context: string; // Why this matters
  type?: "blocking" | "optional" | "required";
  options?: string[]; // For multiple choice
  current_assumption?: string; // What agent will assume if no answer
}

export interface ClarificationNeeded {
  agent_id: string;
  timestamp: string; // ISO-8601
  blocked_at: string; // Where agent stopped
  reason: string; // Why clarification needed
  questions: ClarificationQuestion[];
  can_resume_with: string; // What info unblocks
  current_state: string; // What's completed vs blocked
  work_continues?: boolean; // Can do parallel work
  parallel_work_available?: string;
}

export interface ClarificationAnswer {
  question_id: string;
  answer: string;
  context?: string;
  reasoning?: string;
}

export interface ClarificationResponse {
  agent_id: string;
  timestamp: string;
  resume_signal: boolean;
  responses: ClarificationAnswer[];
}

/**
 * Get clarification directory path
 */
export function getClarificationDir(): string {
  const tmpDir = process.env.TMPDIR || "/tmp";
  const clarDir = resolve(tmpDir, "claude", "clarifications");

  if (!existsSync(clarDir)) {
    mkdirSync(clarDir, { recursive: true });
  }

  return clarDir;
}

/**
 * Write CLARIFICATION_NEEDED signal
 * Agent calls this when it needs user input
 */
export function writeClarificationNeeded(
  clarification: ClarificationNeeded
): string {
  const clarDir = getClarificationDir();
  const filePath = resolve(clarDir, `${clarification.agent_id}.needed.yaml`);

  // Format as YAML
  const yaml = formatClarificationNeededAsYAML(clarification);

  writeFileSync(filePath, yaml, "utf-8");

  console.log(`[CLARIFICATION_NEEDED] Written to ${filePath}`);

  return filePath;
}

/**
 * Read CLARIFICATION_NEEDED signal
 * Parent agent calls this to check for clarification requests
 */
export function readClarificationNeeded(agentId: string): ClarificationNeeded | null {
  const clarDir = getClarificationDir();
  const filePath = resolve(clarDir, `${agentId}.needed.yaml`);

  if (!existsSync(filePath)) {
    return null;
  }

  const yaml = readFileSync(filePath, "utf-8");
  return parseClarificationNeededFromYAML(yaml);
}

/**
 * Write CLARIFICATION_RESPONSE signal
 * Parent agent calls this to send answers back to agent
 */
export function writeClarificationResponse(
  response: ClarificationResponse
): string {
  const clarDir = getClarificationDir();
  const filePath = resolve(clarDir, `${response.agent_id}.response.yaml`);

  const yaml = formatClarificationResponseAsYAML(response);

  writeFileSync(filePath, yaml, "utf-8");

  console.log(`[CLARIFICATION_RESPONSE] Written to ${filePath}`);

  return filePath;
}

/**
 * Read CLARIFICATION_RESPONSE signal
 * Agent calls this to get answers from parent
 */
export function readClarificationResponse(agentId: string): ClarificationResponse | null {
  const clarDir = getClarificationDir();
  const filePath = resolve(clarDir, `${agentId}.response.yaml`);

  if (!existsSync(filePath)) {
    return null;
  }

  const yaml = readFileSync(filePath, "utf-8");
  return parseClarificationResponseFromYAML(yaml);
}

/**
 * Wait for clarification response with polling
 * Agent calls this to block until parent responds
 */
export async function waitForClarificationResponse(
  agentId: string,
  options: {
    pollInterval?: number; // ms
    timeout?: number; // ms
    onPoll?: () => void;
  } = {}
): Promise<ClarificationResponse> {
  const { pollInterval = 5000, timeout = 300000, onPoll } = options; // Default 5s poll, 5min timeout

  const startTime = Date.now();

  while (true) {
    const response = readClarificationResponse(agentId);

    if (response && response.resume_signal) {
      return response;
    }

    if (Date.now() - startTime > timeout) {
      throw new Error(`Clarification response timeout after ${timeout}ms`);
    }

    if (onPoll) {
      onPoll();
    }

    await new Promise(resolve => setTimeout(resolve, pollInterval));
  }
}

/**
 * Monitor for clarification requests (parent agent side)
 */
export async function monitorClarifications(
  agentIds: string[],
  onClarification: (clarification: ClarificationNeeded) => Promise<ClarificationResponse>
): Promise<void> {
  const clarDir = getClarificationDir();

  // Poll for new clarification files
  const checkInterval = 2000; // 2 seconds

  const checkedAgents = new Set<string>();

  while (true) {
    for (const agentId of agentIds) {
      if (checkedAgents.has(agentId)) {
        continue; // Already handled
      }

      const clarification = readClarificationNeeded(agentId);

      if (clarification) {
        console.log(`[MONITOR] Detected clarification request from ${agentId}`);

        // Handle clarification
        const response = await onClarification(clarification);

        // Write response
        writeClarificationResponse(response);

        checkedAgents.add(agentId);
      }
    }

    await new Promise(resolve => setTimeout(resolve, checkInterval));
  }
}

/**
 * Format CLARIFICATION_NEEDED as YAML
 */
function formatClarificationNeededAsYAML(clarification: ClarificationNeeded): string {
  const lines = ["[CLARIFICATION_NEEDED]"];

  lines.push(`agent_id: ${clarification.agent_id}`);
  lines.push(`timestamp: ${clarification.timestamp}`);
  lines.push(`blocked_at: "${clarification.blocked_at}"`);
  lines.push(`reason: "${clarification.reason}"`);
  lines.push("");
  lines.push("questions:");

  clarification.questions.forEach(q => {
    lines.push(`  - question_id: ${q.question_id}`);
    lines.push(`    text: "${q.text}"`);
    lines.push(`    context: |`);
    q.context.split("\n").forEach(line => {
      lines.push(`      ${line}`);
    });

    if (q.type) {
      lines.push(`    type: ${q.type}`);
    }

    if (q.options && q.options.length > 0) {
      lines.push(`    options:`);
      q.options.forEach(opt => {
        lines.push(`      - "${opt}"`);
      });
    }

    if (q.current_assumption) {
      lines.push(`    current_assumption: "${q.current_assumption}"`);
    }

    lines.push("");
  });

  lines.push(`can_resume_with: "${clarification.can_resume_with}"`);
  lines.push("current_state: |");
  clarification.current_state.split("\n").forEach(line => {
    lines.push(`  ${line}`);
  });

  if (clarification.work_continues !== undefined) {
    lines.push(`work_continues: ${clarification.work_continues}`);
  }

  if (clarification.parallel_work_available) {
    lines.push("parallel_work_available: |");
    clarification.parallel_work_available.split("\n").forEach(line => {
      lines.push(`  ${line}`);
    });
  }

  lines.push("[/CLARIFICATION_NEEDED]");

  return lines.join("\n");
}

/**
 * Parse CLARIFICATION_NEEDED from YAML
 */
function parseClarificationNeededFromYAML(yaml: string): ClarificationNeeded {
  // Simple parser for our YAML format
  const lines = yaml.split("\n");

  const clarification: Partial<ClarificationNeeded> = {
    questions: []
  };

  let currentQuestion: Partial<ClarificationQuestion> | null = null;
  let inContext = false;
  let inCurrentState = false;
  let inParallelWork = false;
  let contextLines: string[] = [];
  let stateLines: string[] = [];
  let parallelLines: string[] = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.trim() === "[CLARIFICATION_NEEDED]" || line.trim() === "[/CLARIFICATION_NEEDED]") {
      continue;
    }

    if (line.startsWith("agent_id:")) {
      clarification.agent_id = line.split(":")[1].trim();
    } else if (line.startsWith("timestamp:")) {
      clarification.timestamp = line.split(":")[1].trim();
    } else if (line.startsWith("blocked_at:")) {
      clarification.blocked_at = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
    } else if (line.startsWith("reason:")) {
      clarification.reason = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
    } else if (line.trim() === "questions:") {
      // Start questions section
    } else if (line.trim().startsWith("- question_id:")) {
      // Save previous question
      if (currentQuestion && currentQuestion.question_id) {
        if (inContext) {
          currentQuestion.context = contextLines.join("\n");
        }
        clarification.questions!.push(currentQuestion as ClarificationQuestion);
      }

      // Start new question
      currentQuestion = {
        question_id: line.split(":")[1].trim()
      };
      inContext = false;
      contextLines = [];
    } else if (line.trim().startsWith("text:")) {
      if (currentQuestion) {
        currentQuestion.text = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
      }
    } else if (line.trim() === "context: |") {
      inContext = true;
    } else if (inContext && line.startsWith("      ")) {
      contextLines.push(line.substring(6));
    } else if (line.trim().startsWith("type:")) {
      if (currentQuestion) {
        currentQuestion.type = line.split(":")[1].trim() as any;
        inContext = false;
      }
    } else if (line.trim().startsWith("current_assumption:")) {
      if (currentQuestion) {
        currentQuestion.current_assumption = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
      }
    } else if (line.startsWith("can_resume_with:")) {
      if (currentQuestion && currentQuestion.question_id) {
        if (inContext) {
          currentQuestion.context = contextLines.join("\n");
        }
        clarification.questions!.push(currentQuestion as ClarificationQuestion);
        currentQuestion = null;
        inContext = false;
      }

      clarification.can_resume_with = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
    } else if (line.trim() === "current_state: |") {
      inCurrentState = true;
    } else if (inCurrentState && line.startsWith("  ")) {
      stateLines.push(line.substring(2));
    } else if (line.startsWith("work_continues:")) {
      if (inCurrentState) {
        clarification.current_state = stateLines.join("\n");
        inCurrentState = false;
      }
      clarification.work_continues = line.split(":")[1].trim() === "true";
    } else if (line.trim() === "parallel_work_available: |") {
      inParallelWork = true;
    } else if (inParallelWork && line.startsWith("  ")) {
      parallelLines.push(line.substring(2));
    }
  }

  // Finalize
  if (inCurrentState) {
    clarification.current_state = stateLines.join("\n");
  }
  if (inParallelWork) {
    clarification.parallel_work_available = parallelLines.join("\n");
  }

  return clarification as ClarificationNeeded;
}

/**
 * Format CLARIFICATION_RESPONSE as YAML
 */
function formatClarificationResponseAsYAML(response: ClarificationResponse): string {
  const lines = ["[CLARIFICATION_RESPONSE]"];

  lines.push(`agent_id: ${response.agent_id}`);
  lines.push(`timestamp: ${response.timestamp}`);
  lines.push(`resume_signal: ${response.resume_signal}`);
  lines.push("");
  lines.push("responses:");

  response.responses.forEach(r => {
    lines.push(`  - question_id: ${r.question_id}`);
    lines.push(`    answer: "${r.answer}"`);

    if (r.context) {
      lines.push(`    context: "${r.context}"`);
    }

    if (r.reasoning) {
      lines.push(`    reasoning: "${r.reasoning}"`);
    }

    lines.push("");
  });

  lines.push("[/CLARIFICATION_RESPONSE]");

  return lines.join("\n");
}

/**
 * Parse CLARIFICATION_RESPONSE from YAML
 */
function parseClarificationResponseFromYAML(yaml: string): ClarificationResponse {
  const lines = yaml.split("\n");

  const response: Partial<ClarificationResponse> = {
    responses: []
  };

  let currentResponse: Partial<ClarificationAnswer> | null = null;

  for (const line of lines) {
    if (line.trim() === "[CLARIFICATION_RESPONSE]" || line.trim() === "[/CLARIFICATION_RESPONSE]") {
      continue;
    }

    if (line.startsWith("agent_id:")) {
      response.agent_id = line.split(":")[1].trim();
    } else if (line.startsWith("timestamp:")) {
      response.timestamp = line.split(":")[1].trim();
    } else if (line.startsWith("resume_signal:")) {
      response.resume_signal = line.split(":")[1].trim() === "true";
    } else if (line.trim() === "responses:") {
      // Start responses section
    } else if (line.trim().startsWith("- question_id:")) {
      // Save previous response
      if (currentResponse && currentResponse.question_id) {
        response.responses!.push(currentResponse as ClarificationAnswer);
      }

      // Start new response
      currentResponse = {
        question_id: line.split(":")[1].trim()
      };
    } else if (line.trim().startsWith("answer:")) {
      if (currentResponse) {
        currentResponse.answer = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
      }
    } else if (line.trim().startsWith("context:")) {
      if (currentResponse) {
        currentResponse.context = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
      }
    } else if (line.trim().startsWith("reasoning:")) {
      if (currentResponse) {
        currentResponse.reasoning = line.split(":").slice(1).join(":").trim().replace(/^"(.*)"$/, "$1");
      }
    }
  }

  // Save last response
  if (currentResponse && currentResponse.question_id) {
    response.responses!.push(currentResponse as ClarificationAnswer);
  }

  return response as ClarificationResponse;
}
