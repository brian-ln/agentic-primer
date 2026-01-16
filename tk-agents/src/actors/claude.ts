// ClaudeActor - Spawns Claude CLI with resumable sessions

import { spawn, type Subprocess } from "bun";
import type { Actor, Message, Response, StreamEvent } from "./base";
import { withTiming } from "./base";

export interface ClaudeActorConfig {
  id: string;
  systemPrompt?: string;
  model?: string;
  tools?: string[];
  maxTurns?: number;
  outputFormat?: "text" | "json" | "stream-json";
}

export class ClaudeActor implements Actor {
  readonly id: string;
  readonly type = "agent" as const;

  private config: ClaudeActorConfig;
  private sessionId: string;
  private turnCount = 0;

  constructor(config: ClaudeActorConfig) {
    this.id = config.id;
    this.config = config;
    this.sessionId = crypto.randomUUID();
  }

  // Get the session ID (for chaining or external reference)
  getSessionId(): string {
    return this.sessionId;
  }

  // Build CLI arguments
  private buildArgs(prompt: string, streaming: boolean): string[] {
    const args = ["claude", "-p"];

    // Session management
    args.push("--session-id", this.sessionId);
    if (this.turnCount > 0) {
      args.push("--resume", this.sessionId);
    }

    // Output format
    if (streaming || this.config.outputFormat === "stream-json") {
      args.push("--output-format", "stream-json");
    } else if (this.config.outputFormat === "json") {
      args.push("--output-format", "json");
    }

    // Model
    if (this.config.model) {
      args.push("--model", this.config.model);
    }

    // System prompt (only on first turn)
    if (this.turnCount === 0 && this.config.systemPrompt) {
      args.push("--system-prompt", this.config.systemPrompt);
    }

    // Tools
    if (this.config.tools && this.config.tools.length > 0) {
      args.push("--tools", this.config.tools.join(","));
    }

    // Max turns
    if (this.config.maxTurns) {
      args.push("--max-turns", String(this.config.maxTurns));
    }

    // The prompt itself
    args.push(prompt);

    return args;
  }

  // Simple send - wait for completion
  async send(message: Message): Promise<Response> {
    // Handle ping for heartbeat
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    const prompt = typeof message.payload === "string"
      ? message.payload
      : JSON.stringify(message.payload);

    const args = this.buildArgs(prompt, false);

    const { result, durationMs } = await withTiming(async () => {
      const proc = spawn(args, {
        stdout: "pipe",
        stderr: "pipe",
      });

      const stdout = await new Blob([await proc.stdout]).text();
      const stderr = await new Blob([await proc.stderr]).text();
      const exitCode = await proc.exited;

      return { stdout, stderr, exitCode };
    });

    this.turnCount++;

    if (result.exitCode !== 0) {
      return {
        success: false,
        error: result.stderr || `Exit code ${result.exitCode}`,
        metadata: { durationMs, sessionId: this.sessionId },
      };
    }

    // Try to parse JSON output
    let data: unknown = result.stdout;
    if (this.config.outputFormat === "json") {
      try {
        data = JSON.parse(result.stdout);
      } catch {
        // Keep as string
      }
    }

    return {
      success: true,
      data,
      metadata: { durationMs, sessionId: this.sessionId },
    };
  }

  // NEW: Semantically correct method (Hewitt Actor Model)
  // During Phase 2, this delegates to send() for backward compatibility
  async receive(message: Message): Promise<Response> {
    return this.send(message);
  }

  // Streaming send - yields events as they arrive
  async *stream(message: Message): AsyncGenerator<StreamEvent, Response> {
    const prompt = typeof message.payload === "string"
      ? message.payload
      : JSON.stringify(message.payload);

    const args = this.buildArgs(prompt, true);
    const startTime = Date.now();

    const proc = spawn(args, {
      stdout: "pipe",
      stderr: "pipe",
    });

    const decoder = new TextDecoder();
    let lastResult: unknown = null;
    let errorOutput = "";

    // Read stderr in background
    (async () => {
      for await (const chunk of proc.stderr) {
        errorOutput += decoder.decode(chunk);
      }
    })();

    // Stream stdout events
    let buffer = "";
    for await (const chunk of proc.stdout) {
      buffer += decoder.decode(chunk);

      // Process complete lines
      const lines = buffer.split("\n");
      buffer = lines.pop() || ""; // Keep incomplete line in buffer

      for (const line of lines) {
        if (!line.trim()) continue;

        try {
          const event = JSON.parse(line);
          const streamEvent: StreamEvent = {
            type: event.type || "message",
            data: event,
            timestamp: new Date(),
          };

          // Track result for return value
          if (event.type === "result") {
            lastResult = event;
          }

          yield streamEvent;
        } catch {
          // Non-JSON output, wrap it
          yield {
            type: "message",
            data: { raw: line },
            timestamp: new Date(),
          };
        }
      }
    }

    // Process any remaining buffer
    if (buffer.trim()) {
      try {
        const event = JSON.parse(buffer);
        yield { type: event.type || "message", data: event, timestamp: new Date() };
        if (event.type === "result") lastResult = event;
      } catch {
        yield { type: "message", data: { raw: buffer }, timestamp: new Date() };
      }
    }

    const exitCode = await proc.exited;
    this.turnCount++;

    const durationMs = Date.now() - startTime;

    if (exitCode !== 0) {
      return {
        success: false,
        error: errorOutput || `Exit code ${exitCode}`,
        metadata: { durationMs, sessionId: this.sessionId },
      };
    }

    return {
      success: true,
      data: lastResult,
      metadata: {
        durationMs,
        sessionId: this.sessionId,
        costUsd: (lastResult as { cost_usd?: number })?.cost_usd,
      },
    };
  }

  // Reset session (start fresh)
  resetSession(): void {
    this.sessionId = crypto.randomUUID();
    this.turnCount = 0;
  }
}

// Factory function
export function createClaudeActor(config: ClaudeActorConfig): ClaudeActor {
  return new ClaudeActor(config);
}
