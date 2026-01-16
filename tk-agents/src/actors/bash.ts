// BashActor - Deterministic actor that runs shell commands

import { spawn } from "bun";
import type { Actor, Message, Response } from "./base";
import { withTiming } from "./base";

export interface BashActorConfig {
  id: string;
  cwd?: string;
  env?: Record<string, string>;
  timeout?: number; // ms
}

export class BashActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;

  private config: BashActorConfig;

  constructor(config: BashActorConfig) {
    this.id = config.id;
    this.config = config;
  }

  async send(message: Message): Promise<Response> {
    // Handle ping for heartbeat
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Payload should be the command string or {command: string, args?: string[]}
    let command: string;
    let args: string[] = [];

    if (typeof message.payload === "string") {
      command = message.payload;
    } else if (typeof message.payload === "object" && message.payload !== null) {
      const p = message.payload as { command?: string; args?: string[] };
      command = p.command || "";
      args = p.args || [];
    } else {
      return { success: false, error: "Invalid payload: expected string or {command, args}" };
    }

    if (!command) {
      return { success: false, error: "No command provided" };
    }

    const { result, durationMs } = await withTiming(async () => {
      const proc = spawn(["bash", "-c", command, ...args], {
        cwd: this.config.cwd,
        env: { ...process.env, ...this.config.env },
        stdout: "pipe",
        stderr: "pipe",
      });

      // Handle timeout
      let timedOut = false;
      const timeoutId = this.config.timeout
        ? setTimeout(() => {
            timedOut = true;
            proc.kill();
          }, this.config.timeout)
        : null;

      const stdoutChunks: Uint8Array[] = [];
      const stderrChunks: Uint8Array[] = [];

      // Read streams
      const readStdout = async () => {
        for await (const chunk of proc.stdout) {
          stdoutChunks.push(chunk);
        }
      };
      const readStderr = async () => {
        for await (const chunk of proc.stderr) {
          stderrChunks.push(chunk);
        }
      };

      await Promise.all([readStdout(), readStderr()]);
      const exitCode = await proc.exited;

      const decoder = new TextDecoder();
      const stdout = decoder.decode(Buffer.concat(stdoutChunks));
      const stderr = decoder.decode(Buffer.concat(stderrChunks));

      if (timeoutId) clearTimeout(timeoutId);

      return { stdout, stderr, exitCode, timedOut };
    });

    if (result.timedOut) {
      return {
        success: false,
        error: `Command timed out after ${this.config.timeout}ms`,
        metadata: { durationMs },
      };
    }

    if (result.exitCode !== 0) {
      return {
        success: false,
        error: result.stderr || `Exit code ${result.exitCode}`,
        data: { stdout: result.stdout, stderr: result.stderr, exitCode: result.exitCode },
        metadata: { durationMs },
      };
    }

    return {
      success: true,
      data: result.stdout,
      metadata: { durationMs },
    };
  }

  // NEW: Semantically correct method (Hewitt Actor Model)
  // During Phase 2, this delegates to send() for backward compatibility
  async receive(message: Message): Promise<Response> {
    return this.send(message);
  }
}

// Factory
export function createBashActor(config: BashActorConfig): BashActor {
  return new BashActor(config);
}
