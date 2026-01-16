// ChainedActors - Pipeline of Claude actors (A → B)
// A's output streams directly to B's input

import { spawn } from "bun";
import type { Actor, Message, Response, StreamEvent } from "./base";
import type { ClaudeActorConfig } from "./claude";

export interface ChainConfig {
  id: string;
  actors: [ChainStageConfig, ChainStageConfig]; // Exactly 2 for now
}

export interface ChainStageConfig extends Omit<ClaudeActorConfig, "id" | "outputFormat"> {
  name: string;
  prompt: string; // The instruction for this stage
}

export class ChainedActors implements Actor {
  readonly id: string;
  readonly type = "agent" as const;

  private config: ChainConfig;

  constructor(config: ChainConfig) {
    this.id = config.id;
    this.config = config;
  }

  // Build args for first stage (outputs stream-json)
  private buildFirstStageArgs(inputPrompt: string): string[] {
    const stage = this.config.actors[0];
    const args = ["claude", "-p", "--output-format", "stream-json"];

    if (stage.model) args.push("--model", stage.model);
    if (stage.systemPrompt) args.push("--system-prompt", stage.systemPrompt);
    if (stage.tools?.length) args.push("--tools", stage.tools.join(","));
    if (stage.maxTurns) args.push("--max-turns", String(stage.maxTurns));

    // Combine stage prompt with input
    const fullPrompt = `${stage.prompt}\n\nInput:\n${inputPrompt}`;
    args.push(fullPrompt);

    return args;
  }

  // Build args for second stage (receives stream-json, outputs stream-json)
  private buildSecondStageArgs(): string[] {
    const stage = this.config.actors[1];
    const args = [
      "claude", "-p",
      "--input-format", "stream-json",
      "--output-format", "stream-json",
    ];

    if (stage.model) args.push("--model", stage.model);
    if (stage.systemPrompt) args.push("--system-prompt", stage.systemPrompt);
    if (stage.tools?.length) args.push("--tools", stage.tools.join(","));
    if (stage.maxTurns) args.push("--max-turns", String(stage.maxTurns));

    // Stage 2's prompt is its instruction for processing
    args.push(stage.prompt);

    return args;
  }

  async receive(message: Message): Promise<Response> {
    const inputPrompt = typeof message.payload === "string"
      ? message.payload
      : JSON.stringify(message.payload);

    const startTime = Date.now();

    // Spawn both processes
    const proc1Args = this.buildFirstStageArgs(inputPrompt);
    const proc2Args = this.buildSecondStageArgs();

    const proc1 = spawn(proc1Args, {
      stdout: "pipe",
      stderr: "pipe",
    });

    const proc2 = spawn(proc2Args, {
      stdin: proc1.stdout, // Pipe proc1 stdout to proc2 stdin
      stdout: "pipe",
      stderr: "pipe",
    });

    // Collect outputs
    const [stdout2, stderr1, stderr2] = await Promise.all([
      new Blob([await proc2.stdout]).text(),
      new Blob([await proc1.stderr]).text(),
      new Blob([await proc2.stderr]).text(),
    ]);

    const [exit1, exit2] = await Promise.all([proc1.exited, proc2.exited]);

    const durationMs = Date.now() - startTime;

    if (exit1 !== 0) {
      return {
        success: false,
        error: `Stage 1 (${this.config.actors[0].name}) failed: ${stderr1}`,
        metadata: { durationMs },
      };
    }

    if (exit2 !== 0) {
      return {
        success: false,
        error: `Stage 2 (${this.config.actors[1].name}) failed: ${stderr2}`,
        metadata: { durationMs },
      };
    }

    // Parse the final output (last line should be result)
    const lines = stdout2.trim().split("\n").filter(l => l.trim());
    let finalResult: unknown = stdout2;

    for (const line of lines.reverse()) {
      try {
        const parsed = JSON.parse(line);
        if (parsed.type === "result") {
          finalResult = parsed;
          break;
        }
      } catch {
        continue;
      }
    }

    return {
      success: true,
      data: finalResult,
      metadata: { durationMs },
    };
  }

  // Streaming version - yields events from both stages
  async *stream(message: Message): AsyncGenerator<StreamEvent, Response> {
    const inputPrompt = typeof message.payload === "string"
      ? message.payload
      : JSON.stringify(message.payload);

    const startTime = Date.now();

    const proc1Args = this.buildFirstStageArgs(inputPrompt);
    const proc2Args = this.buildSecondStageArgs();

    const proc1 = spawn(proc1Args, {
      stdout: "pipe",
      stderr: "pipe",
    });

    const proc2 = spawn(proc2Args, {
      stdin: proc1.stdout,
      stdout: "pipe",
      stderr: "pipe",
    });

    // Yield init event
    yield {
      type: "init",
      data: {
        chain: this.id,
        stages: this.config.actors.map(a => a.name),
      },
      timestamp: new Date(),
    };

    // Stream proc2's output (which includes context from proc1)
    const decoder = new TextDecoder();
    let buffer = "";
    let lastResult: unknown = null;

    for await (const chunk of proc2.stdout) {
      buffer += decoder.decode(chunk);
      const lines = buffer.split("\n");
      buffer = lines.pop() || "";

      for (const line of lines) {
        if (!line.trim()) continue;
        try {
          const event = JSON.parse(line);
          yield {
            type: event.type || "message",
            data: event,
            timestamp: new Date(),
          };
          if (event.type === "result") lastResult = event;
        } catch {
          yield { type: "message", data: { raw: line }, timestamp: new Date() };
        }
      }
    }

    // Handle remaining buffer
    if (buffer.trim()) {
      try {
        const event = JSON.parse(buffer);
        yield { type: event.type || "message", data: event, timestamp: new Date() };
        if (event.type === "result") lastResult = event;
      } catch {
        yield { type: "message", data: { raw: buffer }, timestamp: new Date() };
      }
    }

    await Promise.all([proc1.exited, proc2.exited]);

    return {
      success: true,
      data: lastResult,
      metadata: { durationMs: Date.now() - startTime },
    };
  }
}

// Factory
export function createChain(config: ChainConfig): ChainedActors {
  return new ChainedActors(config);
}
