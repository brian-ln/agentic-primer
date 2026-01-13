// Actor base interface and types

export interface Message {
  id: string;
  type: string;
  payload: unknown;
  correlationId?: string;  // For matching responses to requests
}

export interface Response {
  success: boolean;
  data?: unknown;
  error?: string;
  metadata?: {
    durationMs?: number;
    costUsd?: number;
    sessionId?: string;
  };
}

export interface StreamEvent {
  type: "init" | "message" | "tool_use" | "tool_result" | "result" | "error";
  data: unknown;
  timestamp: Date;
}

// All actors implement this interface - deterministic or not
export interface Actor {
  readonly id: string;
  readonly type: "deterministic" | "agent";

  // Synchronous-style send (returns when complete)
  send(message: Message): Promise<Response>;

  // Optional: streaming interface for long-running actors
  stream?(message: Message): AsyncGenerator<StreamEvent, Response>;

  // Lifecycle
  start?(): Promise<void>;
  stop?(): Promise<void>;
}

// Factory function signature
export type ActorFactory<T extends Actor = Actor> = (config: unknown) => T;

// Helper to create message IDs
let messageCounter = 0;
export function createMessage(type: string, payload: unknown): Message {
  return {
    id: `msg_${++messageCounter}_${Date.now()}`,
    type,
    payload,
  };
}

// Helper to measure execution time
export async function withTiming<T>(
  fn: () => Promise<T>
): Promise<{ result: T; durationMs: number }> {
  const start = Date.now();
  const result = await fn();
  return { result, durationMs: Date.now() - start };
}
