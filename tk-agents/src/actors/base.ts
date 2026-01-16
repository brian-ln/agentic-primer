// Actor base interface and types

import type { ActorError } from "./errors";

export interface Message {
  id: string;
  type: string;
  payload: unknown;
  correlationId?: string;  // For matching responses to requests
  sender?: string;  // Optional: ID of the actor or entity that sent this message
}

export interface Response {
  success: boolean;
  data?: unknown;
  error?: string | ActorError;  // Support both string (legacy) and structured errors
  metadata?: {
    durationMs?: number;
    costUsd?: number;
    sessionId?: string;
    [key: string]: unknown;  // Allow additional metadata fields
  };
}

export interface StreamEvent {
  type: "init" | "message" | "tool_use" | "tool_result" | "result" | "error";
  data: unknown;
  timestamp: Date;
}

// Actor types
export type ActorType = "deterministic" | "agent";

// All actors implement this interface - deterministic or not
export interface Actor {
  readonly id: string;
  readonly type: ActorType;

  // Required for backward compatibility (will be deprecated in Phase 2)
  // During Phase 1-2, this is still the primary method
  send(message: Message): Promise<Response>;

  // NEW: Semantically correct method (Hewitt Actor Model) - optional during Phase 1
  // Will become required in Phase 3, replacing send()
  receive?(message: Message): Promise<Response>;

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
