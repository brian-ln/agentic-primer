// MockActor - For testing without real Claude CLI

import type { Actor, Message, Response, StreamEvent } from "./base";

export interface MockResponse {
  delay?: number;      // Simulate latency
  response: Response;
  events?: StreamEvent[]; // For streaming
}

export type MockHandler = (message: Message) => MockResponse | Promise<MockResponse>;

export interface MockActorConfig {
  id: string;
  type?: "deterministic" | "agent";
  handler?: MockHandler;
  responses?: MockResponse[]; // Queue of responses (used in order)
}

export class MockActor implements Actor {
  readonly id: string;
  readonly type: "deterministic" | "agent";

  private handler?: MockHandler;
  private responseQueue: MockResponse[];
  private responseIndex = 0;

  // Track all received messages for assertions
  public receivedMessages: Message[] = [];

  constructor(config: MockActorConfig) {
    this.id = config.id;
    this.type = config.type || "deterministic";
    this.handler = config.handler;
    this.responseQueue = config.responses || [];
  }

  async send(message: Message): Promise<Response> {
    this.receivedMessages.push(message);

    // Use handler if provided
    if (this.handler) {
      const mockResponse = await this.handler(message);
      if (mockResponse.delay) {
        await new Promise(r => setTimeout(r, mockResponse.delay));
      }
      return mockResponse.response;
    }

    // Use response queue
    if (this.responseIndex < this.responseQueue.length) {
      const mockResponse = this.responseQueue[this.responseIndex++];
      if (mockResponse.delay) {
        await new Promise(r => setTimeout(r, mockResponse.delay));
      }
      return mockResponse.response;
    }

    // Default response
    return {
      success: true,
      data: { echo: message.payload },
    };
  }

  async *stream(message: Message): AsyncGenerator<StreamEvent, Response> {
    this.receivedMessages.push(message);

    let mockResponse: MockResponse;

    if (this.handler) {
      mockResponse = await this.handler(message);
    } else if (this.responseIndex < this.responseQueue.length) {
      mockResponse = this.responseQueue[this.responseIndex++];
    } else {
      mockResponse = { response: { success: true, data: { echo: message.payload } } };
    }

    // Yield events if provided
    if (mockResponse.events) {
      for (const event of mockResponse.events) {
        if (mockResponse.delay) {
          await new Promise(r => setTimeout(r, mockResponse.delay / mockResponse.events!.length));
        }
        yield event;
      }
    }

    return mockResponse.response;
  }

  // Reset for reuse in tests
  reset(): void {
    this.receivedMessages = [];
    this.responseIndex = 0;
  }
}

// Helpers for common mock patterns

export function createEchoMock(id: string): MockActor {
  return new MockActor({
    id,
    handler: (msg) => ({
      response: { success: true, data: msg.payload },
    }),
  });
}

export function createFailingMock(id: string, error: string): MockActor {
  return new MockActor({
    id,
    handler: () => ({
      response: { success: false, error },
    }),
  });
}

export function createDelayedMock(id: string, delayMs: number): MockActor {
  return new MockActor({
    id,
    handler: (msg) => ({
      delay: delayMs,
      response: { success: true, data: msg.payload },
    }),
  });
}

// Mock that simulates Claude-like behavior
export function createClaudeMock(id: string, responses: string[]): MockActor {
  let index = 0;
  return new MockActor({
    id,
    type: "agent",
    handler: () => {
      const response = responses[index++ % responses.length];
      return {
        delay: 100, // Simulate some latency
        response: {
          success: true,
          data: response,
          metadata: { sessionId: `mock-session-${id}` },
        },
      };
    },
  });
}
