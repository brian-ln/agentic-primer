// Minimal Actor Examples
//
// This file demonstrates the bare minimum required to implement
// actors in the tk-agents system. Each example is a fully working
// actor that can be registered and used with the Registry.

import type { Actor, Message, Response, StreamEvent } from "../src/actors/base";

// ============================================================================
// 1. Minimal Deterministic Actor
// ============================================================================
//
// Demonstrates the absolute minimum: id, type, send(), and ping handling

export class MinimalDeterministicActor implements Actor {
  // Required: unique identifier
  readonly id: string;

  // Required: actor type (deterministic = predictable behavior)
  readonly type = "deterministic" as const;

  constructor(id: string) {
    this.id = id;
  }

  // Required: handle messages and return responses
  async receive(message: Message): Promise<Response> {
    // Required: handle ping for heartbeat monitoring
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Your actor logic here
    // This example just echoes the payload back
    return {
      success: true,
      data: { echo: message.payload },
    };
  }
}

// ============================================================================
// 2. Minimal Agent Actor
// ============================================================================
//
// Same as deterministic, but type="agent" for non-deterministic behavior

export class MinimalAgentActor implements Actor {
  readonly id: string;

  // Note: "agent" type indicates non-deterministic behavior
  readonly type = "agent" as const;

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    // Required: handle ping
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Agent logic here (could involve LLMs, complex reasoning, etc.)
    // This example simulates a simple decision
    const input = String(message.payload);
    const decision = input.length > 10 ? "approve" : "reject";

    return {
      success: true,
      data: {
        decision,
        reasoning: `Input length: ${input.length}`,
        // Optionally include sender info in response for tracking
        receivedFrom: message.sender
      },
    };
  }
}

// ============================================================================
// 3. Minimal Human Actor
// ============================================================================
//
// Demonstrates human-in-the-loop pattern

export class MinimalHumanActor implements Actor {
  readonly id: string;
  readonly type = "agent" as const; // Humans are non-deterministic

  private pendingMessages: Message[] = [];

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Store message for human review
    this.pendingMessages.push(message);

    // Signal that human input is needed
    return {
      success: true,
      data: {
        status: "awaiting_human_response",
        message: "Human input required",
        pendingCount: this.pendingMessages.length,
      },
    };
  }

  // Helper method to retrieve pending messages
  getPendingMessages(): Message[] {
    return [...this.pendingMessages];
  }
}

// ============================================================================
// 4. Enhanced Deterministic Actor
// ============================================================================
//
// Demonstrates recommended features: timing metadata, error handling

export class EnhancedDeterministicActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    const startTime = Date.now();

    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Example: validate input
    if (!message.payload) {
      return {
        success: false,
        error: "Payload is required",
        metadata: { durationMs: Date.now() - startTime },
      };
    }

    // Simulate work
    await new Promise(resolve => setTimeout(resolve, 10));

    // Return with timing metadata
    return {
      success: true,
      data: { processed: message.payload },
      metadata: { durationMs: Date.now() - startTime },
    };
  }
}

// ============================================================================
// 5. Streaming Agent Actor
// ============================================================================
//
// Demonstrates optional stream() method for long-running operations

export class StreamingAgentActor implements Actor {
  readonly id: string;
  readonly type = "agent" as const;

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Non-streaming response
    return {
      success: true,
      data: { result: "Use stream() for incremental updates" },
    };
  }

  // Optional: streaming interface for long-running operations
  async *stream(message: Message): AsyncGenerator<StreamEvent, Response> {
    const startTime = Date.now();

    // Yield initialization event
    yield {
      type: "init",
      data: { status: "starting" },
      timestamp: new Date(),
    };

    // Simulate streaming work
    for (let i = 1; i <= 3; i++) {
      await new Promise(resolve => setTimeout(resolve, 100));

      yield {
        type: "message",
        data: { progress: i, message: `Step ${i} complete` },
        timestamp: new Date(),
      };
    }

    // Return final response
    return {
      success: true,
      data: { completed: true, steps: 3 },
      metadata: { durationMs: Date.now() - startTime },
    };
  }
}

// ============================================================================
// 6. Message Forwarding Actor (Sender Tracking)
// ============================================================================
//
// Demonstrates sender field usage for message forwarding and coordination

export class ForwardingActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Track who sent this message
    const receivedFrom = message.sender || "unknown";

    // Example: Forward message to another actor (in real code)
    // When forwarding, set sender to this actor's ID so recipient knows the source
    const forwardedMessage: Message = {
      ...message,
      id: `fwd_${message.id}`,
      sender: this.id,  // Set sender to our ID when forwarding
    };

    // For this example, just return info about what we would forward
    return {
      success: true,
      data: {
        action: "forwarded",
        receivedFrom,
        forwardedAs: forwardedMessage.id,
        ourId: this.id,
      },
    };
  }
}

// ============================================================================
// 7. Lifecycle-Aware Actor
// ============================================================================
//
// Demonstrates optional start() and stop() methods

export class LifecycleAwareActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;

  private isStarted = false;
  private resource: string | null = null;

  constructor(id: string) {
    this.id = id;
  }

  // Optional: called when actor is registered
  async start(): Promise<void> {
    console.log(`[${this.id}] Starting...`);
    // Initialize resources (connections, files, etc.)
    this.resource = "initialized-resource";
    this.isStarted = true;
  }

  // Optional: called when actor is unregistered
  async stop(): Promise<void> {
    console.log(`[${this.id}] Stopping...`);
    // Cleanup resources
    this.resource = null;
    this.isStarted = false;
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    if (!this.isStarted) {
      return {
        success: false,
        error: "Actor not started",
      };
    }

    return {
      success: true,
      data: { resource: this.resource, message: "Working!" },
    };
  }
}

// ============================================================================
// Integration Example
// ============================================================================
//
// Demonstrates how to use these actors with the Registry

export async function integrationExample() {
  // Import registry (in real code)
  // import { registry } from "../src/actors/registry";

  // For this example, we'll simulate registry operations
  const actors: Map<string, Actor> = new Map();

  // Create actors
  const deterministicActor = new MinimalDeterministicActor("deterministic-1");
  const agentActor = new MinimalAgentActor("agent-1");
  const humanActor = new MinimalHumanActor("human-1");

  // Register actors (simulated)
  actors.set(deterministicActor.id, deterministicActor);
  actors.set(agentActor.id, agentActor);
  actors.set(humanActor.id, humanActor);

  console.log("=== Integration Example ===\n");

  // Example 1: Send to deterministic actor
  console.log("1. Deterministic Actor:");
  const response1 = await deterministicActor.send({
    id: "msg_1",
    type: "process",
    payload: "Hello, deterministic world!",
  });
  console.log("Response:", JSON.stringify(response1, null, 2));

  // Example 2: Send to agent actor
  console.log("\n2. Agent Actor:");
  const response2 = await agentActor.send({
    id: "msg_2",
    type: "decide",
    payload: "short",
  });
  console.log("Response:", JSON.stringify(response2, null, 2));

  // Example 3: Send to human actor
  console.log("\n3. Human Actor:");
  const response3 = await humanActor.send({
    id: "msg_3",
    type: "review",
    payload: "Please review this proposal",
  });
  console.log("Response:", JSON.stringify(response3, null, 2));

  // Example 4: Ping for heartbeat
  console.log("\n4. Heartbeat Ping:");
  const response4 = await deterministicActor.send({
    id: "msg_4",
    type: "ping",
    payload: {},
  });
  console.log("Response:", JSON.stringify(response4, null, 2));

  // Example 5: Streaming
  console.log("\n5. Streaming Actor:");
  const streamingActor = new StreamingAgentActor("streaming-1");
  console.log("Stream events:");
  for await (const event of streamingActor.stream({
    id: "msg_5",
    type: "process",
    payload: "Stream this work",
  })) {
    console.log("  Event:", JSON.stringify(event, null, 2));
  }

  // Example 6: Sender Tracking
  console.log("\n6. Forwarding Actor (Sender Tracking):");
  const forwardingActor = new ForwardingActor("forwarder-1");
  const response6 = await forwardingActor.send({
    id: "msg_6",
    type: "forward",
    payload: "Forward this message",
    sender: "external-system",  // Simulate message from external source
  });
  console.log("Response:", JSON.stringify(response6, null, 2));

  // Example 7: Lifecycle
  console.log("\n7. Lifecycle-Aware Actor:");
  const lifecycleActor = new LifecycleAwareActor("lifecycle-1");
  await lifecycleActor.start();
  const response7 = await lifecycleActor.send({
    id: "msg_7",
    type: "work",
    payload: "Do some work",
  });
  console.log("Response:", JSON.stringify(response7, null, 2));
  await lifecycleActor.stop();
}

// Run the example if executed directly
if (import.meta.main) {
  integrationExample().catch(console.error);
}
