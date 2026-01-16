// Tests for minimal actor examples
// Validates that all example actors work correctly

import { test, expect } from "bun:test";
import {
  MinimalDeterministicActor,
  MinimalAgentActor,
  MinimalHumanActor,
  EnhancedDeterministicActor,
  StreamingAgentActor,
  LifecycleAwareActor,
} from "./minimal-actors";

// ============================================================================
// Minimal Deterministic Actor Tests
// ============================================================================

test("MinimalDeterministicActor - basic instantiation", () => {
  const actor = new MinimalDeterministicActor("test-1");
  expect(actor.id).toBe("test-1");
  expect(actor.type).toBe("deterministic");
});

test("MinimalDeterministicActor - handles ping", async () => {
  const actor = new MinimalDeterministicActor("test-1");
  const response = await actor.receive({
    id: "msg_1",
    type: "ping",
    payload: {},
  });

  expect(response.success).toBe(true);
  expect(response.data).toHaveProperty("alive", true);
  expect(response.data).toHaveProperty("timestamp");
});

test("MinimalDeterministicActor - echoes payload", async () => {
  const actor = new MinimalDeterministicActor("test-1");
  const response = await actor.receive({
    id: "msg_1",
    type: "process",
    payload: "test data",
  });

  expect(response.success).toBe(true);
  expect(response.data).toEqual({ echo: "test data" });
});

// ============================================================================
// Minimal Agent Actor Tests
// ============================================================================

test("MinimalAgentActor - basic instantiation", () => {
  const actor = new MinimalAgentActor("agent-1");
  expect(actor.id).toBe("agent-1");
  expect(actor.type).toBe("agent");
});

test("MinimalAgentActor - handles ping", async () => {
  const actor = new MinimalAgentActor("agent-1");
  const response = await actor.receive({
    id: "msg_1",
    type: "ping",
    payload: {},
  });

  expect(response.success).toBe(true);
  expect(response.data).toHaveProperty("alive", true);
});

test("MinimalAgentActor - makes decisions based on input", async () => {
  const actor = new MinimalAgentActor("agent-1");

  // Short input -> reject
  const response1 = await actor.receive({
    id: "msg_1",
    type: "decide",
    payload: "short",
  });
  expect(response1.success).toBe(true);
  expect(response1.data).toHaveProperty("decision", "reject");

  // Long input -> approve
  const response2 = await actor.receive({
    id: "msg_2",
    type: "decide",
    payload: "this is a much longer input string",
  });
  expect(response2.success).toBe(true);
  expect(response2.data).toHaveProperty("decision", "approve");
});

// ============================================================================
// Minimal Human Actor Tests
// ============================================================================

test("MinimalHumanActor - basic instantiation", () => {
  const actor = new MinimalHumanActor("human-1");
  expect(actor.id).toBe("human-1");
  expect(actor.type).toBe("agent");
});

test("MinimalHumanActor - handles ping", async () => {
  const actor = new MinimalHumanActor("human-1");
  const response = await actor.receive({
    id: "msg_1",
    type: "ping",
    payload: {},
  });

  expect(response.success).toBe(true);
  expect(response.data).toHaveProperty("alive", true);
});

test("MinimalHumanActor - queues messages for human review", async () => {
  const actor = new MinimalHumanActor("human-1");

  const response = await actor.receive({
    id: "msg_1",
    type: "review",
    payload: "Please review this",
  });

  expect(response.success).toBe(true);
  expect(response.data).toHaveProperty("status", "awaiting_human_response");
  expect(response.data).toHaveProperty("pendingCount", 1);

  const pending = actor.getPendingMessages();
  expect(pending).toHaveLength(1);
  expect(pending[0].type).toBe("review");
});

// ============================================================================
// Enhanced Deterministic Actor Tests
// ============================================================================

test("EnhancedDeterministicActor - validates input", async () => {
  const actor = new EnhancedDeterministicActor("enhanced-1");

  // Missing payload
  const response = await actor.receive({
    id: "msg_1",
    type: "process",
    payload: null,
  });

  expect(response.success).toBe(false);
  expect(response.error).toBe("Payload is required");
  expect(response.metadata).toHaveProperty("durationMs");
});

test("EnhancedDeterministicActor - includes timing metadata", async () => {
  const actor = new EnhancedDeterministicActor("enhanced-1");

  const response = await actor.receive({
    id: "msg_1",
    type: "process",
    payload: "test data",
  });

  expect(response.success).toBe(true);
  expect(response.metadata).toHaveProperty("durationMs");
  expect(typeof response.metadata?.durationMs).toBe("number");
});

// ============================================================================
// Streaming Agent Actor Tests
// ============================================================================

test("StreamingAgentActor - supports non-streaming send", async () => {
  const actor = new StreamingAgentActor("streaming-1");

  const response = await actor.receive({
    id: "msg_1",
    type: "process",
    payload: "test",
  });

  expect(response.success).toBe(true);
  expect(response.data).toHaveProperty("result");
});

test("StreamingAgentActor - streams events", async () => {
  const actor = new StreamingAgentActor("streaming-1");

  const events = [];
  const generator = actor.stream({
    id: "msg_1",
    type: "process",
    payload: "test",
  });

  for await (const event of generator) {
    events.push(event);
  }

  // Should have: init + 3 message events
  expect(events.length).toBe(4);
  expect(events[0].type).toBe("init");
  expect(events[1].type).toBe("message");
  expect(events[2].type).toBe("message");
  expect(events[3].type).toBe("message");
});

test("StreamingAgentActor - returns final response", async () => {
  const actor = new StreamingAgentActor("streaming-1");

  const generator = actor.stream({
    id: "msg_1",
    type: "process",
    payload: "test",
  });

  // Consume all events
  const events = [];
  for await (const event of generator) {
    events.push(event);
  }

  // Generator's return value is the final response
  // Note: In async generators, the final value is in the last iteration
  // We need to check the done value
  const gen = actor.stream({
    id: "msg_2",
    type: "process",
    payload: "test",
  });

  let finalResponse;
  let result = await gen.next();
  while (!result.done) {
    result = await gen.next();
  }
  finalResponse = result.value;

  expect(finalResponse).toBeDefined();
  expect(finalResponse.success).toBe(true);
  expect(finalResponse.data).toHaveProperty("completed", true);
  expect(finalResponse.metadata).toHaveProperty("durationMs");
});

// ============================================================================
// Lifecycle-Aware Actor Tests
// ============================================================================

test("LifecycleAwareActor - lifecycle management", async () => {
  const actor = new LifecycleAwareActor("lifecycle-1");

  // Before start - should fail
  const response1 = await actor.receive({
    id: "msg_1",
    type: "work",
    payload: "test",
  });
  expect(response1.success).toBe(false);
  expect(response1.error).toBe("Actor not started");

  // After start - should work
  await actor.start();
  const response2 = await actor.receive({
    id: "msg_2",
    type: "work",
    payload: "test",
  });
  expect(response2.success).toBe(true);
  expect(response2.data).toHaveProperty("resource");

  // After stop
  await actor.stop();
});

test("LifecycleAwareActor - handles ping always", async () => {
  const actor = new LifecycleAwareActor("lifecycle-1");

  // Ping should work even without start()
  const response = await actor.receive({
    id: "msg_1",
    type: "ping",
    payload: {},
  });

  expect(response.success).toBe(true);
  expect(response.data).toHaveProperty("alive", true);
});

// ============================================================================
// Integration Tests
// ============================================================================

test("All actors handle ping correctly", async () => {
  const actors = [
    new MinimalDeterministicActor("test-1"),
    new MinimalAgentActor("test-2"),
    new MinimalHumanActor("test-3"),
    new EnhancedDeterministicActor("test-4"),
    new StreamingAgentActor("test-5"),
    new LifecycleAwareActor("test-6"),
  ];

  for (const actor of actors) {
    const response = await actor.receive({
      id: "ping_test",
      type: "ping",
      payload: {},
    });

    expect(response.success).toBe(true);
    expect(response.data).toHaveProperty("alive", true);
  }
});

test("All actors have required fields", () => {
  const actors = [
    new MinimalDeterministicActor("test-1"),
    new MinimalAgentActor("test-2"),
    new MinimalHumanActor("test-3"),
    new EnhancedDeterministicActor("test-4"),
    new StreamingAgentActor("test-5"),
    new LifecycleAwareActor("test-6"),
  ];

  for (const actor of actors) {
    expect(actor.id).toBeDefined();
    expect(typeof actor.id).toBe("string");
    expect(actor.type).toBeDefined();
    expect(["deterministic", "agent"]).toContain(actor.type);
    expect(typeof actor.receive).toBe("function");
  }
});
