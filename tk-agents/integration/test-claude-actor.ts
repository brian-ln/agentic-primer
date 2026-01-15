#!/usr/bin/env bun
/**
 * Integration test: ClaudeActor with real Claude CLI
 *
 * Run this OUTSIDE of a Claude session:
 *   bun run integration/test-claude-actor.ts
 */

import { ClaudeActor } from "../src/actors/claude";
import { createMessage } from "../src/actors/base";

async function testResumableSession() {
  console.log("=== Test: Resumable Session ===\n");

  const actor = new ClaudeActor({
    id: "test-agent",
    systemPrompt: "You are a helpful math tutor. Keep responses brief.",
    model: "haiku", // Use haiku for faster/cheaper tests
  });

  console.log(`Session ID: ${actor.getSessionId()}\n`);

  // Turn 1
  console.log("Turn 1: Asking about addition...");
  const r1 = await actor.send(createMessage("prompt", "What is 15 + 27? Just give the number."));
  console.log("Response:", r1.success ? r1.data : r1.error);
  console.log(`Duration: ${r1.metadata?.durationMs}ms\n`);

  // Turn 2 - should resume with context
  console.log("Turn 2: Follow-up question...");
  const r2 = await actor.send(createMessage("prompt", "Now double that result."));
  console.log("Response:", r2.success ? r2.data : r2.error);
  console.log(`Duration: ${r2.metadata?.durationMs}ms\n`);

  // Turn 3 - test memory
  console.log("Turn 3: Testing memory...");
  const r3 = await actor.send(createMessage("prompt", "What was the original addition problem I asked?"));
  console.log("Response:", r3.success ? r3.data : r3.error);
  console.log(`Duration: ${r3.metadata?.durationMs}ms\n`);

  console.log("=== Session Complete ===\n");
}

async function testStreamingOutput() {
  console.log("=== Test: Streaming Output ===\n");

  const actor = new ClaudeActor({
    id: "streaming-agent",
    model: "haiku",
    outputFormat: "stream-json",
  });

  console.log("Streaming response...\n");

  const generator = actor.stream(createMessage("prompt", "Count from 1 to 5, one number per line."));

  for await (const event of generator) {
    console.log(`Event [${event.type}]:`, JSON.stringify(event.data).slice(0, 100));
  }

  console.log("\n=== Streaming Complete ===\n");
}

async function main() {
  console.log("Claude Actor Integration Tests\n");
  console.log("NOTE: These tests require Claude CLI to be available and authenticated.\n");

  try {
    await testResumableSession();
    await testStreamingOutput();
    console.log("All tests completed!");
  } catch (error) {
    console.error("Test failed:", error);
    process.exit(1);
  }
}

main();
