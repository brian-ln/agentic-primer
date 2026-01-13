#!/usr/bin/env bun
/**
 * Integration test: ChainedActors (A → B pipeline)
 *
 * Run this OUTSIDE of a Claude session:
 *   bun run integration/test-chain.ts
 */

import { ChainedActors } from "../src/actors/chain";
import { createMessage } from "../src/actors/base";

async function testPlannerExecutorChain() {
  console.log("=== Test: Planner → Executor Chain ===\n");

  const chain = new ChainedActors({
    id: "planner-executor",
    actors: [
      {
        name: "planner",
        prompt: "You are a task planner. Break down the given task into 3 simple steps. Output as a numbered list.",
        model: "haiku",
      },
      {
        name: "executor",
        prompt: "You received a plan from another agent. Summarize what you would do to execute each step in one sentence each.",
        model: "haiku",
      },
    ],
  });

  console.log("Input task: 'Make a peanut butter sandwich'\n");
  console.log("Running chain: Planner → Executor\n");

  const response = await chain.send(createMessage("task", "Make a peanut butter sandwich"));

  if (response.success) {
    console.log("Chain completed successfully!");
    console.log(`Duration: ${response.metadata?.durationMs}ms\n`);
    console.log("Final output:", response.data);
  } else {
    console.log("Chain failed:", response.error);
  }

  console.log("\n=== Chain Complete ===\n");
}

async function testStreamingChain() {
  console.log("=== Test: Streaming Chain ===\n");

  const chain = new ChainedActors({
    id: "streaming-chain",
    actors: [
      {
        name: "researcher",
        prompt: "You are a researcher. List 3 interesting facts about the given topic.",
        model: "haiku",
      },
      {
        name: "writer",
        prompt: "You received research from another agent. Write a brief paragraph summarizing the facts.",
        model: "haiku",
      },
    ],
  });

  console.log("Topic: 'Honey bees'\n");
  console.log("Streaming chain events:\n");

  const generator = chain.stream(createMessage("research", "Honey bees"));

  let eventCount = 0;
  for await (const event of generator) {
    eventCount++;
    const preview = JSON.stringify(event.data).slice(0, 80);
    console.log(`[${eventCount}] ${event.type}: ${preview}...`);
  }

  console.log(`\nTotal events: ${eventCount}`);
  console.log("\n=== Streaming Chain Complete ===\n");
}

async function main() {
  console.log("Chain Integration Tests\n");
  console.log("NOTE: These tests require Claude CLI to be available and authenticated.\n");
  console.log("This demonstrates stream-chaining where one Claude's output pipes to another.\n");

  try {
    await testPlannerExecutorChain();
    await testStreamingChain();
    console.log("All chain tests completed!");
  } catch (error) {
    console.error("Test failed:", error);
    process.exit(1);
  }
}

main();
