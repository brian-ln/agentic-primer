#!/usr/bin/env bun
/**
 * Experiment v2: Initial prompt + stream-json for follow-ups
 */

import { spawn } from "bun";

async function test() {
  console.log("Spawning Claude...\n");

  const proc = spawn([
    "claude",
    "-p",
    "--output-format", "stream-json",
    "--input-format", "stream-json",
    "You are a calculator. I'll send you math problems. Reply with just the number, nothing else.",
  ], {
    stdin: "pipe",
    stdout: "pipe",
    stderr: "inherit",
  });

  // Collect all output
  const decoder = new TextDecoder();

  const readOutput = async () => {
    for await (const chunk of proc.stdout) {
      const text = decoder.decode(chunk);
      for (const line of text.split("\n").filter(l => l.trim())) {
        console.log("OUT:", line.slice(0, 200));
      }
    }
  };

  readOutput();

  // Wait for initial response
  await Bun.sleep(8000);

  // Try follow-up
  console.log("\n--- Sending follow-up ---");
  const msg = JSON.stringify({
    type: "user",
    message: { content: [{ type: "text", text: "What is 5 * 5?" }] }
  }) + "\n";

  proc.stdin.write(msg);
  proc.stdin.flush();

  await Bun.sleep(8000);

  proc.stdin.end();
  const code = await proc.exited;
  console.log("\nExit code:", code);
}

test();
