#!/usr/bin/env bun
/**
 * Experiment: Can we do multi-turn with a single Claude process?
 */

import { spawn } from "bun";

async function testMultiTurn() {
  console.log("Spawning Claude with stream-json...\n");

  const proc = spawn([
    "claude",
    "-p",
    "--input-format", "stream-json",
    "--output-format", "stream-json",
  ], {
    stdin: "pipe",
    stdout: "pipe",
    stderr: "pipe",
  });

  // Read stdout in background
  const reader = async () => {
    const decoder = new TextDecoder();
    for await (const chunk of proc.stdout) {
      const text = decoder.decode(chunk);
      for (const line of text.split("\n").filter(l => l.trim())) {
        try {
          const event = JSON.parse(line);
          console.log("EVENT:", event.type, event.subtype || "");
          if (event.type === "result") {
            console.log("  result:", event.result?.slice(0, 100));
          }
        } catch {
          console.log("RAW:", line.slice(0, 100));
        }
      }
    }
    console.log("stdout closed");
  };

  reader();

  // Send first message
  console.log("--- Sending message 1 ---");
  const msg1 = JSON.stringify({
    type: "user",
    message: { content: [{ type: "text", text: "What is 2+2? Reply with just the number." }] }
  }) + "\n";

  proc.stdin.write(msg1);
  proc.stdin.flush();

  // Wait a bit
  await Bun.sleep(5000);

  // Try sending second message
  console.log("\n--- Sending message 2 ---");
  const msg2 = JSON.stringify({
    type: "user",
    message: { content: [{ type: "text", text: "Now multiply that by 3. Reply with just the number." }] }
  }) + "\n";

  proc.stdin.write(msg2);
  proc.stdin.flush();

  // Wait for response
  await Bun.sleep(5000);

  console.log("\n--- Closing stdin ---");
  proc.stdin.end();

  // Wait for process to exit
  const exitCode = await proc.exited;
  console.log("Process exited with code:", exitCode);
}

testMultiTurn().catch(console.error);
