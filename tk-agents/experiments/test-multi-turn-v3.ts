#!/usr/bin/env bun
/**
 * Experiment v3: Correct message format from docs
 * {"type":"user","message":{"role":"user","content":"..."}}
 */

import { spawn } from "bun";

async function test() {
  console.log("Spawning Claude with stream-json I/O...\n");

  const proc = spawn([
    "claude",
    "-p",
    "--input-format", "stream-json",
    "--output-format", "stream-json",
  ], {
    stdin: "pipe",
    stdout: "pipe",
    stderr: "inherit",
  });

  // Read stdout in background
  const decoder = new TextDecoder();
  const readOutput = async () => {
    for await (const chunk of proc.stdout) {
      const text = decoder.decode(chunk);
      for (const line of text.split("\n").filter(l => l.trim())) {
        console.log("<<", line.slice(0, 300));
      }
    }
    console.log("[stdout closed]");
  };
  readOutput();

  // Correct format from docs
  const msg1 = JSON.stringify({
    type: "user",
    message: { role: "user", content: "What is 2+2? Reply with just the number." }
  }) + "\n";

  console.log(">> Sending:", msg1.trim());
  proc.stdin.write(msg1);
  proc.stdin.flush();

  await Bun.sleep(10000);

  // Second message
  const msg2 = JSON.stringify({
    type: "user",
    message: { role: "user", content: "Multiply that by 3. Reply with just the number." }
  }) + "\n";

  console.log("\n>> Sending:", msg2.trim());
  proc.stdin.write(msg2);
  proc.stdin.flush();

  await Bun.sleep(10000);

  console.log("\n[Closing stdin]");
  proc.stdin.end();

  const code = await proc.exited;
  console.log("Exit code:", code);
}

test();
