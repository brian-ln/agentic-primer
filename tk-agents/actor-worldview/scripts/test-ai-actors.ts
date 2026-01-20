import { System, Actor, Message } from "../seag/kernel";
import { GeminiInferenceActor } from "../seag/inference-actor";
import { GeminiEmbeddingActor } from "../seag/embedding-actor";
import { readFileSync, existsSync } from "fs";

async function main() {
  // Manual .env loading for Bun if needed
  if (existsSync(".env")) {
    const envFile = readFileSync(".env", "utf8");
    envFile.split("\n").forEach(line => {
      const [key, ...value] = line.split("=");
      if (key && value.length > 0) {
        process.env[key.trim()] = value.join("=").trim();
      }
    });
  }

  const system = new System();
  console.log(`API Key present: ${!!process.env.GEMINI_API_KEY}`);
  
  // 1. Setup Actors
  system.spawn("seag://system/inference", GeminiInferenceActor);
  system.spawn("seag://system/embedder", GeminiEmbeddingActor);

  // Give actors time to run onStart()
  await new Promise(resolve => setTimeout(resolve, 100));

  // 2. Setup a "Human Proxy" to receive the results
  class TesterActor extends Actor {
    async receive(msg: Message) {
      if (msg.type === "RESPONSE") {
        console.log("\n--- INFERENCE RESPONSE ---");
        console.log(msg.payload.text);
        console.log("--------------------------\n");
        process.exit(0);
      }
      if (msg.type === "VECTOR") {
        console.log("\n--- EMBEDDING RESPONSE ---");
        console.log(`Vector Dimension: ${msg.payload.floats.length}`);
        console.log(`First 5 values: ${msg.payload.floats.slice(0, 5)}`);
        console.log("--------------------------\n");
        process.exit(0);
      }
      if (msg.type === "ERROR") {
        console.error("\n--- ACTOR ERROR ---");
        console.error(msg.payload.message);
        console.error("-------------------\n");
        process.exit(1);
      }
    }
  }
  system.spawn("seag://local/tester", TesterActor);

  // 3. Parse Args
  const cmd = process.argv[2];
  const input = process.argv.slice(3).join(" ");

  if (!cmd || !input) {
    console.log("Usage:");
    console.log("  bun scripts/test-ai-actors.ts ask <prompt>");
    console.log("  bun scripts/test-ai-actors.ts embed <text>");
    process.exit(1);
  }

  // 4. Send Message
  if (cmd === "ask") {
    console.log(`Sending PROMPT to inference actor: "${input}"`);
    system.send("seag://system/inference", {
      type: "PROMPT",
      sender: "seag://local/tester",
      payload: { text: input }
    });
  } else if (cmd === "embed") {
    console.log(`Sending EMBED to embedding actor: "${input}"`);
    system.send("seag://system/embedder", {
      type: "EMBED",
      sender: "seag://local/tester",
      payload: { text: input }
    });
  } else {
    console.error("Unknown command. Use 'ask' or 'embed'.");
    process.exit(1);
  }

  // Set a safety timeout
  setTimeout(() => {
    console.error("Timeout waiting for actor response.");
    process.exit(1);
  }, 30000);
}

main().catch(err => {
  console.error("CLI Crash:", err);
  process.exit(1);
});
