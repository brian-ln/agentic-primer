import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { BrainAgent } from "../../seag/brain-agent";
import { GraphProjector } from "../../seag/graph-projector";

/**
 * PHASE 4.2 HARNESS: The Brain (Inference & Coordination)
 * 
 * Objectives:
 * 1. Brain receives THINK message.
 * 2. Brain coordinates with Projector.
 * 3. Brain replies with OUTPUT.
 */

describe("SEAG Phase 4.2: The Brain Agent", () => {

  test("Objective 4.2.1: Sense-Think-Act Loop", async () => {
    const system = new System();
    
    // 1. Setup Actors
    system.spawn("seag://system/brain", BrainAgent);
    system.spawn("seag://system/projector", GraphProjector);

    let lastOutput: string | null = null;
    let isThinking = false;

    class MockUserProxy extends Actor {
      async receive(msg: Message) {
        if (msg.type === "SIGNAL" && msg.payload.status === "thinking") {
          isThinking = true;
        }
        if (msg.type === "OUTPUT") {
          lastOutput = msg.payload.content;
        }
      }
    }
    system.spawn("seag://local/mock-user", MockUserProxy);

    // 2. Act: Send THINK command
    system.send("seag://system/brain", {
      type: "THINK",
      sender: "seag://local/mock-user",
      payload: { input: "Hello SEAG" }
    });

    // 3. Wait for thinking and processing
    await new Promise(resolve => setTimeout(resolve, 200));

    // Assert
    expect(isThinking).toBe(true);
    expect(lastOutput).toContain("Hello Human");
  });

});
