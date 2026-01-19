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

    // Mock System Services
    class MockFileIO extends Actor {
      async receive(msg: Message) {
        if (msg.type === "READ_FILE") {
          this.send(msg.sender!, { 
            type: "FILE_CONTENT", 
            payload: { path: msg.payload.path, data: "{}" } 
          });
        }
      }
    }
    system.spawn("seag://system/file-io", MockFileIO);

    class MockParser extends Actor {
      async receive(msg: Message) {
        // No-op for test
      }
    }
    system.spawn("seag://system/parser", MockParser);

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
    system.spawn("seag://local/user-proxy", MockUserProxy);

    // 2. Act: Send THINK command
    system.send("seag://system/brain", {
      type: "THINK",
      sender: "seag://local/user-proxy",
      payload: { input: "mount data/demo.json" }
    });

    // 3. Wait for thinking and processing
    await new Promise(resolve => setTimeout(resolve, 500)); // Increased timeout for file I/O

    // Assert
    expect(isThinking).toBe(true);
    expect(lastOutput).toContain("Mounted data/demo.json");
  });

  test("Objective 4.2.2: Get/Set Commands", async () => {
    const system = new System();
    system.spawn("seag://system/brain", BrainAgent);

    let targetState = "initial";
    class MockTarget extends Actor {
      async receive(msg: Message) {
        if (msg.type === "PATCH") targetState = msg.payload;
        if (msg.type === "GET") this.send(msg.sender!, { type: "STATE", payload: targetState });
      }
    }
    system.spawn("seag://local/target", MockTarget);

    let lastOutput: string | null = null;
    class User extends Actor {
      async receive(msg: Message) {
        if (msg.type === "OUTPUT") lastOutput = msg.payload.content;
      }
    }
    system.spawn("seag://local/user-proxy", User);

    // 1. SET
    system.send("seag://system/brain", {
      type: "THINK",
      sender: "seag://local/user-proxy",
      payload: { input: "set seag://local/target updated" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(targetState).toBe("updated");
    expect(lastOutput).toContain("Updated seag://local/target");

    // 2. GET
    system.send("seag://system/brain", {
      type: "THINK",
      sender: "seag://local/user-proxy",
      payload: { input: "get seag://local/target" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(lastOutput).toContain('"updated"');
  });

});
