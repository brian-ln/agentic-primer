import { expect, test, describe, beforeAll, afterAll } from "bun:test";
import { System, Actor, Message, Event } from "../../seag/kernel";
import { EventLogActor } from "../../seag/event-log";
import { rm } from "node:fs/promises";

/**
 * PHASE 2 HARNESS: Persistence & Memory
 * 
 * Objectives:
 * 1. Automatic event logging for state-changing messages.
 * 2. Durable storage (JSONL).
 * 3. Event Replay.
 */

describe("SEAG Phase 2: Persistence", () => {
  const TEST_LOG = "data/events.jsonl";

  class StateActor extends Actor {
    public value: number = 0;
    async receive(msg: Message) {
      if (msg.type === "INCREMENT") {
        this.value += (msg.payload?.amount || 1);
      }
    }
  }

  beforeAll(async () => {
    try { await rm(TEST_LOG); } catch {}
  });

  test("Objective 2.1: Automatic Event Logging", async () => {
    const system = new System();
    
    // 1. Setup EventLog
    system.spawn("seag://system/event-log", EventLogActor, "permanent");
    system.setEventLog("seag://system/event-log");

    // 2. Spawn a StateActor
    const actor = system.spawn("seag://local/counter", StateActor);

    // 3. Send a message with a mutator name (automatic event)
    system.send("seag://local/counter", { 
      type: "UPDATE_VALUE", // Starts with 'update' -> automatic event
      payload: { amount: 10 } 
    });

    // Wait for file I/O
    await new Promise(resolve => setTimeout(resolve, 100));

    // 4. Verify file exists and contains the event
    const fs = await import("node:fs/promises");
    const content = await fs.readFile(TEST_LOG, "utf-8");
    const event: Event = JSON.parse(content.trim());

    expect(event.type).toBe("UPDATE_VALUE");
    expect(event.source).toBe("seag://local/counter");
    expect(event.payload.amount).toBe(10);
    expect(event.traceId).toBeDefined();
  });

  test("Objective 2.3: Event Replay", async () => {
    const system = new System();
    const logger = system.spawn("seag://system/event-log", EventLogActor, "permanent");
    
    let replayedEvents: Event[] = [];
    
    class ReplayReceiver extends Actor {
      async receive(msg: Message) {
        if (msg.type === "REPLAY_RESULT") {
          replayedEvents = msg.payload;
        }
      }
    }
    
    const receiver = system.spawn("seag://local/receiver", ReplayReceiver);
    
    // Trigger replay
    system.send("seag://system/event-log", { 
      type: "REPLAY", 
      sender: "seag://local/receiver" 
    });

    await new Promise(resolve => setTimeout(resolve, 100));

    expect(replayedEvents.length).toBeGreaterThan(0);
    expect(replayedEvents[0].type).toBe("UPDATE_VALUE");
  });

});
