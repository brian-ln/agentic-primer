import { expect, test, describe, beforeAll, afterAll } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { Gateway } from "../../seag/gateway";
import { UserProxy } from "../../seag/user-proxy";
import { EventLogActor } from "../../seag/event-log";

/**
 * PHASE 4.1 HARNESS: Gateway & UserProxy
 * 
 * Objectives:
 * 1. Bridge WebSocket message to UserProxy.
 * 2. Verify causality chain: Gateway -> UserProxy -> InteractionLog.
 */

describe("SEAG Phase 4.1: Gateway Integration", () => {
  const PORT = 3333;
  const TEST_LOG = "data/events.jsonl";

  beforeAll(async () => {
    try { await rm(TEST_LOG); } catch {}
  });

  test("Objective 4.1.1: E2E Input Flow", async () => {
    const system = new System();
    
    // 1. Setup Actors
    system.spawn("seag://local/user-proxy", UserProxy);
    system.spawn("seag://system/interaction-log", EventLogActor);
    system.setEventLog("seag://system/interaction-log");

    // 2. Start Gateway
    const gateway = new Gateway(system);
    gateway.start(PORT);

    // 3. Connect as a client to /ws
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`);
    
    await new Promise((resolve) => {
      ws.onopen = () => {
        ws.send(JSON.stringify({ type: "INPUT", payload: { text: "Hello SEAG" } }));
        setTimeout(resolve, 300); // Increased wait
      };
    });

    // --- Additional test: connecting to root should NOT upgrade to websocket
    const wsRoot = new WebSocket(`ws://localhost:${PORT}`);
    let rootOpened = false;
    await new Promise((resolve) => {
      wsRoot.onopen = () => { rootOpened = true; resolve(null); };
      wsRoot.onerror = () => resolve(null);
      // Wait 300ms to allow handshake to succeed/fail
      setTimeout(resolve, 300);
    });
    expect(rootOpened).toBe(false);

    // 4. Verify Log Entry
    let logEntries: any[] = [];
    class LogCheckActor extends Actor {
      async receive(msg: Message) {
        if (msg.type === "REPLAY_RESULT") logEntries = msg.payload;
      }
    }
    system.spawn("seag://local/checker", LogCheckActor);
    system.send("seag://system/interaction-log", { type: "REPLAY", sender: "seag://local/checker" });

    await new Promise(resolve => setTimeout(resolve, 300));

    // console.log("[Test] Log Entries:", JSON.stringify(logEntries, null, 2));

    // Assert: The message should have been logged
    expect(logEntries.length).toBeGreaterThan(0);
    const hasMsg = logEntries.some(e => e.content === "Hello SEAG" || (e.payload && e.payload.content === "Hello SEAG"));
    expect(hasMsg).toBe(true);

    gateway.stop();
    ws.close();
  });

});
