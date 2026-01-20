import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress, CapabilityToken } from "../../seag/kernel";
import { PersistenceManager } from "../../seag/persistence-manager";
import { FileEffectActor } from "../../seag/file-effect";
import { existsSync, unlinkSync, readFileSync, writeFileSync } from "fs";

describe("SEAG Phase 7: Persistence Subsystem", () => {
  // Helper actor to send messages in tests
  class TestClientActor extends Actor {
    public receivedMessages: Message[] = [];
    constructor(id: string, system: System) {
      super(id, system);
    }
    async receive(msg: Message) {
      this.receivedMessages.push(msg);
    }
    // Helper to send a message and wait for a response if needed
    sendAndWait(target: ActorAddress, msg: Message, timeout = 100) {
      return new Promise<Message | null>(resolve => {
        this.receivedMessages = []; // Clear previous messages
        this.send(target, { ...msg, sender: this.id });
        const timer = setTimeout(() => resolve(null), timeout);
        const check = setInterval(() => {
          if (this.receivedMessages.length > 0) {
            clearInterval(check);
            clearTimeout(timer);
            resolve(this.receivedMessages[0]);
          }
        }, 10);
      });
    }
  }

  test("Objective 7.1.1: PersistenceManager Snapshot Logic", async () => {
    const system = new System();
    system.spawn("seag://system/persistence", PersistenceManager);
    system.spawn("seag://system/file-io", FileEffectActor);

    const testFile = "data/test_snapshot.json";
    if (existsSync(testFile)) unlinkSync(testFile);

    // 1. Create a stateful actor to snapshot
    class StatefulActor extends Actor {
      private state = { counter: 42 };
      async receive(msg: Message) {
        if (msg.type === "GET") {
          this.send(msg.sender!, { type: "STATE", payload: this.state });
        }
      }
    }
    system.spawn("seag://local/stateful", StatefulActor);

    const client = system.spawn("seag://local/client-snap", TestClientActor);

    // 2. Trigger Snapshot
    const snapshotResponse = await client.sendAndWait("seag://system/persistence", {
      type: "SNAPSHOT",
      sender: "seag://local/stateful",
      payload: { target_uri: testFile }
    });

    expect(snapshotResponse?.type).toBe("SNAPSHOT_OK"); // PersistenceManager should send this (not implemented yet)

    // 3. Verify file exists and has correct content
    expect(existsSync(testFile)).toBe(true);
    const content = JSON.parse(readFileSync(testFile, "utf-8"));
    expect(content.counter).toBe(42);

    // Cleanup
    if (existsSync(testFile)) unlinkSync(testFile);
  });

  test("Objective 7.1.2: PersistenceManager Restore Logic", async () => {
    const system = new System();
    system.spawn("seag://system/persistence", PersistenceManager);
    system.spawn("seag://system/file-io", FileEffectActor);

    const testFile = "data/test_restore.json";
    const initialState = { name: "Restored Actor", status: "online" };
    writeFileSync(testFile, JSON.stringify(initialState));

    let restoredState: any = null;

    class TargetActor extends Actor {
      async receive(msg: Message) {
        if (msg.type === "PATCH") {
          restoredState = msg.payload;
        }
      }
    }
    system.spawn("seag://local/target", TargetActor);

    const client = system.spawn("seag://local/client-restore", TestClientActor);

    // 1. Trigger Restore
    const restoreResponse = await client.sendAndWait("seag://system/persistence", {
      type: "RESTORE",
      payload: { source_uri: testFile, target_actor: "seag://local/target" }
    });

    expect(restoreResponse?.type).toBe("RESTORE_OK"); // PersistenceManager should send this (not implemented yet)

    // 2. Verify state was patched
    expect(restoredState).not.toBeNull();
    expect(restoredState.name).toBe("Restored Actor");

    // Cleanup
    if (existsSync(testFile)) unlinkSync(testFile);
  });
});