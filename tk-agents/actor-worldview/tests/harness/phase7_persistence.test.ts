import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { PersistenceManager } from "../../seag/persistence-manager";
import { FileEffectActor } from "../../seag/file-effect";
import { existsSync, unlinkSync, readFileSync } from "fs";

describe("SEAG Phase 7: Persistence Subsystem", () => {
  
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

    // 2. Trigger Snapshot
    system.send("seag://system/persistence", {
      type: "SNAPSHOT",
      sender: "seag://local/stateful",
      payload: { target_uri: testFile }
    });

    await new Promise(resolve => setTimeout(resolve, 200));

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
    const fs = require("fs");
    fs.writeFileSync(testFile, JSON.stringify(initialState));

    let restoredState: any = null;

    class TargetActor extends Actor {
      async receive(msg: Message) {
        if (msg.type === "PATCH") {
          restoredState = msg.payload;
        }
      }
    }
    system.spawn("seag://local/target", TargetActor);

    // 1. Trigger Restore
    system.send("seag://system/persistence", {
      type: "RESTORE",
      payload: { source_uri: testFile, target_actor: "seag://local/target" }
    });

    await new Promise(resolve => setTimeout(resolve, 200));

    // 2. Verify state was patched
    expect(restoredState).not.toBeNull();
    expect(restoredState.name).toBe("Restored Actor");

    // Cleanup
    if (fs.existsSync(testFile)) fs.unlinkSync(testFile);
  });
});
