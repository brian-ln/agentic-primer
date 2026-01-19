import { expect, test, describe, afterAll, beforeAll } from "bun:test";
import { System, Actor, Message, CapabilityToken } from "../../seag/kernel";
import { FileEffectActor } from "../../seag/file-effect";
import { writeFile, rm } from "node:fs/promises";

/**
 * Protocol Compliance Test: FileIO
 * Verifies that an actor adheres to the FileIO contract defined in ap/PROTOCOLS.model.lisp.
 * 
 * Contract:
 * (on READ_FILE (path) -> (yields FILE_CONTENT | ERROR))
 * (on WRITE_FILE (path data) -> (yields WRITE_OK | ERROR))
 * (on WATCH_FILE (path) -> (yields WATCH_OK -> FILE_CHANGED))
 */

describe("Protocol Compliance: FileIO (FileEffectActor)", () => {
  const TEST_PATH = "data/compliance_test.txt";
  const system = new System();
  let actorId: string;
  let token: string;

  beforeAll(async () => {
    await writeFile(TEST_PATH, "initial");
    const ct: CapabilityToken = { resource: "*", action: "*", expiresAt: Date.now() + 10000 };
    token = Buffer.from(JSON.stringify(ct)).toString('base64');
    
    actorId = "seag://system/file-io";
    system.spawn(actorId, FileEffectActor);
  });

  afterAll(async () => {
    try { await rm(TEST_PATH); } catch {}
  });

  test("READ_FILE returns FILE_CONTENT", async () => {
    let result: any = null;
    class Reader extends Actor {
      async receive(msg: Message) { result = msg; }
    }
    system.spawn("seag://local/reader", Reader);

    system.send(actorId, {
      type: "READ_FILE",
      sender: "seag://local/reader",
      payload: { path: TEST_PATH },
      capabilityToken: token
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    
    expect(result).not.toBeNull();
    expect(result.type).toBe("FILE_CONTENT");
    expect(result.payload.data).toBe("initial");
  });

  test("WRITE_FILE returns WRITE_OK and updates disk", async () => {
    let result: any = null;
    class Writer extends Actor {
      async receive(msg: Message) { result = msg; }
    }
    system.spawn("seag://local/writer", Writer);

    system.send(actorId, {
      type: "WRITE_FILE",
      sender: "seag://local/writer",
      payload: { path: TEST_PATH, data: "updated" },
      capabilityToken: token
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(result).not.toBeNull();
    expect(result.type).toBe("WRITE_OK");
    
    const content = await Bun.file(TEST_PATH).text();
    expect(content).toBe("updated");
  });

  test("WATCH_FILE returns WATCH_OK and emits FILE_CHANGED", async () => {
    const events: Message[] = [];
    class Watcher extends Actor {
      async receive(msg: Message) { events.push(msg); }
    }
    system.spawn("seag://local/watcher", Watcher);

    // 1. Subscribe
    system.send(actorId, {
      type: "WATCH_FILE",
      sender: "seag://local/watcher",
      payload: { path: TEST_PATH },
      capabilityToken: token
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(events.length).toBeGreaterThan(0);
    expect(events[0].type).toBe("WATCH_OK");

    // 2. Trigger Change
    await writeFile(TEST_PATH, "changed");
    await new Promise(resolve => setTimeout(resolve, 200)); // Allow fs watcher to fire

    // 3. Verify Event
    const changeEvent = events.find(e => e.type === "FILE_CHANGED");
    expect(changeEvent).toBeDefined();
    expect(changeEvent?.payload.content).toBe("changed");
  });

  test("Missing Token returns ERROR", async () => {
    let result: any = null;
    class Hacker extends Actor {
      async receive(msg: Message) { result = msg; }
    }
    system.spawn("seag://local/hacker", Hacker);

    system.send(actorId, {
      type: "READ_FILE",
      sender: "seag://local/hacker",
      payload: { path: TEST_PATH }
      // No token
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(result.type).toBe("ERROR");
    expect(result.payload.message).toContain("Unauthorized");
  });
});
