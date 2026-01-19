import { expect, test, describe, afterAll, beforeAll } from "bun:test";
import { System, Actor, Message, CapabilityToken } from "../../seag/kernel";
import { DocumentParser, FragmentNode } from "../../seag/shredder";
import { DocumentActor } from "../../seag/document-actor";
import { FileEffectActor } from "../../seag/file-effect";
import { rm, writeFile, mkdir } from "node:fs/promises";
import { dirname } from "node:path";

/**
 * PHASE 3.3 HARNESS: Two-Way Sync (Watcher)
 * 
 * Objectives:
 * 1. Disk-to-Graph sync: External changes are reflected in actors.
 * 2. Reconciliation: Only changed fragments are updated.
 */

describe("SEAG Phase 3.3: Two-Way Sync", () => {
  const TEST_FILE = "data/watcher_test.json";

  beforeAll(async () => {
    await mkdir(dirname(TEST_FILE), { recursive: true });
    await writeFile(TEST_FILE, JSON.stringify({ status: "original" }));
  });

  afterAll(async () => {
    try { await rm(TEST_FILE); } catch {}
  });

  function createToken(resource: string): string {
    const ct: CapabilityToken = { resource, action: "*", expiresAt: Date.now() + 10000 };
    return Buffer.from(JSON.stringify(ct)).toString('base64');
  }

  test("Objective 3.3.1: External Disk Change Sync", async () => {
    const system = new System();
    const token = createToken(TEST_FILE);

    // Setup actors
    system.spawn("seag://system/file-io", FileEffectActor);
    system.spawn("seag://system/parser", DocumentParser);
    
    const docId = "seag://local/watched-doc";
    system.spawn(docId, DocumentActor);
    system.send(docId, { 
      type: "INIT_DOCUMENT", 
      payload: { path: TEST_FILE, format: "json" },
      capabilityToken: token
    });

    // 1. Initial shred
    system.send("seag://system/parser", {
      type: "SHRED",
      payload: { content: JSON.stringify({ status: "original" }), format: "json", docId }
    });

    // 2. Start watching
    system.send("seag://system/file-io", {
      type: "WATCH_FILE",
      sender: docId,
      payload: { path: TEST_FILE },
      capabilityToken: token
    });

    await new Promise(resolve => setTimeout(resolve, 100));

    // 3. Simulate external disk change
    await writeFile(TEST_FILE, JSON.stringify({ status: "externally-changed" }));

    // Wait for FS event -> Actor cycle
    await new Promise(resolve => setTimeout(resolve, 300));

    // 4. Verify fragment actor
    let fragmentState: any = null;
    class StateReceiver extends Actor {
      async receive(msg: Message) {
        if (msg.type === "STATE") fragmentState = msg.payload;
      }
    }
    system.spawn("seag://local/receiver", StateReceiver);

    system.send(`${docId}/fragments/status`, {
      type: "GET_STATE",
      sender: "seag://local/receiver"
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(fragmentState).toBe("externally-changed");
  });

});
