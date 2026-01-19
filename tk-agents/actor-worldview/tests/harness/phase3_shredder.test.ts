import { expect, test, describe, afterAll, beforeAll } from "bun:test";
import { System, Actor, Message, CapabilityToken } from "../../seag/kernel";
import { DocumentParser, FragmentNode } from "../../seag/shredder";
import { GraphProjector } from "../../seag/graph-projector";
import { DocumentActor } from "../../seag/document-actor";
import { FileEffectActor } from "../../seag/file-effect";
import { rm, readFile, mkdir } from "node:fs/promises";
import { dirname } from "node:path";

/**
 * PHASE 3.2 HARNESS: Structural Destructuring (Shredder)
 * 
 * Objectives:
 * 1. Decomposition: Shred a JSON blob into multiple FragmentNode actors.
 * 2. Back-Propagation: Patching a fragment updates the physical file.
 */

describe("SEAG Phase 3.2: The Shredder", () => {
  const TEST_FILE = "data/shredder_test.json";

  beforeAll(async () => {
    await mkdir(dirname(TEST_FILE), { recursive: true });
  });

  afterAll(async () => {
    try { await rm(TEST_FILE); } catch {}
  });

  function createToken(resource: string): string {
    const ct: CapabilityToken = { resource, action: "*", expiresAt: Date.now() + 10000 };
    return Buffer.from(JSON.stringify(ct)).toString('base64');
  }

  test("Objective 3.2.1: JSON Decomposition", async () => {
    const system = new System();
    
    // Setup infrastructure
    system.spawn("seag://system/projector", GraphProjector);
    system.spawn("seag://system/parser", DocumentParser);

    const docId = "seag://local/config-file";
    system.spawn(docId, DocumentActor); // Fix: DocumentActor must exist to receive registration

    const content = JSON.stringify({ 
      version: "1.0", 
      author: "SEAG",
      settings: { theme: "dark" }
    });

    // Act: Shred the document
    system.send("seag://system/parser", {
      type: "SHRED",
      payload: { content, format: "json", docId }
    });

    await new Promise(resolve => setTimeout(resolve, 100));

    // Verify: Fragment actors should exist
    const actors = system.inspect();
    expect(actors).toContain(`${docId}/fragments/version`);
    expect(actors).toContain(`${docId}/fragments/author`);

    // 3. Verify Shredding
    // We expect 3 fragments: name, version, status
    let fragmentState = null;
    class MockReceiver extends Actor {
      async receive(msg: Message) {
        if (msg.type === "STATE") fragmentState = msg.payload;
      }
    }
    system.spawn("seag://local/receiver", MockReceiver);

    system.send(`${docId}/fragments/author`, {
      type: "GET",
      sender: "seag://local/receiver"
    });

    await new Promise(resolve => setTimeout(resolve, 100));
    expect(fragmentState).toBe("SEAG");
  });

  test("Objective 3.2.2: Back-Propagation to Disk", async () => {
    const system = new System();
    const token = createToken(TEST_FILE);

    // Setup actors
    system.spawn("seag://system/file-io", FileEffectActor);
    system.spawn("seag://system/projector", GraphProjector);
    system.spawn("seag://system/parser", DocumentParser);
    
    const docId = "seag://local/managed-doc";
    system.spawn(docId, DocumentActor);
    system.send(docId, { 
      type: "INIT_DOCUMENT", 
      payload: { path: TEST_FILE, format: "json" },
      capabilityToken: token // Initial authorization
    });

    // 1. Shred initial content
    const initialContent = JSON.stringify({ name: "old-name" });
    system.send("seag://system/parser", {
      type: "SHRED",
      payload: { content: initialContent, format: "json", docId }
    });

    await new Promise(resolve => setTimeout(resolve, 100));

    // 2. Patch the fragment actor
    system.send(`${docId}/fragments/name`, {
      type: "PATCH",
      payload: "new-name",
      capabilityToken: token // Required for the final write
    });

    // Wait for Fragment -> Document -> FileIO cycle
    await new Promise(resolve => setTimeout(resolve, 200));

    // 3. Verify physical file
    const diskContent = await readFile(TEST_FILE, "utf-8");
    const data = JSON.parse(diskContent);
    expect(data.name).toBe("new-name");
  });

});