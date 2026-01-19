import { expect, test, describe, beforeAll, afterAll } from "bun:test";
import { System, Actor, Message, CapabilityToken } from "../../seag/kernel";
import { FileEffectActor } from "../../seag/file-effect";
import { rm, writeFile } from "node:fs/promises";

/**
 * PHASE 3 HARNESS: The Virtual World (I/O Abstraction)
 * 
 * Objectives:
 * 1. Security check: Block unauthorized I/O.
 * 2. authorized I/O: Perform reads/writes via actors.
 */

describe("SEAG Phase 3: The Virtual World", () => {
  const TEST_FILE = "data/test_file.txt";

  beforeAll(async () => {
    await writeFile(TEST_FILE, "Initial Content");
  });

  afterAll(async () => {
    try { await rm(TEST_FILE); } catch {}
  });

  function createToken(resource: string, action: string): string {
    const ct: CapabilityToken = { resource, action, expiresAt: Date.now() + 10000 };
    return Buffer.from(JSON.stringify(ct)).toString('base64');
  }

  test("Objective 3.1.1: Security Blocking (Unauthorized)", async () => {
    const system = new System();
    system.spawn("seag://system/file-io", FileEffectActor);

    let lastError: string | null = null;
    class UserActor extends Actor {
      async receive(msg: Message) {
        if (msg.type === "ERROR") lastError = msg.payload.message;
      }
    }
    system.spawn("seag://local/user", UserActor);

    // Act: Try to read without a token
    system.send("seag://system/file-io", {
      type: "READ_FILE",
      sender: "seag://local/user",
      payload: { path: TEST_FILE }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(lastError).toContain("Unauthorized");
  });

  test("Objective 3.1.2: Authorized Read", async () => {
    const system = new System();
    system.spawn("seag://system/file-io", FileEffectActor);

    let fileData: string | null = null;
    class AuthorizedUser extends Actor {
      async receive(msg: Message) {
        if (msg.type === "FILE_CONTENT") fileData = msg.payload.data;
      }
    }
    system.spawn("seag://local/user-auth", AuthorizedUser);

    // Create a valid token
    const token = createToken(TEST_FILE, "READ_FILE");

    // Act: Read with token
    system.send("seag://system/file-io", {
      type: "READ_FILE",
      sender: "seag://local/user-auth",
      capabilityToken: token,
      payload: { path: TEST_FILE }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(fileData).toBe("Initial Content");
  });

});
