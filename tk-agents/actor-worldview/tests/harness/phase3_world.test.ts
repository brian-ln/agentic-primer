import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress, CapabilityToken } from "../../seag/kernel";
import { FileEffectActor } from "../../seag/file-effect";
import { writeFileSync, existsSync, unlinkSync } from "fs";
import { join } from "node:path";

describe("SEAG Phase 3: The Virtual World", () => {
  const TEST_FILE = join(process.cwd(), "data/test_file.txt");

  // Helper function to create a capability token
  const createToken = (resource: string, action: string): string => {
    const ct: CapabilityToken = { resource, action, expiresAt: Date.now() + 10000 };
    return Buffer.from(JSON.stringify(ct)).toString('base64');
  };

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

  test("Objective 3.1.1: Security Blocking (Unauthorized)", async () => {
    const system = new System();
    system.spawn("seag://system/file-io", FileEffectActor);

    // Create a dummy file for testing
    writeFileSync(TEST_FILE, "hello");

    const client = system.spawn("seag://local/client-unauth", TestClientActor);

    // Act: Try to read without a token
    const response = await client.sendAndWait("seag://system/file-io", {
      type: "READ_FILE",
      payload: { path: TEST_FILE }
    });

    expect(response?.type).toBe("ERROR");
    expect(response?.payload.message).toContain("Unauthorized");

    // Cleanup
    if (existsSync(TEST_FILE)) unlinkSync(TEST_FILE);
  });

  test("Objective 3.1.2: Authorized Read", async () => {
    const system = new System();
    system.spawn("seag://system/file-io", FileEffectActor);

    // Create a dummy file for testing
    writeFileSync(TEST_FILE, "secure content");

    const client = system.spawn("seag://local/client-auth", TestClientActor);

    // Create a valid token
    const resource = "file://" + TEST_FILE;
    const token = createToken(resource, "READ_FILE");

    // Act: Read with token
    const response = await client.sendAndWait("seag://system/file-io", {
      type: "READ_FILE",
      capabilityToken: token,
      payload: { path: TEST_FILE }
    });

    expect(response?.type).toBe("FILE_CONTENT");
    expect(response?.payload.data).toBe("secure content");

    // Cleanup
    if (existsSync(TEST_FILE)) unlinkSync(TEST_FILE);
  });
});