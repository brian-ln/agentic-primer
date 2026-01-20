import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";
import { UserProxy } from "../../seag/user-proxy";
import { BrainAgent } from "../../seag/brain-agent";

describe("SEAG Phase 4.2: The Brain Agent", () => {
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

  // Mock the UserProxy to capture output
  class MockUserProxy extends Actor {
    public static lastOutput: string | null = null;
    async receive(msg: Message) {
      if (msg.type === "OUTPUT") {
        MockUserProxy.lastOutput = msg.payload.content;
      }
    }
  }

  test("Objective 4.2.1: Sense-Think-Act Loop", async () => {
    const system = new System();
    system.spawn("seag://system/brain", BrainAgent);
    system.spawn("seag://local/user-proxy", MockUserProxy);

    MockUserProxy.lastOutput = null;

    const client = system.spawn("seag://local/client-brain", TestClientActor);

    // Act: Send THINK command (e.g., from a user proxy)
    client.send("seag://system/brain", {
      type: "THINK",
      sender: "seag://local/user-proxy",
      payload: { input: "mount data/demo.json" }
    });

    // The UserProxy should receive an OUTPUT from the Brain
    await new Promise(resolve => setTimeout(resolve, 50));
    expect(MockUserProxy.lastOutput).toContain("Mounted data/demo.json");
  });

  test("Objective 4.2.2: Get/Set Commands", async () => {
    const system = new System();
    system.spawn("seag://system/brain", BrainAgent);
    system.spawn("seag://local/target", Actor); // A simple mock actor to get/set state
    system.spawn("seag://local/user-proxy", MockUserProxy);

    MockUserProxy.lastOutput = null;
    
    const client = system.spawn("seag://local/client-setget", TestClientActor);

    // 1. SET
    client.send("seag://system/brain", {
      type: "THINK",
      sender: "seag://local/user-proxy",
      payload: { input: "set seag://local/target updated" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(MockUserProxy.lastOutput).toContain("Updated");

    MockUserProxy.lastOutput = null;
    // 2. GET
    client.send("seag://system/brain", {
      type: "THINK",
      sender: "seag://local/user-proxy",
      payload: { input: "get seag://local/target" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(MockUserProxy.lastOutput).toContain('"updated"'); // Expect the JSON string representation
  });
});