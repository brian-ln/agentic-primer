import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";
import { UserProxy } from "../../seag/user-proxy";
import { BrainAgent } from "../../seag/brain-agent";
import { Gateway } from "../../seag/gateway";
import { EventLogActor } from "../../seag/event-log";

describe("SEAG Phase 4.1: Gateway Integration", () => {
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

  test("Objective 4.1.1: E2E Input Flow", async () => {
    const system = new System();
    system.spawn("seag://system/brain", BrainAgent);
    system.spawn("seag://local/user-proxy", UserProxy);
    system.spawn("seag://system/interaction-log", EventLogActor); // Mock event log

    const client = system.spawn("seag://local/client-gateway", TestClientActor);

    // The Gateway talks directly to the System, so this is a bit different.
    // We'll mock the incoming message as if it came from the Gateway.

    // Simulate incoming message from WebSocket/HTTP Gateway
    system.send("seag://local/user-proxy", {
      type: "INPUT",
      sender: "seag://local/client-gateway", // Gateway is technically the sender here
      payload: { input: "Hello SEAG" }
    });

    // The UserProxy should forward this to the Brain
    // We can't directly assert Brain's state without making it testable,
    // but we can check if the UserProxy received an OUTPUT.
    const response = await client.sendAndWait("seag://local/user-proxy", { type: "OUTPUT" });

    expect(response?.type).toBe("OUTPUT");
    expect(response?.payload.content).toContain("Unknown command"); // Brain should report this
  });
});