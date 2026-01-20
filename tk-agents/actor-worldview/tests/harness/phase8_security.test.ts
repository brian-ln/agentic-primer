import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress, CapabilityToken } from "../../seag/kernel";
import { CredentialProviderActor } from "../../seag/credential-provider";

describe("SEAG Phase 8: Capability-Based Security", () => {
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

  test("Objective 8.1.1: CredentialProvider Security", async () => {
    const system = new System();
    system.spawn("seag://system/credentials", CredentialProviderActor);

    const rogueClient = system.spawn("seag://local/rogue-client", TestClientActor);
    const authorizedClient = system.spawn("seag://local/authorized-client", TestClientActor);

    // 1. Rogue actor tries to get a secret WITHOUT a capability -> Should fail
    const rogueResponse = await rogueClient.sendAndWait("seag://system/credentials", {
      type: "GET_CREDENTIALS",
      payload: { service_id: "google-ai-studio" }
    });
    
    expect(rogueResponse?.type).toBe("ERROR");
    expect(rogueResponse?.payload.message).toContain("Unauthorized");

    // 2. Grant a valid capability to the authorized actor (simulated by client sending token)
    const resource = "seag://system/credentials/google-ai-studio";
    const capability: CapabilityToken = { resource, action: "GET", expiresAt: system.clock.now() + 10000 };
    const token = Buffer.from(JSON.stringify(capability)).toString('base64');

    // 3. Authorized actor tries to get a secret WITH the capability -> Should succeed
    const authorizedResponse = await authorizedClient.sendAndWait("seag://system/credentials", {
      type: "GET_CREDENTIALS",
      payload: { service_id: "google-ai-studio" },
      capabilityToken: token
    });

    expect(authorizedResponse?.type).toBe("CREDENTIALS");
    expect(authorizedResponse?.payload.credential_type).toBe("api_key");
  });
});