import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";
import { GraphProjector } from "../../seag/graph-projector";
import { GeminiInference } from "../../seag/ai/inference";
import { CredentialProviderActor } from "../../seag/credential-provider";

describe("SEAG Phase 5: AI Integration", () => {
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

  // We need a mock user proxy for the inference actor to send responses to.
  class MockUser extends Actor {
    public static lastResponse: string | null = null;
    public static lastError: any | null = null;
    async receive(msg: Message) {
      if (msg.type === "RESPONSE") MockUser.lastResponse = msg.payload.text;
      if (msg.type === "ERROR") MockUser.lastError = msg.payload;
    }
  }

  test("Objective 5.1.1: Vector Search Logic", async () => {
    const system = new System();
    system.spawn("seag://system/projector", GraphProjector);

    const client = system.spawn("seag://local/client-proj", TestClientActor);

    // 1. Manually set vectors for two nodes
    client.send("seag://system/projector", {
      type: "SET_VECTOR",
      payload: { id: "node-apple", vector: [1.0, 0.1, 0.0] }
    });
    client.send("seag://system/projector", {
      type: "SET_VECTOR",
      payload: { id: "node-banana", vector: [0.0, 0.1, 1.0] }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 2. Query for a vector similar to Node A
    const queryResponse = await client.sendAndWait("seag://system/projector", {
      type: "QUERY",
      payload: { predicate: "vector_search", args: { vector: [0.9, 0.2, 0.0], limit: 1 } }
    });

    expect(queryResponse?.payload.results).toHaveLength(1);
    expect(queryResponse?.payload.results[0].id).toBe("node-apple");
  });

  test("Objective 5.1.2: Inference Actor Protocol (REAL API)", async () => {
    const system = new System();
    console.log("VERTEX MODE:", process.env.GOOGLE_GENAI_USE_VERTEXAI);
    console.log("PROJECT:", process.env.GOOGLE_CLOUD_PROJECT);

    system.spawn("seag://system/credentials", CredentialProviderActor);
    await new Promise(resolve => setTimeout(resolve, 50)); // Wait for CredentialProvider to start
    
    system.spawn("seag://system/inference", GeminiInference);
    await new Promise(resolve => setTimeout(resolve, 50)); // Wait for onStart

    // Reset mocks before each test
    MockUser.lastResponse = null;
    MockUser.lastError = null;

    const client = system.spawn("seag://local/client-inf", TestClientActor);

    // The MockUser will be the actual sender that receives the response
    system.spawn("seag://local/user", MockUser);

    client.send("seag://system/inference", {
      type: "PROMPT",
      sender: "seag://local/user",
      payload: { 
        text: "Say 'Hello from Gemini 3 Pro'",
        params: { model: "gemini-3-pro-preview" }
      }
    });

    await new Promise(resolve => setTimeout(resolve, 15000));
    
    expect(MockUser.lastResponse).not.toBeNull();
    expect(MockUser.lastResponse?.toLowerCase()).toContain("hello");
    expect(MockUser.lastError).toBeNull();
  }, 20000);
});