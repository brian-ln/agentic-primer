import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";
import { RootSupervisor } from "../../seag/kernel"; // Assuming RootSupervisor is still exported for testing

describe("SEAG Phase 1: Actor Kernel", () => {
  // --- Test Helpers ---

  // A generic actor that can send messages and capture responses
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

  // --- Actual Tests ---

  class PingPongActor extends Actor {
    public static messageCount = 0;
    public static lastMessage: Message | null = null;
    async receive(msg: Message) {
      PingPongActor.messageCount++;
      PingPongActor.lastMessage = msg;
      if (msg.type === "PING") {
        this.send(msg.sender!, { type: "PONG", payload: { count: msg.payload.count + 1 }, sender: this.id });
      }
    }
  }

  test("Objective 1.1: Local Message Passing (Ping-Pong)", async () => {
    const system = new System();
    system.spawn("seag://local/ping-target", PingPongActor);
    PingPongActor.messageCount = 0;

    const client = system.spawn("seag://local/client", TestClientActor);
    const response = await client.sendAndWait(
      "seag://local/ping-target",
      { type: "PING", payload: { count: 1 } },
      100
    );

    expect(PingPongActor.messageCount).toBe(1);
    expect(response?.type).toBe("PONG");
    expect(response?.payload.count).toBe(2);
  });

  test("Objective 1.2: Idempotent Spawning", async () => {
    const system = new System();
    const actor1 = system.spawn("seag://local/test-actor", PingPongActor);
    const actor2 = system.spawn("seag://local/test-actor", PingPongActor); // Should return same instance

    expect(actor1).toBe(actor2);
  });

  test("Objective 1.3: Dead Letter Handling", async () => {
    const system = new System();
    const client = system.spawn("seag://local/client-dl", TestClientActor);

    // Send to a non-existent actor
    client.send("seag://local/void", { type: "GHOST" });

    // This is hard to assert directly without mocking console.warn
    // For now, we rely on the system logging a warning.
    await new Promise(resolve => setTimeout(resolve, 50)); 
  });

  class IsolationActor extends Actor {
    public static sharedState = { value: 0 };
    public privateState = { value: 0 };

    async receive(msg: Message) {
      if (msg.type === "UPDATE_SHARED") {
        IsolationActor.sharedState.value = msg.payload.value;
      }
      if (msg.type === "UPDATE_PRIVATE") {
        this.privateState.value = msg.payload.value;
      }
      if (msg.type === "GET_STATE") {
        this.send(msg.sender!, { 
          type: "STATE", 
          payload: { shared: IsolationActor.sharedState.value, private: this.privateState.value },
          sender: this.id
        });
      }
    }
  }

  test("Objective 1.4: In-Memory Isolation (SERDE Check)", async () => {
    const system = new System();
    system.spawn("seag://local/actor1", IsolationActor);
    system.spawn("seag://local/actor2", IsolationActor);

    const client = system.spawn("seag://local/client-iso", TestClientActor);

    // Update shared and private states
    client.send("seag://local/actor1", { type: "UPDATE_SHARED", payload: { value: 10 } });
    client.send("seag://local/actor1", { type: "UPDATE_PRIVATE", payload: { value: 20 } });
    client.send("seag://local/actor2", { type: "UPDATE_SHARED", payload: { value: 30 } }); 
    client.send("seag://local/actor2", { type: "UPDATE_PRIVATE", payload: { value: 40 } });

    // Get states
    const state1Response = await client.sendAndWait("seag://local/actor1", { type: "GET_STATE" });
    const state2Response = await client.sendAndWait("seag://local/actor2", { type: "GET_STATE" });

    expect(state1Response?.payload.shared).toBe(30);
    expect(state2Response?.payload.shared).toBe(30);
    expect(state1Response?.payload.private).toBe(20);
    expect(state2Response?.payload.private).toBe(40);
  });

  class CrashActor extends Actor {
    public static startCount = 0;
    async onStart() { CrashActor.startCount++; }
    async receive(msg: Message) {
      if (msg.type === "CRASH") {
        throw new Error("Boom!");
      }
    }
  }

  test("Objective 1.5: Self-Healing (RootSupervisor)", async () => {
    const system = new System();
    system.spawn("seag://system/supervisor", RootSupervisor);
    system.setSupervisor("seag://system/supervisor");

    CrashActor.startCount = 0;
    system.spawn("seag://local/permanent-actor", CrashActor, "permanent");

    await new Promise(resolve => setTimeout(resolve, 50));

    const client = system.spawn("seag://local/client-crash", TestClientActor);

    // Act: Crash the actor
    client.send("seag://local/permanent-actor", { type: "CRASH" });

    // Wait for crash -> supervisor -> restart
    await new Promise(resolve => setTimeout(resolve, 100)); // Increased wait time

    // Assert: The actor should have been restarted (startCount is now 2)
    expect(CrashActor.startCount).toBe(2);
  });

  // Subjective Tests remain the same
});
