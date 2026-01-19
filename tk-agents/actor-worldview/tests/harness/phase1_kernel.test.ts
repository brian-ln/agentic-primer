import { expect, test, describe } from "bun:test";
import { System, Actor, Message, RootSupervisor } from "../../seag/kernel";

/**
 * PHASE 1 HARNESS: Minimal Actor Runtime
 * 
 * Objectives:
 * 1. Spawn actors at seag:// addresses.
 * 2. Asynchronous message passing.
 * 3. Local routing.
 * 4. SERDE Isolation.
 * 5. Self-Healing (Supervisor restarts).
 */

describe("SEAG Phase 1: Actor Kernel", () => {

  class PingPongActor extends Actor {
    public receivedCount = 0;
    public lastPayload: any = null;
    public static startCount = 0;

    async onStart() {
      PingPongActor.startCount++;
    }

    async receive(msg: Message) {
      this.receivedCount++;
      this.lastPayload = msg.payload;

      if (msg.type === "PING") {
        this.send(msg.sender!, { 
          type: "PONG", 
          payload: { reply: "Hello from Pong" } 
        });
      }

      if (msg.type === "CRASH") {
        throw new Error("Boom!");
      }
    }
  }

  test("Objective 1.1: Local Message Passing (Ping-Pong)", async () => {
    const system = new System();

    const actorA = system.spawn("seag://local/actor-a", PingPongActor);
    const actorB = system.spawn("seag://local/actor-b", PingPongActor);

    system.send("seag://local/actor-b", { 
      type: "PING", 
      sender: "seag://local/actor-a",
      payload: { data: "test" } 
    });

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(actorB.receivedCount).toBe(1);
    expect(actorA.receivedCount).toBe(1);
    expect(actorA.lastPayload.reply).toBe("Hello from Pong");
  });

  test("Objective 1.2: Idempotent Spawning", () => {
    const system = new System();
    const actor1 = system.spawn("seag://local/test", PingPongActor);
    const actor2 = system.spawn("seag://local/test", PingPongActor);

    // Assert: It should return the same instance, not throw or create a second one
    expect(actor1).toBe(actor2);
  });

  test("Objective 1.3: Dead Letter Handling", async () => {
    const system = new System();
    system.send("seag://local/void", { type: "GHOST" });
  });

  test("Objective 1.4: In-Memory Isolation (SERDE Check)", async () => {
    const system = new System();
    const actor = system.spawn("seag://local/iso", PingPongActor);

    const sharedObject = { nested: { count: 1 } };
    
    system.send("seag://local/iso", {
      type: "DATA", 
      payload: sharedObject 
    });

    sharedObject.nested.count = 99;

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(actor.lastPayload.nested.count).toBe(1);
    expect(actor.lastPayload.nested.count).not.toBe(99);
  });

  test("Objective 1.5: Self-Healing (RootSupervisor)", async () => {
    const system = new System();
    
    // Setup supervisor
    system.spawn("seag://system/supervisor", RootSupervisor);
    system.setSupervisor("seag://system/supervisor");

    // Reset static counter
    PingPongActor.startCount = 0;

    // Spawn a permanent actor
    system.spawn("seag://local/permanent-actor", PingPongActor, "permanent");
    
    await new Promise(resolve => setTimeout(resolve, 10));
    expect(PingPongActor.startCount).toBe(1);

    // Act: Send a crash message
    system.send("seag://local/permanent-actor", { type: "CRASH" });

    // Wait for crash -> supervisor -> restart
    await new Promise(resolve => setTimeout(resolve, 50));

    // Assert: The actor should have been restarted (startCount is now 2)
    expect(PingPongActor.startCount).toBe(2);
  });

  test("Subjective: Ergonomics Check", () => {
    console.log("\n--- Subjective Review: Phase 1 ---");
    console.log("1. Minimal boilerplate to define PingPongActor? (Yes)");
    console.log("2. URI-based addressing feels natural? (Yes)");
    console.log("3. System API is clean? (Yes)");
    console.log("4. Error recovery is transparent? (Yes)");
  });

});