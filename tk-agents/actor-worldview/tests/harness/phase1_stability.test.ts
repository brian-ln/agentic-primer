import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";

/**
 * PHASE 1.3 HARNESS: Stability & Observability
 * 
 * Objectives:
 * 1. Trace ID propagation across multiple hops.
 * 2. Loop avoidance (hop limit).
 */

describe("SEAG Phase 1.3: Stability & Observability", () => {

  class RelayActor extends Actor {
    public lastTraceId: string | undefined;
    public lastHops: number | undefined;

    async receive(msg: Message) {
      this.lastTraceId = msg.traceId;
      this.lastHops = msg.hops;

      if (msg.payload?.forwardTo) {
        this.send(msg.payload.forwardTo, { 
          type: "FORWARD", 
          payload: msg.payload.nextPayload || {}
        });
      }
    }
  }

  class LoopActor extends Actor {
    public static totalHops = 0;
    async receive(msg: Message) {
      LoopActor.totalHops = msg.hops || 0;
      this.send(msg.payload.partner, { 
        type: "LOOP", 
        payload: { partner: this.id } 
      });
    }
  }

  test("Objective 1.3.1: Trace ID Propagation", async () => {
    const system = new System();
    const actorA = system.spawn("seag://local/a", RelayActor);
    const actorB = system.spawn("seag://local/b", RelayActor);
    const actorC = system.spawn("seag://local/c", RelayActor);

    // Chain: A -> B -> C
    system.send("seag://local/a", { 
      type: "START", 
      payload: { forwardTo: "seag://local/b", nextPayload: { forwardTo: "seag://local/c" } } 
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(actorA.lastTraceId).toBeDefined();
    expect(actorB.lastTraceId).toBe(actorA.lastTraceId);
    expect(actorC.lastTraceId).toBe(actorA.lastTraceId);
    
    expect(actorA.lastHops).toBe(1);
    expect(actorB.lastHops).toBe(2);
    expect(actorC.lastHops).toBe(3);
  });

  test("Objective 1.3.2: Loop Avoidance (TTL)", async () => {
    const system = new System();
    const actor1 = system.spawn("seag://local/1", LoopActor);
    const actor2 = system.spawn("seag://local/2", LoopActor);

    LoopActor.totalHops = 0;

    // Start a loop between 1 and 2
    system.send("seag://local/1", { 
      type: "START", 
      payload: { partner: "seag://local/2" } 
    });

    // Wait long enough for 100+ hops (each is a setTimeout 0)
    await new Promise(resolve => setTimeout(resolve, 200));

    // The loop should have been broken at 100 hops
    expect(LoopActor.totalHops).toBeLessThanOrEqual(100);
    // Since it's async, it might be exactly 100 or slightly less depending on when the check happens
    expect(LoopActor.totalHops).toBeGreaterThan(90); 
  });

  test("Objective 1.3.3: Cross-Boundary Hop Propagation", async () => {
    const system = new System();
    const actor = system.spawn("seag://local/target", RelayActor);

    // Simulate an incoming remote message with existing hops
    const incomingHops = 5;
    const incomingTrace = "trace-remote-123";

    // This simulates what a Transport would do when receiving a wire envelope
    system.send("seag://local/target", {
      type: "REMOTE_MSG",
      traceId: incomingTrace,
      hops: incomingHops,
      sender: "seag://remote/sender"
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // The kernel should have incremented the hops from 5 to 6 upon entry
    expect(actor.lastHops).toBe(6);
    expect(actor.lastTraceId).toBe(incomingTrace);
  });

});
