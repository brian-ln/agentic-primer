import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";

describe("SEAG Phase 1: On-Demand Tracing", () => {

  test("Objective 1.6: Trace Flag Propagation", async () => {
    const system = new System();
    
    // 1. Setup a chain of actors: A -> B -> C
    class ChainActor extends Actor {
      async receive(msg: Message) {
        if (msg.payload.next) {
          this.send(msg.payload.next, { ...msg, payload: { next: null } });
        }
      }
    }
    
    system.spawn("seag://local/a", ChainActor);
    system.spawn("seag://local/b", ChainActor);
    system.spawn("seag://local/c", ChainActor);

    // 2. Mock the Trace Topic to capture trace spans
    const traces: any[] = [];
    class MockTopic extends Actor {
      async receive(msg: Message) {
        if (msg.type === "PUBLISH" && msg.payload.messageType) {
          traces.push(msg.payload);
        }
      }
    }
    system.spawn("seag://system/topic/trace", MockTopic);

    // 3. Send a TRACED message to A
    system.send("seag://local/a", {
      type: "PING",
      payload: { next: "seag://local/b" },
      meta: { trace: true }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 4. Verify Traces
    expect(traces.length).toBeGreaterThanOrEqual(2);
    expect(traces[0].sender).toContain("seag://system/anonymous");
    expect(traces[0].target).toContain("seag://local/a");
    expect(traces[0].messageType).toBe("PING");
    
    expect(traces[1].sender).toContain("seag://local/a");
    expect(traces[1].target).toContain("seag://local/b");
    expect(traces[1].messageType).toBe("PING");
  });

  test("Objective 1.7: No Trace Leakage", async () => {
    const system = new System();
    const traces: Message[] = [];
    class MockGateway extends Actor {
      async receive(msg: Message) { traces.push(msg); }
    }
    system.spawn("seag://system/gateway-relay", MockGateway);
    
    class ChainActor extends Actor {
      async receive(msg: Message) {
        // Echo back
      }
    }
    system.spawn("seag://local/a", ChainActor);

    // Send normal message
    system.send("seag://local/a", { type: "PING" });
    
    await new Promise(resolve => setTimeout(resolve, 20));
    expect(traces.length).toBe(0);
  });

});
