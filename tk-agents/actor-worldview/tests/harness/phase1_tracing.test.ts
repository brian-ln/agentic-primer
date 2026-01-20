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

    // 2. Mock the Gateway Relay to capture trace spans
    const traces: Message[] = [];
    class MockGateway extends Actor {
      async receive(msg: Message) {
        if (msg.type === "SIGNAL" && msg.payload.status === "trace") {
          traces.push(msg);
        }
      }
    }
    system.spawn("seag://system/gateway-relay", MockGateway);

    // 3. Send a TRACED message to A
    system.send("seag://local/a", {
      type: "PING",
      payload: { next: "seag://local/b" },
      meta: { trace: true }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 4. Verify Traces
    // Expect:
    // 1. Kernel dispatching PING to A (Traced) -> Emits trace
    // 2. A sending PING to B (Inherits Trace) -> Kernel dispatching to B -> Emits trace
    
    // Note: The first send ("seag://local/a") is external, so we might not see a dispatch trace 
    // unless the system logs external sends or dispatchLocal handles it.
    // dispatchLocal handles it.
    
    // Expected Traces:
    // 1. seag://system/anonymous -> seag://local/a [PING]
    // 2. seag://local/a -> seag://local/b [PING]
    
    expect(traces.length).toBeGreaterThanOrEqual(2);
    expect(traces[0].payload.detail).toContain("seag://local/a");
    expect(traces[1].payload.detail).toContain("seag://local/b");
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
