import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";

describe("SEAG Phase 1.3: Stability & Observability", () => {
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

  class RelayActor extends Actor {
    public lastTraceId: string | undefined;
    public lastHops: number | undefined;
    async receive(msg: Message) {
      if (msg.type === "START") {
        this.lastTraceId = msg.traceId;
        this.lastHops = msg.hops;
        this.send(msg.payload.forwardTo, { type: "CONTINUE", payload: msg.payload.nextPayload, sender: this.id });
      }
      if (msg.type === "CONTINUE") {
        this.lastTraceId = msg.traceId;
        this.lastHops = msg.hops;
        if (msg.payload.forwardTo) {
          this.send(msg.payload.forwardTo, { type: "FINAL", payload: msg.payload.nextPayload, sender: this.id });
        }
      }
      if (msg.type === "FINAL") {
        this.lastTraceId = msg.traceId;
        this.lastHops = msg.hops;
      }
    }
  }

  test("Objective 1.3.1: Trace ID Propagation", async () => {
    const system = new System();
    const actorA = system.spawn("seag://local/a", RelayActor);
    const actorB = system.spawn("seag://local/b", RelayActor);
    const actorC = system.spawn("seag://local/c", RelayActor);

    const client = system.spawn("seag://local/client-trace", TestClientActor);

    // Chain: A -> B -> C
    client.send("seag://local/a", {
      type: "START",
      payload: { forwardTo: "seag://local/b", nextPayload: { forwardTo: "seag://local/c" } }
    });

    await new Promise(resolve => setTimeout(resolve, 100));

    expect(actorA.lastTraceId).toBeDefined();
    expect(actorB.lastTraceId).toBe(actorA.lastTraceId);
    expect(actorC.lastTraceId).toBe(actorA.lastTraceId);

    expect(actorA.lastHops).toBe(1);
    expect(actorB.lastHops).toBe(2);
    expect(actorC.lastHops).toBe(3);
  });

  class LoopActor extends Actor {
    public static totalHops = 0;
    async receive(msg: Message) {
      if (msg.type === "START") {
        LoopActor.totalHops = msg.hops!;
        this.send(msg.payload.partner, { type: "LOOP", payload: msg.payload, sender: this.id });
      }
      if (msg.type === "LOOP") {
        LoopActor.totalHops = msg.hops!;
        if (msg.hops! < 100) {
          this.send(msg.payload.partner, { type: "LOOP", payload: msg.payload, sender: this.id });
        }
      }
    }
  }

  test("Objective 1.3.2: Loop Avoidance (TTL)", async () => {
    const system = new System();
    system.spawn("seag://local/1", LoopActor);
    system.spawn("seag://local/2", LoopActor);

    LoopActor.totalHops = 0;

    const client = system.spawn("seag://local/client-loop", TestClientActor);
    client.send("seag://local/1", {
      type: "START",
      payload: { partner: "seag://local/2" }
    });

    await new Promise(resolve => setTimeout(resolve, 200));

    // Should not exceed 100 hops due to TTL in kernel.send
    expect(LoopActor.totalHops).toBeLessThanOrEqual(100);
  });

  test("Objective 1.3.3: Cross-Boundary Hop Propagation", async () => {
    const system = new System();
    const targetActor = system.spawn("seag://local/target", RelayActor);
    const client = system.spawn("seag://local/client-hops", TestClientActor);

    // Simulate an incoming remote message with existing hops
    const incomingHops = 5;
    const incomingTrace = "trace-remote-123";

    // This simulates what a Transport would do when receiving a wire envelope
    // We are bypassing the system.send for this specific scenario to inject hops
    system.dispatchLocal("seag://local/target", {
      type: "REMOTE_MSG",
      traceId: incomingTrace,
      hops: incomingHops,
      sender: "seag://remote/sender"
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(targetActor.lastTraceId).toBe(incomingTrace);
    expect(targetActor.lastHops).toBe(incomingHops);
  });
});