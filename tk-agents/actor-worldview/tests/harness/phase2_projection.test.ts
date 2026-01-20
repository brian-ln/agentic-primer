import { expect, test, describe } from "bun:test";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";
import { GraphProjector } from "../../seag/graph-projector";

describe("SEAG Phase 2.2: Graph Projection", () => {
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

  test("Objective 2.2.1: Reachability Query", async () => {
    const system = new System();
    system.spawn("seag://system/projector", GraphProjector);

    const client = system.spawn("seag://local/client", TestClientActor);

    // In a real system, the EventLog would broadcast to the Projector.
    // For this test, we send events directly to the projector.

    // 1. Create a chain: A -> B -> C
    client.send("seag://system/projector", {
      type: "APPEND",
      payload: { source: "node-a", type: "LINK_TO", payload: { to: "node-b", type: "references" }, traceId: "t1" }
    });
    client.send("seag://system/projector", {
      type: "APPEND",
      payload: { source: "node-b", type: "LINK_TO", payload: { to: "node-c", type: "references" }, traceId: "t2" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 2. Query for reachable nodes from A
    const queryResponse = await client.sendAndWait(
      "seag://system/projector",
      { type: "QUERY", payload: { predicate: "reachable", args: { from: "node-a" } } }
    );

    expect(queryResponse?.type).toBe("QUERY_RESULT");
    expect(queryResponse?.payload.results).toEqual(expect.arrayContaining(["node-b", "node-c"]));
  });

  test("Objective 2.2.2: Node State & Link Queries", async () => {
    const system = new System();
    system.spawn("seag://system/projector", GraphProjector);

    const client = system.spawn("seag://local/client-2", TestClientActor);

    // 1. Populate State
    client.send("seag://system/projector", {
      type: "UPDATE_STATE",
      sender: "node-x",
      payload: { status: "active", value: 42 }
    });
    client.send("seag://system/projector", {
      type: "LINK_TO",
      sender: "node-x",
      payload: { from: "node-x", to: "node-y", type: "parent" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 2. Query Node State
    const stateResponse = await client.sendAndWait("seag://system/projector", {
      type: "QUERY",
      payload: { predicate: "node_state", args: { id: "node-x" } }
    });
    expect(stateResponse?.payload.results).toEqual({ status: "active", value: 42 });

    // 3. Query Links
    const linksResponse = await client.sendAndWait("seag://system/projector", {
      type: "QUERY",
      payload: { predicate: "links_from", args: { from: "node-x" } }
    });
    expect(linksResponse?.payload.results).toEqual(expect.arrayContaining(["node-y"]));
  });
});