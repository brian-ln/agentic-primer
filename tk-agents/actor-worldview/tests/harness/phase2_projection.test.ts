import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { GraphProjector } from "../../seag/graph-projector";

/**
 * PHASE 2.2 HARNESS: Graph Projection & Queryability
 * 
 * Objectives:
 * 1. Project events into a queryable graph.
 * 2. Calculate transitive reachability (linked nodes).
 */

describe("SEAG Phase 2.2: Graph Projection", () => {

  test("Objective 2.2.1: Reachability Query", async () => {
    const system = new System();
    
    // 1. Setup Projector
    const projector = system.spawn("seag://system/projector", GraphProjector);
    // In a real system, the EventLog would broadcast to the Projector.
    // For this test, we send events directly to the projector.

    // 2. Create a chain: A -> B -> C
    system.send("seag://system/projector", {
      type: "APPEND",
      payload: { source: "node-a", type: "LINK_TO", payload: { to: "node-b", type: "references" }, traceId: "t1" }
    });

    system.send("seag://system/projector", {
      type: "APPEND",
      payload: { source: "node-b", type: "LINK_TO", payload: { to: "node-c", type: "references" }, traceId: "t1" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 3. Query reachability from A
    let queryResult: any = null;
    class QueryReceiver extends Actor {
      async receive(msg: Message) {
        if (msg.type === "QUERY_RESULT") queryResult = msg.payload;
      }
    }
    system.spawn("seag://local/receiver", QueryReceiver);

    system.send("seag://system/projector", {
      type: "QUERY",
      sender: "seag://local/receiver",
      payload: { predicate: "reachable", args: { from: "node-a" } }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // Assert: node-a should reach both b and c
    expect(queryResult).toContain("node-b");
    expect(queryResult).toContain("node-c");
    expect(queryResult).toContain("node-a");
  });

  test("Objective 2.2.2: Node State & Link Queries", async () => {
    const system = new System();
    system.spawn("seag://system/projector", GraphProjector);

    // 1. Populate State
    system.send("seag://system/projector", {
      type: "UPDATE_STATE",
      sender: "node-x",
      payload: { status: "active", value: 42 }
    });

    // 2. Populate Links
    system.send("seag://system/projector", {
      type: "LINK_TO",
      sender: "node-x",
      payload: { to: "node-y", type: "parent-of" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 3. Setup Receiver
    let stateResult: any = null;
    let linkResult: any = null;
    class QueryReceiver extends Actor {
      async receive(msg: Message) {
        if (msg.type === "QUERY_RESULT") {
          if (Array.isArray(msg.payload)) linkResult = msg.payload;
          else stateResult = msg.payload;
        }
      }
    }
    system.spawn("seag://local/receiver", QueryReceiver);

    // 4. Test get_node
    system.send("seag://system/projector", {
      type: "QUERY",
      sender: "seag://local/receiver",
      payload: { predicate: "get_node", args: { id: "node-x" } }
    });

    // 5. Test linked
    system.send("seag://system/projector", {
      type: "QUERY",
      sender: "seag://local/receiver",
      payload: { predicate: "linked", args: { from: "node-x" } }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // Assertions
    expect(stateResult).toEqual({ status: "active", value: 42 });
    expect(linkResult).toHaveLength(1);
    expect(linkResult[0]).toEqual({ from: "node-x", to: "node-y", type: "parent-of" });
  });

});
