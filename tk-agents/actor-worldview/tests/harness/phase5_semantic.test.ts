import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { GeminiInferenceActor } from "../../seag/inference-actor";
import { GeminiEmbeddingActor } from "../../seag/embedding-actor";
import { GraphProjector } from "../../seag/graph-projector";

/**
 * PHASE 5.1 HARNESS: Semantic Discovery (AI Integration)
 * 
 * Objectives:
 * 1. Verify Inference Actor (PROMPT) logic.
 * 2. Verify Embedding Actor (EMBED) logic.
 * 3. Verify Vector Search in Projector.
 */

describe("SEAG Phase 5: Semantic Discovery", () => {

  test("Objective 5.1.1: Vector Search Logic", async () => {
    const system = new System();
    system.spawn("seag://system/projector", GraphProjector);

    // 1. Manually set vectors for two nodes
    // Node A: High values in first dimensions
    system.send("seag://system/projector", {
      type: "SET_VECTOR",
      sender: "node-apple",
      payload: { vector: [1.0, 0.1, 0.0] }
    });

    // Node B: High values in last dimensions
    system.send("seag://system/projector", {
      type: "SET_VECTOR",
      sender: "node-banana",
      payload: { vector: [0.0, 0.1, 1.0] }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    // 2. Query with a vector close to 'apple'
    let results: any = null;
    class MockClient extends Actor {
      async receive(msg: Message) {
        if (msg.type === "QUERY_RESULT") results = msg.payload;
      }
    }
    system.spawn("seag://local/client", MockClient);

    system.send("seag://system/projector", {
      type: "QUERY",
      sender: "seag://local/client",
      payload: { 
        predicate: "vector_search", 
        args: { vector: [0.9, 0.2, 0.1], limit: 1 } 
      }
    });

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(results).toHaveLength(1);
    expect(results[0].id).toBe("node-apple");
    expect(results[0].score).toBeGreaterThan(0.9);
  });

  test("Objective 5.1.2: Inference Actor Protocol (Mocked API)", async () => {
    const system = new System();
    
    // We override fetch to avoid real network calls during test
    const originalFetch = global.fetch;
    let lastUrl = "";
    global.fetch = (async (url: string) => {
      lastUrl = url;
      return {
        json: async () => ({
          candidates: [{ content: { parts: [{ text: "Mock Gemini Response" }] } }]
        })
      };
    }) as any;

    system.spawn("seag://system/inference", GeminiInferenceActor);

    let response: string | null = null;
    class MockUser extends Actor {
      async receive(msg: Message) {
        if (msg.type === "RESPONSE") response = msg.payload.text;
      }
    }
    system.spawn("seag://local/user", MockUser);

    system.send("seag://system/inference", {
      type: "PROMPT",
      sender: "seag://local/user",
      payload: { text: "What is SEAG?" }
    });

    await new Promise(resolve => setTimeout(resolve, 50));
    expect(response).toBe("Mock Gemini Response");
    expect(lastUrl).toContain("gemini-3-flash-preview");

    global.fetch = originalFetch;
  });

});
