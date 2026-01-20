import { expect, test, describe } from "bun:test";
import { System, Actor, Message } from "../../seag/kernel";
import { VertexInferenceActor } from "../../seag/inference/google/vertex";
import { GeminiEmbeddingActor } from "../../seag/embedding-actor";
import { GraphProjector } from "../../seag/graph-projector";
import { CredentialProviderActor } from "../../seag/credential-provider";

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
      payload: { id: "node-apple", vector: [1.0, 0.1, 0.0] }
    });

    // Node B: High values in last dimensions
    system.send("seag://system/projector", {
      type: "SET_VECTOR",
      payload: { id: "node-banana", vector: [0.0, 0.1, 1.0] }
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

    test("Objective 5.1.2: Inference Actor Protocol (REAL API)", async () => {
      const system = new System();
      console.log("VERTEX MODE:", process.env.GOOGLE_GENAI_USE_VERTEXAI);
      console.log("PROJECT:", process.env.GOOGLE_CLOUD_PROJECT);
  
      system.spawn("seag://system/credentials", CredentialProviderActor);
      await new Promise(resolve => setTimeout(resolve, 50)); // Allow it to start
  
      system.spawn("seag://system/inference", VertexInferenceActor);
      await new Promise(resolve => setTimeout(resolve, 50)); // Wait for onStart
      let response: string | null = null;
    class MockUser extends Actor {
      async receive(msg: Message) {
        if (msg.type === "RESPONSE") response = msg.payload.text;
        if (msg.type === "ERROR") console.error("API Error:", msg.payload);
      }
    }
    system.spawn("seag://local/user", MockUser);

    system.send("seag://system/inference", {
      type: "PROMPT",
      sender: "seag://local/user",
      payload: { 
        text: "Say 'Hello from Gemini'",
        params: { model: "gemini-2.0-flash-exp" }
      }
    });

    // Wait longer for real network call
    await new Promise(resolve => setTimeout(resolve, 15000));
    
    expect(response).not.toBeNull();
    expect(response?.toLowerCase()).toContain("hello");
  }, 20000);

});
