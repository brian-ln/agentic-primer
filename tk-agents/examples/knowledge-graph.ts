/**
 * Knowledge Graph Actor System Example
 *
 * Demonstrates:
 * - Actors managing interconnected data (knowledge nodes)
 * - Query propagation through graph structure
 * - Actor-to-actor communication for graph traversal
 */

import { createSystem } from "../src/actors/index.ts";
import type { ActorFactory, Message } from "../src/actors/index.ts";

// Knowledge node data
interface KnowledgeNodeData {
  id: string;
  content: string;
  links: string[]; // IDs of related nodes
  visits: number;
}

// KnowledgeNode actor - manages a node in the knowledge graph
const KnowledgeNodeActor: ActorFactory<KnowledgeNodeData> = (data, send) => {
  return {
    send: async (message: Message) => {
      switch (message.type) {
        case "query": {
          const { searchTerm, visited = [] } = message.payload as {
            searchTerm: string;
            visited?: string[];
          };

          data.visits++;

          // Avoid cycles
          if (visited.includes(data.id)) {
            return { success: true, data: [] };
          }

          const newVisited = [...visited, data.id];
          const results: Array<{ id: string; content: string }> = [];

          // Check if this node matches
          if (data.content.toLowerCase().includes(searchTerm.toLowerCase())) {
            results.push({ id: data.id, content: data.content });
            console.log(`[${data.id}] Match found: "${data.content}"`);
          }

          // Propagate query to linked nodes (using send from scope)
          for (const linkId of data.links) {
            const response = await send(linkId, {
              id: crypto.randomUUID(),
              type: "query",
              payload: { searchTerm, visited: newVisited },
            });

            if (response.success && response.data) {
              results.push(...(response.data as Array<{ id: string; content: string }>));
            }
          }

          return { success: true, data: results };
        }

        case "getInfo": {
          return {
            success: true,
            data: {
              id: data.id,
              content: data.content,
              links: data.links,
              visits: data.visits,
            },
          };
        }

        case "addLink": {
          const { targetId } = message.payload as { targetId: string };
          if (!data.links.includes(targetId)) {
            data.links.push(targetId);
            console.log(`[${data.id}] Added link to ${targetId}`);
          }
          return { success: true };
        }

        default:
          return {
            success: false,
            error: `Unknown message type: ${message.type}`,
          };
      }
    },
  };
};

async function main() {
  console.log("=== Knowledge Graph Actor System ===\n");

  const system = createSystem();

  // Build a knowledge graph about programming concepts
  const nodes = [
    {
      id: "actors",
      content: "Actors are concurrent entities that communicate via messages",
      links: ["concurrency", "messages"],
      visits: 0,
    },
    {
      id: "concurrency",
      content: "Concurrency enables multiple computations to progress simultaneously",
      links: ["actors", "parallelism"],
      visits: 0,
    },
    {
      id: "messages",
      content: "Messages are data structures sent between actors",
      links: ["actors", "data"],
      visits: 0,
    },
    {
      id: "parallelism",
      content: "Parallelism is the simultaneous execution of computations",
      links: ["concurrency"],
      visits: 0,
    },
    {
      id: "data",
      content: "Data structures organize and store information",
      links: ["messages"],
      visits: 0,
    },
    {
      id: "functions",
      content: "Functions are pure transformations from inputs to outputs",
      links: ["purity"],
      visits: 0,
    },
    {
      id: "purity",
      content: "Pure functions have no side effects and depend only on inputs",
      links: ["functions"],
      visits: 0,
    },
  ];

  // Create and register actors
  const actors = new Map();
  for (const nodeData of nodes) {
    const actor = KnowledgeNodeActor(nodeData, system.send);
    system.register(nodeData.id, actor);
    actors.set(nodeData.id, actor);
  }

  console.log(`Created knowledge graph with ${nodes.length} nodes\n`);

  // Query 1: Search for "actors"
  console.log('=== Query 1: Search for "actors" ===');
  const actorsActor = actors.get("actors")!;
  const result1 = await actorsActor.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "actors" },
  });
  console.log(`Found ${(result1.data as any[]).length} results\n`);

  // Query 2: Search for "concurrent"
  console.log('=== Query 2: Search for "concurrent" ===');
  const concurrencyActor = actors.get("concurrency")!;
  const result2 = await concurrencyActor.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "concurrent" },
  });
  console.log(`Found ${(result2.data as any[]).length} results\n`);

  // Query 3: Search for "pure" starting from different node
  console.log('=== Query 3: Search for "pure" from actors node ===');
  // This demonstrates graph traversal - starting from "actors" node
  // It won't reach "purity" because they're not connected
  const result3 = await actorsActor.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "pure" },
  });
  console.log(`Found ${(result3.data as any[]).length} results`);
  console.log("(No results - 'actors' and 'purity' are in separate subgraphs)\n");

  // Add connection between subgraphs
  console.log("=== Adding link between subgraphs ===");
  await actorsActor.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetId: "functions" },
  });
  console.log("Connected 'actors' -> 'functions'\n");

  // Query 4: Search again after connecting subgraphs
  console.log('=== Query 4: Search for "pure" again (after connection) ===');
  const result4 = await actorsActor.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "pure" },
  });
  console.log(`Found ${(result4.data as any[]).length} results`);
  const matches = result4.data as Array<{ id: string; content: string }>;
  for (const match of matches) {
    console.log(`  - ${match.id}: ${match.content}`);
  }

  // Show visit statistics
  console.log("\n=== Node Visit Statistics ===");
  for (const [id, actor] of actors) {
    const info = await actor.send({
      id: crypto.randomUUID(),
      type: "getInfo",
      payload: {},
    });
    const data = info.data as any;
    console.log(`${id}: ${data.visits} visits`);
  }

  console.log("\n=== Key Insights ===");
  console.log("1. Actors can form complex graph structures");
  console.log(
    "2. Message propagation enables distributed graph traversal"
  );
  console.log("3. Each node uses send() to reach neighbors without knowing routing");
  console.log("4. Graph topology can be modified dynamically via messages");
}

main().catch(console.error);
