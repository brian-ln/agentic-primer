/**
 * Knowledge Graph Actor System Example
 *
 * Demonstrates:
 * - Actors managing interconnected data (knowledge nodes)
 * - Query propagation through graph structure using Address references
 * - Actor-to-actor communication via ergonomic Address.send()
 * - Dynamic graph topology modification
 */

import { System } from "../src/actors/index.ts";
import type { ActorFactory, Address, Message, System as SystemType } from "../src/actors/index.ts";

// Knowledge node data
interface KnowledgeNodeData {
  id: string;
  content: string;
  links: Address[]; // Addresses of related nodes
  visits: number;
  system: SystemType;
}

// KnowledgeNode actor - manages a node in the knowledge graph
const KnowledgeNodeActor: ActorFactory<KnowledgeNodeData> = (data) => {
  const actor = {
    send: async (message: Message) => {
      switch (message.type) {
        case "query": {
          const { searchTerm, visited = [] } = message.payload as {
            searchTerm: string;
            visited?: symbol[]; // Track visited by Address.__id
          };

          data.visits++;

          // Avoid cycles - check if we've visited this address
          const myAddress = message.payload.__self as Address | undefined;
          if (visited.some(id => myAddress && id === myAddress.__id)) {
            return { success: true, data: [] };
          }

          const newVisited = myAddress ? [...visited, myAddress.__id] : visited;
          const results: Array<{ id: string; content: string }> = [];

          // Check if this node matches
          if (data.content.toLowerCase().includes(searchTerm.toLowerCase())) {
            results.push({ id: data.id, content: data.content });
            console.log(`[${data.id}] Match found: "${data.content}"`);
          }

          // Propagate query to linked nodes using ergonomic Address.send()
          for (const linkAddr of data.links) {
            const response = await linkAddr.send({
              id: crypto.randomUUID(),
              type: "query",
              payload: { searchTerm, visited: newVisited, __self: linkAddr },
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
              linkCount: data.links.length,
              visits: data.visits,
            },
          };
        }

        case "addLink": {
          const { targetAddr } = message.payload as { targetAddr: Address };
          // Check if link already exists
          if (!data.links.some(addr => addr.__id === targetAddr.__id)) {
            data.links.push(targetAddr);
            console.log(`[${data.id}] Added link to another node`);
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

  return data.system.register(actor);
};

async function main() {
  console.log("=== Knowledge Graph Actor System (Address Proxy Pattern) ===\n");

  const system = System();

  // Build a knowledge graph about programming concepts
  // We'll create addresses first, then link them

  console.log("Creating knowledge nodes...");

  const actorsNode = KnowledgeNodeActor({
    id: "actors",
    content: "Actors are concurrent entities that communicate via messages",
    links: [],
    visits: 0,
    system,
  });

  const concurrencyNode = KnowledgeNodeActor({
    id: "concurrency",
    content: "Concurrency enables multiple computations to progress simultaneously",
    links: [],
    visits: 0,
    system,
  });

  const messagesNode = KnowledgeNodeActor({
    id: "messages",
    content: "Messages are data structures sent between actors",
    links: [],
    visits: 0,
    system,
  });

  const parallelismNode = KnowledgeNodeActor({
    id: "parallelism",
    content: "Parallelism is the simultaneous execution of computations",
    links: [],
    visits: 0,
    system,
  });

  const dataNode = KnowledgeNodeActor({
    id: "data",
    content: "Data structures organize and store information",
    links: [],
    visits: 0,
    system,
  });

  const functionsNode = KnowledgeNodeActor({
    id: "functions",
    content: "Functions are pure transformations from inputs to outputs",
    links: [],
    visits: 0,
    system,
  });

  const purityNode = KnowledgeNodeActor({
    id: "purity",
    content: "Pure functions have no side effects and depend only on inputs",
    links: [],
    visits: 0,
    system,
  });

  // Now add links between nodes by sending addLink messages
  console.log("Building graph connections...");

  await actorsNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: concurrencyNode },
  });

  await actorsNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: messagesNode },
  });

  await concurrencyNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: actorsNode },
  });

  await concurrencyNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: parallelismNode },
  });

  await messagesNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: actorsNode },
  });

  await messagesNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: dataNode },
  });

  await parallelismNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: concurrencyNode },
  });

  await dataNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: messagesNode },
  });

  await functionsNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: purityNode },
  });

  await purityNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: functionsNode },
  });

  console.log("Knowledge graph created with 7 nodes\n");

  // Query 1: Search for "actors"
  console.log('=== Query 1: Search for "actors" ===');
  const result1 = await actorsNode.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "actors", __self: actorsNode },
  });
  console.log(`Found ${(result1.data as any[]).length} results\n`);

  // Query 2: Search for "concurrent"
  console.log('=== Query 2: Search for "concurrent" ===');
  const result2 = await concurrencyNode.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "concurrent", __self: concurrencyNode },
  });
  console.log(`Found ${(result2.data as any[]).length} results\n`);

  // Query 3: Search for "pure" starting from actors node
  console.log('=== Query 3: Search for "pure" from actors node ===');
  // This demonstrates graph traversal - starting from "actors" node
  // It won't reach "purity" because they're in separate subgraphs
  const result3 = await actorsNode.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "pure", __self: actorsNode },
  });
  console.log(`Found ${(result3.data as any[]).length} results`);
  console.log("(No results - 'actors' and 'purity' are in separate subgraphs)\n");

  // Add connection between subgraphs
  console.log("=== Adding link between subgraphs ===");
  await actorsNode.send({
    id: crypto.randomUUID(),
    type: "addLink",
    payload: { targetAddr: functionsNode },
  });
  console.log("Connected 'actors' -> 'functions'\n");

  // Query 4: Search again after connecting subgraphs
  console.log('=== Query 4: Search for "pure" again (after connection) ===');
  const result4 = await actorsNode.send({
    id: crypto.randomUUID(),
    type: "query",
    payload: { searchTerm: "pure", __self: actorsNode },
  });
  console.log(`Found ${(result4.data as any[]).length} results`);
  const matches = result4.data as Array<{ id: string; content: string }>;
  for (const match of matches) {
    console.log(`  - ${match.id}: ${match.content}`);
  }

  // Show visit statistics
  console.log("\n=== Node Visit Statistics ===");
  const nodes = [
    { name: "actors", addr: actorsNode },
    { name: "concurrency", addr: concurrencyNode },
    { name: "messages", addr: messagesNode },
    { name: "parallelism", addr: parallelismNode },
    { name: "data", addr: dataNode },
    { name: "functions", addr: functionsNode },
    { name: "purity", addr: purityNode },
  ];

  for (const { name, addr } of nodes) {
    const info = await addr.send({
      id: crypto.randomUUID(),
      type: "getInfo",
      payload: {},
    });
    const data = info.data as any;
    console.log(`${name}: ${data.visits} visits`);
  }

  console.log("\n=== Key Insights ===");
  console.log("1. Actors can form complex graph structures using Address references");
  console.log(
    "2. Message propagation enables distributed graph traversal"
  );
  console.log("3. Each node uses Address.send() to reach neighbors");
  console.log("4. Graph topology can be modified dynamically via messages");
  console.log("5. No magic strings - type-safe Address objects for all references");
}

main().catch(console.error);
