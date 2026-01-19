#!/usr/bin/env bun

/**
 * Graph CLI - Low-level graph manipulation via Entangled Actor pattern
 *
 * Commands:
 *   graph create-node <id> --type TYPE [--data JSON]
 *   graph delete-node <id>
 *   graph create-edge <from> <to> --type TYPE [--data JSON]
 *   graph delete-edge <edgeId>
 *   graph list-nodes [--type TYPE]
 *   graph list-edges [--from ID] [--to ID] [--type TYPE]
 *   graph show <id>
 *   graph show-graph
 *   graph dump
 */

import { ActorClient } from "../actors/actor-client.ts";

const TARGET_GRAPH = "primer.graph";

// Helper to show help
function showHelp() {
  console.log("Graph CLI (Entangled Actor Mode)");
  console.log("\nUsage: graph <command> [args]");
  console.log("\nCommands:");
  console.log("  create-node <id> --type T [--data JSON]   Create a new node");
  console.log("  delete-node <id>                          Delete a node and its edges");
  console.log("  create-edge <from> <to> --type T          Create an edge between nodes");
  console.log("  delete-edge <id>                          Delete an edge by ID");
  console.log("  list-nodes [--type T]                     List nodes in the graph");
  console.log("  list-edges [--from ID] [--to ID]          List edges in the graph");
  console.log("  show <id>                                 Show node details and edges");
  console.log("  show-graph                                Visualize the graph (ASCII)");
  console.log("  dump                                      Dump full graph JSON");
  console.log("\nGlobal Options:");
  console.log("  --json                                    Output results in JSON format");
  console.log("  --help                                    Show this help message");
}

async function main() {
  const args = process.argv.slice(2);
  const client = new ActorClient();

  if (args.length === 0 || args.includes("--help")) {
    showHelp();
    return;
  }

  const command = args[0];
  const jsonMode = args.includes("--json");

  // Basic argument parsing
  const getArg = (name: string) => {
    const idx = args.indexOf(name);
    return idx >= 0 ? args[idx + 1] : undefined;
  };

  try {
    switch (command) {
      case "create-node": {
        const id = args[1];
        const type = getArg("--type");
        const dataRaw = getArg("--data");
        const data = dataRaw ? JSON.parse(dataRaw) : {};
        
        const res = await client.send(TARGET_GRAPH, "create_node", { id, type, ...data });
        if (!res.success) throw new Error(res.error);
        console.log(`Created node: ${id} [${type}]`);
        break;
      }

      case "delete-node": {
        const id = args[1];
        const res = await client.send(TARGET_GRAPH, "delete_node", { id });
        if (!res.success) throw new Error(res.error);
        console.log(`Deleted node: ${id}`);
        break;
      }

      case "create-edge": {
        const from = args[1];
        const to = args[2];
        const type = getArg("--type");
        const res = await client.send(TARGET_GRAPH, "create_edge", { from, to, type });
        if (!res.success) throw new Error(res.error);
        console.log(`Created edge: ${res.data.id} (${from} -> ${to})`);
        break;
      }

      case "delete-edge": {
        const id = args[1];
        const res = await client.send(TARGET_GRAPH, "delete_edge", { id });
        if (!res.success) throw new Error(res.error);
        console.log(`Deleted edge: ${id}`);
        break;
      }

      case "list-nodes": {
        const type = getArg("--type");
        const res = await client.send(TARGET_GRAPH, "list_nodes", { type });
        if (!res.success) throw new Error(res.error);
        
        if (jsonMode) {
          console.log(JSON.stringify(res.data, null, 2));
        } else {
          console.log("\nNodes:");
          console.log("─".repeat(80));
          res.data.forEach((n: any) => console.log(`${n.id.padEnd(20)} ${n.type}`));
        }
        break;
      }

      case "list-edges": {
        const from = getArg("--from");
        const to = getArg("--to");
        const type = getArg("--type");
        const res = await client.send(TARGET_GRAPH, "list_edges", { from, to, type });
        if (!res.success) throw new Error(res.error);
        
        if (jsonMode) {
          console.log(JSON.stringify(res.data, null, 2));
        } else {
          console.log("\nEdges:");
          console.log("─".repeat(80));
          res.data.forEach((e: any) => console.log(`${e.id.padEnd(12)} ${e.fromId.padEnd(20)} -> ${e.toId.padEnd(20)} (${e.type})`));
        }
        break;
      }

      case "show": {
        const id = args[1];
        const res = await client.send(TARGET_GRAPH, "get_node", { id });
        if (!res.success) throw new Error(res.error);
        
        if (jsonMode) {
          console.log(JSON.stringify(res.data, null, 2));
        } else {
          const n = res.data;
          console.log(`\nNode: ${id}`);
          console.log("─".repeat(80));
          console.log(`Type: ${n.type}`);
          console.log(`Created: ${n.createdAt}`);
          if (n.edges.outgoing.length) {
            console.log("\nOutgoing Edges:");
            n.edges.outgoing.forEach((e: any) => console.log(`  ${e.type} -> ${e.toId}`));
          }
          if (n.edges.incoming.length) {
            console.log("\nIncoming Edges:");
            n.edges.incoming.forEach((e: any) => console.log(`  ${e.type} <- ${e.fromId}`));
          }
        }
        break;
      }

      case "dump": {
        const res = await client.send(TARGET_GRAPH, "dump");
        if (!res.success) throw new Error(res.error);
        console.log(JSON.stringify(res.data, null, 2));
        break;
      }

      default:
        console.error(`Unknown command: ${command}`);
        showHelp();
        process.exit(1);
    }
  } catch (e: any) {
    console.error("Error:", e.message);
    process.exit(1);
  }
}

main().catch(console.error);