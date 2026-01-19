#!/usr/bin/env bun

/**
 * Knowledge CLI - Manage knowledge nodes via Entangled Actor pattern
 *
 * Commands:
 *   knowledge add <title> [--content TEXT] [--sources S1,S2]
 *   knowledge list
 *   knowledge get <id>
 *   knowledge append <id> <content> [--source SRC]
 *   knowledge update <id> [--title TEXT] [--content TEXT]
 *   knowledge delete <id>
 *   knowledge query <id> <question>
 */

import { ActorClient } from "../actors/actor-client.ts";
import {
  parseTimestamp,
  parseRelativeTime,
  applyTimestampFilters,
  type TimestampFilterOptions
} from "../utils/timestamp.ts";

const TARGET_COLLECTION = "primer.knowledge";

// Helper to output JSON or text
function output(data: any, options: { json?: boolean } = {}) {
  if (options.json) {
    console.log(JSON.stringify(data, null, 2));
  } else {
    // Basic text formatting, can be improved
    console.log(JSON.stringify(data, null, 2)); 
  }
}

async function main() {
  const args = process.argv.slice(2);
  const command = args[0];
  const client = new ActorClient();

  // Basic argument parsing (can be replaced with a library later)
  const getArg = (name: string) => {
    const idx = args.indexOf(name);
    return idx >= 0 ? args[idx + 1] : undefined;
  };

  switch (command) {
    case "add": {
      const title = args[1];
      const content = getArg("--content") || "";
      const sources = getArg("--sources")?.split(",") || [];
      
      const response = await client.send(TARGET_COLLECTION, "create", {
        title, content, sources
      });
      
      if (!response.success) {
        console.error("Error:", response.error);
        process.exit(1);
      }
      console.log("Added knowledge:", response.data);
      break;
    }

    case "list": {
      const response = await client.send(TARGET_COLLECTION, "list");
      if (!response.success) {
        console.error("Error:", response.error);
        process.exit(1);
      }
      
      const list = response.data as any[];
      console.log("\nKnowledge Nodes:");
      console.log("─".repeat(80));
      for (const node of list) {
        console.log(`${node.id.padEnd(20)} ${node.title}`);
      }
      break;
    }
    
    // Fallback for other commands - route to specific node ID
    case "get":
    case "append":
    case "update":
    case "delete":
    case "query": {
      const id = args[1];
      if (!id) {
        console.error(`Usage: knowledge ${command} <id> ...`);
        process.exit(1);
      }
      
      // Map CLI args to message payload
      let payload: any = {};
      
      if (command === "append") {
        payload = { data: args[2], source: getArg("--source") };
      } else if (command === "query") {
        payload = { question: args[2] };
      } else if (command === "update") {
        payload = { 
          properties: {
            title: getArg("--title"),
            content: getArg("--content")
          }
        };
      }
      
      // Send directly to the node (entangled actor pattern)
      // The daemon gateway resolves the ID to the actor
      const response = await client.send(id, command, payload);
      
      if (!response.success) {
        console.error("Error:", response.error);
        process.exit(1);
      }
      
      console.log("Response:", JSON.stringify(response.data, null, 2));
      break;
    }

    default:
      console.log("Usage: knowledge <command> [args]");
      console.log("Commands: add, list, get, append, update, delete, query");
  }
}

main().catch(console.error);