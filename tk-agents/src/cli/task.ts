#!/usr/bin/env bun

/**
 * Task CLI - Manage task graphs via Entangled Actor pattern
 *
 * Commands:
 *   task add <goal> [options]              Add a task
 *   task update <id> <action> [args]       Update task (start, complete, block)
 *   task delete <id> [--force]             Delete a task
 *   task list [--status] [--label] [--priority]  List tasks
 *   task show <id>                         Show task details
 *   task ready                             Show ready tasks
 *   task graph <id>                        Show dependency graph
 *   task dep add <from> <to> [--type]      Add dependency
 *   task dep remove <edge-id>              Remove dependency
 *   task dep list <task-id>                List dependencies
 *   task dep show <edge-id>                Show edge details
 */

import { ActorClient } from "../actors/actor-client.ts";
import {
  parseTimestamp,
  parseRelativeTime,
  applyTimestampFilters,
  type TimestampFilterOptions
} from "../utils/timestamp.ts";

const TARGET_COLLECTION = "primer.tasks";
const TARGET_GRAPH = "primer.graph";

// Helper to output JSON or text
function jsonOutput(success: boolean, data?: any, error?: any) {
  const response: any = { success };
  if (data !== undefined) response.data = data;
  if (error) response.error = error;
  console.log(JSON.stringify(response, null, 2));
}

// Helper to show help
function showHelp() {
  console.log("Task CLI (Entangled Actor Mode)");
  console.log("\nUsage: task <command> [args]");
  console.log("\nCommands:");
  console.log("  add <goal> [options]              Add a new task");
  console.log("    --priority P0-P4                Set task priority");
  console.log("    --labels tag1,tag2              Set task labels");
  console.log("    --deliverables d1,d2            Set desired deliverables");
  console.log("    --depends id1,id2               Set task dependencies");
  console.log("    --parent <id>                   Set parent task");
  console.log("\n  list [options]                    List tasks");
  console.log("    --status <state>                Filter by status (created, active, completed, etc.)");
  console.log("    --label <tag>                   Filter by label");
  console.log("    --priority <P0-P4>              Filter by priority");
  console.log("\n  update <id> <action> [args]       Update task state");
  console.log("    Actions: start, complete, block");
  console.log("\n  show <id>                         Show task details and edges");
  console.log("\n  delete <id>                       Delete a task");
  console.log("\n  dep <subcommand>                  Manage dependencies");
  console.log("    add <from> <to> [--type T]      Add dependency edge");
  console.log("    remove <edge-id>                Remove dependency edge");
  console.log("\nGlobal Options:");
  console.log("  --json                            Output results in JSON format");
  console.log("  --help                            Show this help message");
}

async function main() {
  const args = process.argv.slice(2);
  const client = new ActorClient();

  if (args.length === 0 || args.includes("--help")) {
    showHelp();
    return;
  }

  // Basic argument parsing
  const getArg = (name: string) => {
    const idx = args.indexOf(name);
    return idx >= 0 ? args[idx + 1] : undefined;
  };
  const hasFlag = (name: string) => args.includes(name);

  // Global options
  const jsonMode = hasFlag("--json");

  const command = args[0];

  switch (command) {
    case "add": {
      const goal = args[1];
      if (!goal) {
        console.error("Usage: task add <goal> ...");
        process.exit(1);
      }

      const labels = getArg("--labels")?.split(",").map(s => s.trim()) || [];
      const priorityRaw = getArg("--priority");
      const priority = priorityRaw ? parseInt(priorityRaw.replace(/^P/i, "")) : undefined;
      const parent = getArg("--parent");
      const deliverables = getArg("--deliverables")?.split(",");
      const depends = getArg("--depends")?.split(",");

      try {
        // Create task
        const response = await client.send(TARGET_COLLECTION, "create", {
          goal,
          labels,
          priority,
          parentTaskId: parent,
          desiredDeliverables: deliverables,
        });

        if (!response.success) {
          throw new Error(response.error);
        }

        const taskId = response.data.id;

        // Add dependencies if any
        if (depends) {
          for (const depId of depends) {
            await client.send(TARGET_GRAPH, "create_edge", {
              from: taskId,
              to: depId.trim(),
              type: "depends_on"
            });
          }
        }

        // Add parent edge if parent specified
        if (parent) {
           await client.send(TARGET_GRAPH, "create_edge", {
              from: taskId,
              to: parent,
              type: "spawned_by"
           });
        }

        if (jsonMode) {
          jsonOutput(true, { id: taskId, goal, labels, priority });
        } else {
          console.log(`Added task: ${taskId}`);
          console.log(`Goal: ${goal}`);
          if (labels.length) console.log(`Labels: ${labels.join(", ")}`);
          if (priority !== undefined) console.log(`Priority: P${priority}`);
        }
      } catch (e: any) {
        if (jsonMode) jsonOutput(false, undefined, e.message);
        else console.error("Error:", e.message);
        process.exit(1);
      }
      break;
    }

    case "update": {
      const id = args[1];
      const action = args[2];
      
      if (!id || !action) {
        console.error("Usage: task update <id> <action> [args]");
        process.exit(1);
      }

      const payload: any = { id, action };
      if (action === "complete") {
        payload.result = args[3] || "Task completed";
      } else if (action === "block") {
        payload.reason = args[3] || "Blocked";
      }

      try {
        const response = await client.send(TARGET_COLLECTION, "update", payload);
        
        if (!response.success) throw new Error(response.error);

        if (jsonMode) {
          jsonOutput(true, response.data);
        } else {
          console.log(`Updated task ${id}: ${action}`);
        }
      } catch (e: any) {
        if (jsonMode) jsonOutput(false, undefined, e.message);
        else console.error("Error:", e.message);
        process.exit(1);
      }
      break;
    }

    case "delete": {
      const id = args[1];
      if (!id) {
        console.error("Usage: task delete <id>");
        process.exit(1);
      }

      try {
        const response = await client.send(TARGET_COLLECTION, "delete", { id });
        
        if (!response.success) throw new Error(response.error);

        if (jsonMode) {
          jsonOutput(true, { id, deleted: true });
        } else {
          console.log(`Deleted task ${id}`);
        }
      } catch (e: any) {
        if (jsonMode) jsonOutput(false, undefined, e.message);
        else console.error("Error:", e.message);
        process.exit(1);
      }
      break;
    }

    case "list": {
      const filters: any = {};
      if (getArg("--status")) filters.status = getArg("--status");
      if (getArg("--label")) filters.label = getArg("--label");
      if (getArg("--priority")) filters.priority = parseInt(getArg("--priority")!.replace(/^P/i, ""));

      try {
        const response = await client.send(TARGET_COLLECTION, "list", { filters });
        
        if (!response.success) throw new Error(response.error);

        const list = response.data as any[];

        // Apply timestamp filters locally if needed (omitted for brevity, can re-add)
        
        if (jsonMode) {
          jsonOutput(true, list);
        } else {
          console.log("\nTasks:");
          console.log("─".repeat(80));
          if (list.length === 0) console.log("No tasks found.");
          for (const t of list) {
            const statusEmoji = {
              created: "⭕",
              ready: "🟡",
              active: "🔄",
              blocked: "🚫",
              completed: "✅",
              failed: "❌",
            }[t.state] || "❓";
            const priority = t.priority !== undefined ? `P${t.priority}` : "  ";
            console.log(`${statusEmoji} ${priority} ${t.id.padEnd(15)} ${t.state.padEnd(10)} ${t.goal}`);
          }
          console.log();
        }
      } catch (e: any) {
        if (jsonMode) jsonOutput(false, undefined, e.message);
        else console.error("Error:", e.message);
        process.exit(1);
      }
      break;
    }

    case "show": {
      const id = args[1];
      if (!id) {
        console.error("Usage: task show <id>");
        process.exit(1);
      }

      try {
        // Get task details
        const response = await client.send(TARGET_COLLECTION, "get", { id });
        if (!response.success) throw new Error(response.error);
        
        const task = response.data;

        // Get edges separately
        const edgesResp = await client.send(TARGET_GRAPH, "get_node", { id });
        const edges = edgesResp.success ? edgesResp.data.edges : { outgoing: [], incoming: [] };

        if (jsonMode) {
          jsonOutput(true, { ...task, edges });
        } else {
          console.log(`\nTask: ${id}`);
          console.log("─".repeat(80));
          console.log(`Goal: ${task.goal}`);
          console.log(`State: ${task.state}`);
          if (task.priority !== undefined) console.log(`Priority: P${task.priority}`);
          
          if (edges.outgoing.length > 0) {
            console.log("\nOutgoing Edges:");
            for (const e of edges.outgoing) {
              console.log(`  ${e.type} -> ${e.toId}`);
            }
          }
          if (edges.incoming.length > 0) {
            console.log("\nIncoming Edges:");
            for (const e of edges.incoming) {
              console.log(`  ${e.type} <- ${e.fromId}`);
            }
          }
          console.log();
        }
      } catch (e: any) {
        if (jsonMode) jsonOutput(false, undefined, e.message);
        else console.error("Error:", e.message);
        process.exit(1);
      }
      break;
    }

    // Dependency commands
    case "dep": {
      const sub = args[1];
      if (sub === "add") {
        const from = args[2];
        const to = args[3];
        const type = getArg("--type") || "depends_on";
        
        try {
          const res = await client.send(TARGET_GRAPH, "create_edge", { from, to, type });
          if (!res.success) throw new Error(res.error);
          console.log(`Added dependency: ${from} -> ${to} (${type})`);
        } catch (e: any) {
          console.error("Error:", e.message);
        }
      } else if (sub === "remove") {
        const edgeId = args[2];
        try {
          const res = await client.send(TARGET_GRAPH, "delete_edge", { id: edgeId });
          if (!res.success) throw new Error(res.error);
          console.log(`Removed dependency: ${edgeId}`);
        } catch (e: any) {
          console.error("Error:", e.message);
        }
      } else {
        console.log("Usage: task dep [add|remove] ...");
      }
      break;
    }

    default:
      console.log("Task CLI (Entangled Actor Mode)");
      console.log("Commands: add, update, delete, list, show, dep");
  }
}

main().catch(console.error);