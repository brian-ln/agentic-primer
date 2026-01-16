#!/usr/bin/env bun

/**
 * Task CLI - Manage task graphs using the Graph/TaskNode system
 *
 * Commands:
 *   task init                              Create tasks.json
 *   task add <goal> [options]              Add a task
 *   task update <id> <action> [args]       Update task (start, complete, block)
 *   task list                              List all tasks
 *   task show <id>                         Show task details
 *   task eval <id>                         Evaluate task criteria
 *   task status <id>                       Show task status with blockers
 */

import { Graph } from "../graph.ts";
import { TaskNode, type CreateTaskOptions } from "../task.ts";
import type { Edge, NodeProperties, ObjectiveCriterion, TaskProperties } from "../types.ts";
import { readFileSync, writeFileSync, existsSync } from "fs";
import { resolve } from "path";

const TASKS_FILE = "tasks.json";

// File format - matches Graph.dump() structure
interface TaskFile {
  nodes: NodeProperties[];
  edges: Edge[];
}

// Load tasks from file into Graph
async function loadGraph(filePath: string): Promise<Graph> {
  if (!existsSync(filePath)) {
    throw new Error(`Task file not found: ${filePath}`);
  }

  const content = readFileSync(filePath, "utf-8");
  const taskFile: TaskFile = JSON.parse(content);

  const graph = new Graph();

  // Recreate nodes
  for (const nodeProps of taskFile.nodes) {
    if (nodeProps.type === "task") {
      const taskProps = nodeProps as TaskProperties;

      // Reconstruct TaskNode from saved properties
      const task = new TaskNode({
        goal: taskProps.goal,
        desiredDeliverables: taskProps.desiredDeliverables,
        objectiveSuccessCriteria: taskProps.objectiveSuccessCriteria,
        subjectiveSuccessCriteria: taskProps.subjectiveSuccessCriteria,
        informationGaps: taskProps.informationGaps,
        toolsAvailable: taskProps.toolsAvailable,
        parentTaskId: taskProps.parentTaskId,
      });

      // Restore task state
      task.properties = { ...taskProps };

      graph.registerNode(task);
    }
  }

  // Recreate edges
  for (const edge of taskFile.edges) {
    (graph as any).edges.set(edge.id, edge);
    // Update edge counter to avoid ID conflicts
    const edgeNum = parseInt(edge.id.replace("edge_", ""));
    if (!isNaN(edgeNum) && edgeNum >= (graph as any).edgeCounter) {
      (graph as any).edgeCounter = edgeNum;
    }
  }

  return graph;
}

// Save Graph to file
async function saveGraph(graph: Graph, filePath: string): Promise<void> {
  const dump = graph.dump();

  // Serialize dates as ISO strings
  const serialized = JSON.stringify(dump, (key, value) => {
    if (value instanceof Date) {
      return value.toISOString();
    }
    return value;
  }, 2);

  writeFileSync(filePath, serialized, "utf-8");
}

// Commands

async function cmdInit() {
  const filePath = resolve(TASKS_FILE);

  if (existsSync(filePath)) {
    console.error(`Error: ${TASKS_FILE} already exists`);
    process.exit(1);
  }

  const graph = new Graph();

  // Create an example task
  const exampleTask = new TaskNode({
    goal: "Example task - getting started",
    desiredDeliverables: ["Understand the task system", "Run first commands"],
    objectiveSuccessCriteria: [
      {
        criterion: "Commands executed",
        measure: "Number of CLI commands run",
        threshold: 3,
      },
    ],
    toolsAvailable: ["CLI"],
  });

  graph.registerNode(exampleTask);

  await saveGraph(graph, filePath);
  console.log(`Created ${TASKS_FILE} with example task`);
  console.log(`Task ID: ${exampleTask.properties.id}`);
}

async function cmdList() {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);
  const nodes = graph.getAllNodes();

  const tasks = nodes.filter((n) => n.properties.type === "task");

  console.log("\nTasks:");
  console.log("─".repeat(80));

  for (const node of tasks) {
    const props = node.properties as TaskProperties;
    const statusEmoji = {
      created: "⭕",
      ready: "🟡",
      active: "🔄",
      blocked: "🚫",
      completed: "✅",
      failed: "❌",
    }[props.state] || "❓";

    const goalPreview = props.goal.length > 50 ? props.goal.slice(0, 47) + "..." : props.goal;
    console.log(`${statusEmoji} ${props.id.padEnd(15)} ${props.state.padEnd(10)} ${goalPreview}`);
  }
  console.log();
}

async function cmdShow(id: string) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  const result = graph.send(id, "get", {});
  const getResponse = result as { id: string; properties: TaskProperties; edges: Edge[] };

  const props = getResponse.properties;

  console.log(`\nTask: ${id}`);
  console.log("─".repeat(80));
  console.log(`Goal:           ${props.goal}`);
  console.log(`State:          ${props.state}`);
  console.log(`Created:        ${props.createdAt}`);
  if (props.startedAt) console.log(`Started:        ${props.startedAt}`);
  if (props.completedAt) console.log(`Completed:      ${props.completedAt}`);

  console.log(`\nDeliverables:`);
  props.desiredDeliverables.forEach((d) => console.log(`  - ${d}`));

  console.log(`\nSuccess Criteria:`);
  props.objectiveSuccessCriteria.forEach((c) => {
    const status = c.passed !== undefined ? (c.passed ? "✅" : "❌") : "⏳";
    console.log(`  ${status} ${c.criterion} (${c.measure} >= ${c.threshold})`);
  });

  if (getResponse.edges.length > 0) {
    console.log(`\nEdges:`);
    getResponse.edges.forEach((e) => {
      console.log(`  ${e.type}: ${e.fromId} → ${e.toId}`);
    });
  }

  if (props.informationGaps.length > 0) {
    console.log(`\nInformation Gaps:`);
    props.informationGaps.forEach((gap) => console.log(`  - ${gap}`));
  }

  console.log();
}

async function cmdAdd(goal: string, options: { deliverables?: string[]; criteria?: string; depends?: string; parent?: string }) {
  const filePath = resolve(TASKS_FILE);

  if (!existsSync(filePath)) {
    console.error(`Error: ${TASKS_FILE} not found. Run 'task init' first.`);
    process.exit(1);
  }

  const graph = await loadGraph(filePath);

  // Parse criteria if provided (format: "name:measure:threshold")
  const criteria: ObjectiveCriterion[] = [];
  if (options.criteria) {
    const parts = options.criteria.split(":");
    if (parts.length === 3) {
      criteria.push({
        criterion: parts[0],
        measure: parts[1],
        threshold: parseFloat(parts[2]) || parts[2] === "true",
      });
    }
  }

  const createOptions: CreateTaskOptions = {
    goal,
    desiredDeliverables: options.deliverables || ["Task completion"],
    objectiveSuccessCriteria: criteria.length > 0 ? criteria : [
      { criterion: "Task marked complete", measure: "Manual completion", threshold: true }
    ],
    parentTaskId: options.parent,
    toolsAvailable: ["CLI"],
  };

  const task = new TaskNode(createOptions);
  graph.registerNode(task);

  // Add dependency edges
  if (options.depends) {
    const deps = options.depends.split(",");
    for (const depId of deps) {
      graph.addEdge(task.properties.id, depId.trim(), "depends_on");
    }
  }

  // Add parent edge
  if (options.parent) {
    graph.addEdge(task.properties.id, options.parent, "spawned_by");
  }

  await saveGraph(graph, filePath);
  console.log(`Added task: ${task.properties.id}`);
  console.log(`Goal: ${goal}`);
}

async function cmdUpdate(id: string, action: string, ...args: string[]) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  let result: unknown;

  switch (action) {
    case "start": {
      result = graph.send(id, "start", {});
      console.log(`Started task ${id}:`, result);
      break;
    }

    case "complete": {
      // Get task to set actual values for criteria
      const getResponse = graph.send(id, "get", {}) as { properties: TaskProperties };
      const props = getResponse.properties;

      // For now, auto-pass all criteria
      for (const criterion of props.objectiveSuccessCriteria) {
        criterion.actual = criterion.threshold;
      }

      result = graph.send(id, "complete", { result: "Task completed" });
      console.log(`Completed task ${id}:`, result);
      break;
    }

    case "block": {
      const reason = args[0] || "Blocked by dependency";
      result = graph.send(id, "block", { reason });
      console.log(`Blocked task ${id}:`, result);
      break;
    }

    default:
      console.error(`Unknown action: ${action}`);
      console.error(`Valid actions: start, complete, block`);
      process.exit(1);
  }

  await saveGraph(graph, filePath);
}

async function cmdEval(id: string) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  const result = graph.send(id, "eval", {});
  console.log(`\nEvaluation for task ${id}:`);
  console.log(JSON.stringify(result, null, 2));
}

async function cmdStatus(id: string) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  const result = graph.send(id, "query_status", {});
  console.log(`\nStatus for task ${id}:`);
  console.log(JSON.stringify(result, null, 2));
}

// Main CLI

async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  switch (command) {
    case "init":
      await cmdInit();
      break;

    case "list":
      await cmdList();
      break;

    case "show":
      if (args.length < 2) {
        console.error("Usage: task show <id>");
        process.exit(1);
      }
      await cmdShow(args[1]);
      break;

    case "add": {
      if (args.length < 2) {
        console.error("Usage: task add <goal> [--deliverables d1,d2] [--criteria name:measure:threshold] [--depends id1,id2] [--parent id]");
        process.exit(1);
      }

      const goal = args[1];
      const options: { deliverables?: string[]; criteria?: string; depends?: string; parent?: string } = {};

      for (let i = 2; i < args.length; i++) {
        if (args[i] === "--deliverables" && args[i + 1]) {
          options.deliverables = args[i + 1].split(",");
          i++;
        } else if (args[i] === "--criteria" && args[i + 1]) {
          options.criteria = args[i + 1];
          i++;
        } else if (args[i] === "--depends" && args[i + 1]) {
          options.depends = args[i + 1];
          i++;
        } else if (args[i] === "--parent" && args[i + 1]) {
          options.parent = args[i + 1];
          i++;
        }
      }

      await cmdAdd(goal, options);
      break;
    }

    case "update":
      if (args.length < 3) {
        console.error("Usage: task update <id> <action> [args...]");
        console.error("Actions: start, complete, block");
        process.exit(1);
      }
      await cmdUpdate(args[1], args[2], ...args.slice(3));
      break;

    case "eval":
      if (args.length < 2) {
        console.error("Usage: task eval <id>");
        process.exit(1);
      }
      await cmdEval(args[1]);
      break;

    case "status":
      if (args.length < 2) {
        console.error("Usage: task status <id>");
        process.exit(1);
      }
      await cmdStatus(args[1]);
      break;

    default:
      console.log("Task CLI - Manage task graphs using the Graph/TaskNode system\n");
      console.log("Commands:");
      console.log("  task init                              Create tasks.json");
      console.log("  task add <goal> [options]              Add a task");
      console.log("    --deliverables d1,d2                 Deliverables (comma-separated)");
      console.log("    --criteria name:measure:threshold    Success criterion");
      console.log("    --depends id1,id2                    Dependencies (comma-separated)");
      console.log("    --parent id                          Parent task");
      console.log("  task update <id> <action>              Update task");
      console.log("    start                                Start the task");
      console.log("    complete                             Complete the task");
      console.log("    block <reason>                       Block the task");
      console.log("  task list                              List all tasks");
      console.log("  task show <id>                         Show task details");
      console.log("  task eval <id>                         Evaluate task criteria");
      console.log("  task status <id>                       Show task status with blockers");
      process.exit(command ? 1 : 0);
  }
}

main().catch((err) => {
  console.error("Error:", err.message);
  process.exit(1);
});
