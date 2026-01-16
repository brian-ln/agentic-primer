#!/usr/bin/env bun

/**
 * Task CLI - Manage task graphs using the Graph/TaskNode system
 *
 * Commands:
 *   task init                              Create tasks.json
 *   task add <goal> [options]              Add a task
 *   task update <id> <action> [args]       Update task (start, complete, block)
 *   task delete <id> [--force]             Delete a task
 *   task list [--status] [--label] [--priority]  List tasks with optional filters
 *   task ready                             Show tasks with no blockers
 *   task show <id>                         Show task details
 *   task graph <id>                        Show dependency tree
 *   task eval <id>                         Evaluate task criteria
 *   task status <id>                       Show task status with blockers
 *   task search <query>                    Search tasks by keyword
 */

import { Graph } from "../graph.ts";
import { TaskActor, type CreateTaskOptions, getTaskProperties } from "../task.ts";
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

  // Recreate nodes using TaskActor factory
  for (const nodeProps of taskFile.nodes) {
    if (nodeProps.type === "task") {
      const taskProps = nodeProps as TaskProperties;

      // Create task actor using factory
      TaskActor({
        goal: taskProps.goal,
        desiredDeliverables: taskProps.desiredDeliverables,
        objectiveSuccessCriteria: taskProps.objectiveSuccessCriteria,
        subjectiveSuccessCriteria: taskProps.subjectiveSuccessCriteria,
        informationGaps: taskProps.informationGaps,
        toolsAvailable: taskProps.toolsAvailable,
        parentTaskId: taskProps.parentTaskId,
        labels: taskProps.labels,
        priority: taskProps.priority,
        graph,
      });

      // Restore task state by updating properties in graph
      const restoredProps = graph.getNodeProperties(taskProps.id);
      if (restoredProps) {
        Object.assign(restoredProps, taskProps);
      }
    }
  }

  // Recreate edges
  for (const edge of taskFile.edges) {
    // Add edge directly to graph's internal edges map
    const edgeNum = parseInt(edge.id.replace("edge_", ""));
    if (!isNaN(edgeNum)) {
      graph.setEdgeCounter(Math.max(edgeNum, 0));
    }
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
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

  // Create an example task using TaskActor
  TaskActor({
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
    graph,
  });

  // Get the task ID from graph
  const taskId = graph.getNodeIds()[0];

  await saveGraph(graph, filePath);
  console.log(`Created ${TASKS_FILE} with example task`);
  console.log(`Task ID: ${taskId}`);
}

async function cmdList(filters?: { status?: string; label?: string; priority?: number }) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);
  const nodeIds = graph.getNodeIds();

  let tasks = nodeIds.filter((id) => {
    const props = graph.getNodeProperties(id);
    return props?.type === "task";
  });

  // Apply filters (AND logic - all filters must match)
  if (filters) {
    tasks = tasks.filter((id) => {
      const props = graph.getNodeProperties(id) as TaskProperties;

      // Filter by status
      if (filters.status && props.state !== filters.status) {
        return false;
      }

      // Filter by label (match any label)
      if (filters.label) {
        if (!props.labels || !props.labels.some(l => l.toLowerCase() === filters.label!.toLowerCase())) {
          return false;
        }
      }

      // Filter by priority
      if (filters.priority !== undefined && props.priority !== filters.priority) {
        return false;
      }

      return true;
    });
  }

  console.log("\nTasks:");
  console.log("─".repeat(80));

  if (tasks.length === 0) {
    console.log("No tasks match the specified filters.");
  } else {
    for (const id of tasks) {
      const props = graph.getNodeProperties(id) as TaskProperties;
      const statusEmoji = {
        created: "⭕",
        ready: "🟡",
        active: "🔄",
        blocked: "🚫",
        completed: "✅",
        failed: "❌",
      }[props.state] || "❓";

      const priorityDisplay = props.priority !== undefined ? `P${props.priority}` : "  ";
      const labelsDisplay = props.labels && props.labels.length > 0 ? `[${props.labels.join(", ")}]` : "";
      const goalPreview = props.goal.length > 50 ? props.goal.slice(0, 47) + "..." : props.goal;
      console.log(`${statusEmoji} ${priorityDisplay} ${id.padEnd(15)} ${props.state.padEnd(10)} ${goalPreview} ${labelsDisplay}`);
    }
  }
  console.log();
}

async function cmdShow(id: string) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  const result = await graph.send(id, "get", {});
  const getResponse = result as { id: string; properties: TaskProperties; edges: Edge[] };

  const props = getResponse.properties;

  console.log(`\nTask: ${id}`);
  console.log("─".repeat(80));
  console.log(`Goal:           ${props.goal}`);
  console.log(`State:          ${props.state}`);
  if (props.priority !== undefined) console.log(`Priority:       P${props.priority}`);
  if (props.labels && props.labels.length > 0) console.log(`Labels:         ${props.labels.join(", ")}`);
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

async function cmdAdd(goal: string, options: { deliverables?: string[]; criteria?: string; depends?: string; parent?: string; labels?: string[]; priority?: 0 | 1 | 2 | 3 | 4 }) {
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
    labels: options.labels,
    priority: options.priority,
  };

  // Create task using TaskActor factory
  TaskActor({ ...createOptions, graph });

  // Get the new task ID (last one added)
  const allIds = graph.getNodeIds();
  const taskId = allIds[allIds.length - 1];

  // Add dependency edges
  if (options.depends) {
    const deps = options.depends.split(",");
    for (const depId of deps) {
      graph.addEdge(taskId, depId.trim(), "depends_on");
    }
  }

  // Add parent edge
  if (options.parent) {
    graph.addEdge(taskId, options.parent, "spawned_by");
  }

  await saveGraph(graph, filePath);
  console.log(`Added task: ${taskId}`);
  console.log(`Goal: ${goal}`);
  if (options.labels && options.labels.length > 0) console.log(`Labels: ${options.labels.join(", ")}`);
  if (options.priority !== undefined) console.log(`Priority: P${options.priority}`);
}

async function cmdUpdate(id: string, action: string, ...args: string[]) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  let result: unknown;

  switch (action) {
    case "start": {
      result = await graph.send(id, "start", {});
      console.log(`Started task ${id}:`, result);
      break;
    }

    case "complete": {
      // Get task to set actual values for criteria
      const getResponse = await graph.send(id, "get", {}) as { properties: TaskProperties };
      const props = getResponse.properties;

      // For now, auto-pass all criteria
      for (const criterion of props.objectiveSuccessCriteria) {
        criterion.actual = criterion.threshold;
      }

      result = await graph.send(id, "complete", { result: "Task completed" });
      console.log(`Completed task ${id}:`, result);
      break;
    }

    case "block": {
      const reason = args[0] || "Blocked by dependency";
      result = await graph.send(id, "block", { reason });
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

  const result = await graph.send(id, "eval", {});
  console.log(`\nEvaluation for task ${id}:`);
  console.log(JSON.stringify(result, null, 2));
}

async function cmdStatus(id: string) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  const result = await graph.send(id, "query_status", {});
  console.log(`\nStatus for task ${id}:`);
  console.log(JSON.stringify(result, null, 2));
}

async function cmdSearch(query: string) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);
  const nodeIds = graph.getNodeIds();

  // Filter for task nodes only
  const tasks = nodeIds.filter((id) => {
    const props = graph.getNodeProperties(id);
    return props?.type === "task";
  });

  // Normalize query for case-insensitive matching
  const normalizedQuery = query.toLowerCase();

  // Search for matching tasks
  const matches: { id: string; props: TaskProperties; matchedIn: string[] }[] = [];

  for (const id of tasks) {
    const props = graph.getNodeProperties(id) as TaskProperties;
    const matchedIn: string[] = [];

    // Search in goal
    if (props.goal.toLowerCase().includes(normalizedQuery)) {
      matchedIn.push("goal");
    }

    // Search in deliverables
    for (const deliverable of props.desiredDeliverables) {
      if (deliverable.toLowerCase().includes(normalizedQuery)) {
        matchedIn.push("deliverables");
        break; // Only count deliverables once
      }
    }

    // Search in objective success criteria
    for (const criterion of props.objectiveSuccessCriteria) {
      if (criterion.criterion.toLowerCase().includes(normalizedQuery)) {
        matchedIn.push("criteria");
        break; // Only count criteria once
      }
    }

    // If any matches found, add to results
    if (matchedIn.length > 0) {
      matches.push({ id, props, matchedIn });
    }
  }

  // Display results
  if (matches.length === 0) {
    console.log(`\nNo tasks found matching "${query}"`);
    console.log();
    return;
  }

  console.log(`\nFound ${matches.length} task(s) matching "${query}":`);
  console.log("─".repeat(80));

  for (const match of matches) {
    const statusEmoji = {
      created: "⭕",
      ready: "🟡",
      active: "🔄",
      blocked: "🚫",
      completed: "✅",
      failed: "❌",
    }[match.props.state] || "❓";

    const goalPreview = match.props.goal.length > 50 ? match.props.goal.slice(0, 47) + "..." : match.props.goal;
    const matchInfo = `[${match.matchedIn.join(", ")}]`;
    console.log(`${statusEmoji} ${match.id.padEnd(15)} ${match.props.state.padEnd(10)} ${goalPreview}`);
    console.log(`   ${matchInfo}`);
  }
  console.log();
}

async function cmdDelete(id: string, force: boolean = false) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  // Check if task exists
  const props = graph.getNodeProperties(id);
  if (!props) {
    console.error(`Error: Task not found: ${id}`);
    process.exit(1);
  }

  // Get connected edges for confirmation message
  const edges = graph.getAllEdges(id);

  // Show what will be deleted (unless forced)
  if (!force) {
    console.log(`\nTask to delete: ${id}`);
    console.log("─".repeat(80));
    console.log(`Goal: ${(props as TaskProperties).goal}`);
    console.log(`State: ${(props as TaskProperties).state}`);

    if (edges.length > 0) {
      console.log(`\nConnected edges (${edges.length}):`);
      edges.forEach((e) => {
        console.log(`  ${e.type}: ${e.fromId} → ${e.toId}`);
      });
    }

    // Confirmation prompt
    console.log();
    const answer = prompt("Delete this task? (yes/no): ");
    if (answer?.toLowerCase() !== "yes") {
      console.log("Deletion cancelled.");
      process.exit(0);
    }
  }

  // Delete the task (this also removes all connected edges)
  const removed = graph.removeNode(id);

  if (removed) {
    await saveGraph(graph, filePath);
    console.log(`\nDeleted task ${id}`);
    if (edges.length > 0) {
      console.log(`Removed ${edges.length} connected edge(s)`);
    }
  } else {
    console.error(`Error: Failed to delete task ${id}`);
    process.exit(1);
  }
}

async function cmdReady() {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);
  const nodeIds = graph.getNodeIds();

  // Filter to task nodes only
  const taskIds = nodeIds.filter((id) => {
    const props = graph.getNodeProperties(id);
    return props?.type === "task";
  });

  // Find ready tasks (no blockers)
  const readyTasks: Array<{ id: string; props: TaskProperties }> = [];

  for (const taskId of taskIds) {
    const props = graph.getNodeProperties(taskId) as TaskProperties;

    // Skip completed or failed tasks
    if (props.state === "completed" || props.state === "failed") {
      continue;
    }

    // Skip explicitly blocked tasks
    if (props.state === "blocked") {
      continue;
    }

    // Check dependencies - task is blocked if any dependency is not completed
    const dependencies = graph.getEdgesFrom(taskId).filter((e) => e.type === "depends_on");
    let isBlocked = false;

    for (const dep of dependencies) {
      const depProps = graph.getNodeProperties(dep.toId) as TaskProperties;
      if (depProps && depProps.state !== "completed") {
        isBlocked = true;
        break;
      }
    }

    if (!isBlocked) {
      readyTasks.push({ id: taskId, props });
    }
  }

  // Sort by priority (if available) then by createdAt
  readyTasks.sort((a, b) => {
    // Check if priority field exists (task_3 may add this)
    const priorityA = (a.props as any).priority;
    const priorityB = (b.props as any).priority;

    if (priorityA !== undefined && priorityB !== undefined) {
      // Lower priority number = higher priority (P0 > P1 > P2)
      return priorityA - priorityB;
    }

    // Fall back to createdAt (older tasks first)
    const dateA = new Date(a.props.createdAt).getTime();
    const dateB = new Date(b.props.createdAt).getTime();
    return dateA - dateB;
  });

  // Display results
  console.log("\nReady Tasks (no blockers):");
  console.log("─".repeat(80));

  if (readyTasks.length === 0) {
    console.log("No tasks ready to work on.");
  } else {
    for (const { id, props } of readyTasks) {
      const statusEmoji = {
        created: "⭕",
        ready: "🟡",
        active: "🔄",
        blocked: "🚫",
        completed: "✅",
        failed: "❌",
      }[props.state] || "❓";

      const priority = (props as any).priority !== undefined ? `[P${(props as any).priority}]` : "";
      const goalPreview = props.goal.length > 50 ? props.goal.slice(0, 47) + "..." : props.goal;
      console.log(`${statusEmoji} ${id.padEnd(15)} ${priority.padEnd(5)} ${goalPreview}`);
    }
  }
  console.log();
}


async function cmdGraph(id: string) {
  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  // Check if task exists
  const props = graph.getNodeProperties(id) as TaskProperties;
  if (!props || props.type !== "task") {
    throw new Error(`Task not found: ${id}`);
  }

  console.log(`\nDependency graph for ${id}:`);
  console.log("─".repeat(80));

  // Track visited nodes to detect cycles
  const visited = new Set<string>();

  function printTree(taskId: string, indent: number = 0, prefix: string = "", isLast: boolean = true): void {
    // Detect cycles
    if (visited.has(taskId)) {
      console.log(`${prefix}⚠️  (circular dependency detected)`);
      return;
    }
    visited.add(taskId);

    // Get task properties
    const taskProps = graph.getNodeProperties(taskId) as TaskProperties;
    if (!taskProps) {
      console.log(`${prefix}❓ ${taskId} (not found)`);
      return;
    }

    // Get status emoji
    const statusEmoji = {
      created: "⭕",
      ready: "🟡",
      active: "🔄",
      blocked: "🚫",
      completed: "✅",
      failed: "❌",
    }[taskProps.state] || "❓";

    // Print current task
    const goalPreview = taskProps.goal.length > 60 ? taskProps.goal.slice(0, 57) + "..." : taskProps.goal;
    console.log(`${prefix}${statusEmoji} ${taskId}: ${goalPreview}`);

    // Get dependencies (edges FROM this task TO others with type "depends_on")
    const dependencyEdges = graph.getEdgesFrom(taskId).filter(e => e.type === "depends_on");

    if (dependencyEdges.length > 0) {
      // Sort by task ID for consistent output
      const sortedDeps = dependencyEdges.sort((a, b) => a.toId.localeCompare(b.toId));

      sortedDeps.forEach((edge, index) => {
        const isLastDep = index === sortedDeps.length - 1;
        const connector = isLastDep ? "└──" : "├──";
        const nextPrefix = prefix + (isLast ? "    " : "│   ");

        console.log(`${prefix}${connector} depends_on:`);
        printTree(edge.toId, indent + 1, nextPrefix + "    ", isLastDep);
      });
    }

    visited.delete(taskId); // Allow revisiting in other branches
  }

  printTree(id);
  console.log();
}


// Main CLI

async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  switch (command) {
    case "init":
      await cmdInit();
      break;

    case "list": {
      const filters: { status?: string; label?: string; priority?: number } = {};

      for (let i = 1; i < args.length; i++) {
        if (args[i] === "--status" && args[i + 1]) {
          filters.status = args[i + 1];
          i++;
        } else if (args[i] === "--label" && args[i + 1]) {
          filters.label = args[i + 1];
          i++;
        } else if (args[i] === "--priority" && args[i + 1]) {
          const priorityStr = args[i + 1].toUpperCase();
          const priorityMatch = priorityStr.match(/^P?([0-4])$/);
          if (priorityMatch) {
            filters.priority = parseInt(priorityMatch[1]);
          } else {
            console.error(`Error: Invalid priority "${args[i + 1]}". Use P0-P4 or 0-4 (where 0 is highest).`);
            process.exit(1);
          }
          i++;
        }
      }

      await cmdList(Object.keys(filters).length > 0 ? filters : undefined);
      break;
    }

    case "show":
      if (args.length < 2) {
        console.error("Usage: task show <id>");
      console.log("  task graph <id>                        Show dependency tree for a task");
        process.exit(1);
      }
      await cmdShow(args[1]);
      break;

    case "add": {
      if (args.length < 2) {
        console.error("Usage: task add <goal> [--deliverables d1,d2] [--criteria name:measure:threshold] [--depends id1,id2] [--parent id] [--labels tag1,tag2] [--priority P0]");
        process.exit(1);
      }

      const goal = args[1];
      const options: { deliverables?: string[]; criteria?: string; depends?: string; parent?: string; labels?: string[]; priority?: 0 | 1 | 2 | 3 | 4 } = {};

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
        } else if (args[i] === "--labels" && args[i + 1]) {
          options.labels = args[i + 1].split(",").map(l => l.trim());
          i++;
        } else if (args[i] === "--priority" && args[i + 1]) {
          const priorityStr = args[i + 1].toUpperCase();
          const priorityMatch = priorityStr.match(/^P?([0-4])$/);
          if (priorityMatch) {
            options.priority = parseInt(priorityMatch[1]) as 0 | 1 | 2 | 3 | 4;
          } else {
            console.error(`Error: Invalid priority "${args[i + 1]}". Use P0-P4 or 0-4 (where 0 is highest).`);
            process.exit(1);
          }
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

    case "search":
      if (args.length < 2) {
        console.error("Usage: task search <query>");
        process.exit(1);
      }
      await cmdSearch(args[1]);
      break;

    case "delete": {
      if (args.length < 2) {
        console.error("Usage: task delete <id> [--force]");
        process.exit(1);
      }

      const taskId = args[1];
      const force = args.includes("--force");

      await cmdDelete(taskId, force);
      break;
    }

    case "ready":
      await cmdReady();
      break;

    case "graph":
      if (args.length < 2) {
        console.error("Usage: task graph <id>");
        process.exit(1);
      }
      await cmdGraph(args[1]);
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
      console.log("    --labels tag1,tag2                   Labels (comma-separated)");
      console.log("    --priority P0|P1|P2|P3|P4            Priority (P0 is highest)");
      console.log("  task update <id> <action>              Update task");
      console.log("    start                                Start the task");
      console.log("    complete                             Complete the task");
      console.log("    block <reason>                       Block the task");
      console.log("  task delete <id> [--force]             Delete a task and its edges");
      console.log("  task list [options]                    List all tasks");
      console.log("    --status <state>                     Filter by state (created, active, blocked, completed)");
      console.log("    --label <label>                      Filter by label (match any)");
      console.log("    --priority P0|P1|P2|P3|P4            Filter by priority");
      console.log("  task ready                             Show tasks with no blockers");
      console.log("  task show <id>                         Show task details");
      console.log("  task graph <id>                        Show dependency tree for a task");
      console.log("  task eval <id>                         Evaluate task criteria");
      console.log("  task status <id>                       Show task status with blockers");
      console.log("  task search <query>                    Search tasks by keyword");
      process.exit(command ? 1 : 0);
  }
}

main().catch((err) => {
  console.error("Error:", err.message);
  process.exit(1);
});
