#!/usr/bin/env bun

/**
 * Post-Compaction Hook Handler
 *
 * Triggered after conversation compaction to optionally resume
 * agent scheduling based on auto-mode configuration.
 *
 * Usage:
 *   bun src/hooks/post-compaction.ts
 *
 * Environment:
 *   CLAUDE_PROJECT_DIR - Project root (optional, defaults to cwd)
 */

import { loadConfig, saveConfig } from "../../daemon/config.ts";
import type { TaskProperties, Edge, NodeProperties } from "../types.ts";
import { existsSync, readFileSync } from "fs";
import { resolve } from "path";

const TASKS_FILE = "tasks.json";

// File format - matches Graph.dump() structure
interface TaskFile {
  nodes: NodeProperties[];
  edges: Edge[];
}

/**
 * Query ready tasks from task file data
 * Filters by status=created, priority, and excludes specific labels
 */
function queryReadyTasks(
  taskFile: TaskFile,
  options: {
    minPriority: number;
    excludeLabels?: string[];
    limit?: number;
  }
): Array<{ id: string; props: TaskProperties }> {
  // Build task map
  const taskMap = new Map<string, TaskProperties>();
  for (const node of taskFile.nodes) {
    if (node.type === "task") {
      const taskProps = node as TaskProperties;
      taskMap.set(taskProps.id, taskProps);
    }
  }

  const readyTasks: Array<{ id: string; props: TaskProperties }> = [];

  for (const [taskId, props] of taskMap.entries()) {
    // Filter: status must be "created"
    if (props.state !== "created") {
      continue;
    }

    // Filter: priority must be >= minPriority (lower number = higher priority)
    if (props.priority === undefined || props.priority > options.minPriority) {
      continue;
    }

    // Filter: exclude specific labels
    if (options.excludeLabels && props.labels) {
      const hasExcludedLabel = props.labels.some((label) =>
        options.excludeLabels!.includes(label)
      );
      if (hasExcludedLabel) {
        continue;
      }
    }

    // Check dependencies - task is blocked if any dependency is not completed
    const dependencies = taskFile.edges.filter(
      (e) => e.fromId === taskId && e.type === "depends_on"
    );
    let isBlocked = false;

    for (const dep of dependencies) {
      const depProps = taskMap.get(dep.toId);
      if (depProps && depProps.state !== "completed") {
        isBlocked = true;
        break;
      }
    }

    if (!isBlocked) {
      readyTasks.push({ id: taskId, props });
    }
  }

  // Sort by priority (lower number = higher priority)
  readyTasks.sort((a, b) => {
    if (a.props.priority !== b.props.priority) {
      return (a.props.priority || 4) - (b.props.priority || 4);
    }
    // Secondary sort by createdAt
    const dateA = new Date(a.props.createdAt).getTime();
    const dateB = new Date(b.props.createdAt).getTime();
    return dateA - dateB;
  });

  // Apply limit
  if (options.limit && options.limit > 0) {
    return readyTasks.slice(0, options.limit);
  }

  return readyTasks;
}

/**
 * Get count of currently active agents
 * Counts tasks with state="active" and label="agent"
 */
function getCurrentAgentCount(taskFile: TaskFile): number {
  let count = 0;
  for (const node of taskFile.nodes) {
    if (node.type === "task") {
      const props = node as TaskProperties;
      if (props.state === "active" && props.labels?.includes("agent")) {
        count++;
      }
    }
  }

  return count;
}

/**
 * Format task list for display
 */
function formatTaskList(tasks: Array<{ id: string; props: TaskProperties }>): string {
  if (tasks.length === 0) {
    return "  (no tasks available)";
  }

  return tasks
    .map(({ id, props }) => {
      const priority = `P${props.priority ?? "?"}`;
      const goalPreview =
        props.goal.length > 60 ? props.goal.slice(0, 57) + "..." : props.goal;
      return `  ${priority}  ${id}  ${goalPreview}`;
    })
    .join("\n");
}

/**
 * Load task file data
 */
function loadTaskFile(filePath: string): TaskFile {
  if (!existsSync(filePath)) {
    throw new Error(`Task file not found: ${filePath}`);
  }

  const content = readFileSync(filePath, "utf-8");
  return JSON.parse(content);
}

/**
 * Main post-compaction handler
 */
export async function handlePostCompaction(): Promise<void> {
  const config = loadConfig();

  // Update last compaction timestamp
  config.agentScheduling.lastCompaction = new Date().toISOString();
  saveConfig(config);

  console.log("\n" + "=".repeat(80));
  console.log("POST-COMPACTION HOOK TRIGGERED");
  console.log("=".repeat(80) + "\n");

  // Check auto-mode status
  if (!config.agentScheduling.autoMode) {
    console.log("Auto-mode: DISABLED");
    console.log("\nPost-compaction agent scheduling is disabled.");
    console.log("Enable with: bun src/cli/auto.ts on");
    console.log("\nManual options:");
    console.log("  - List ready work: bun src/cli/task.ts ready");
    console.log("  - Launch agent: /bg <task-description>");
    console.log("\n" + "=".repeat(80) + "\n");
    return;
  }

  console.log("Auto-mode: ENABLED");
  console.log("Checking for work to schedule...\n");

  // Check if tasks.json exists
  const projectDir = process.env.CLAUDE_PROJECT_DIR || process.cwd();
  const tasksFile = resolve(projectDir, TASKS_FILE);

  if (!existsSync(tasksFile)) {
    console.log("No tasks.json found - nothing to schedule");
    console.log("Initialize with: bun src/cli/task.ts init");
    console.log("\n" + "=".repeat(80) + "\n");
    return;
  }

  // Load task file
  let taskFile: TaskFile;
  try {
    taskFile = loadTaskFile(tasksFile);
  } catch (error) {
    console.error("Failed to load tasks.json:", error);
    console.log("\n" + "=".repeat(80) + "\n");
    return;
  }

  // Get current agent count
  const currentAgents = getCurrentAgentCount(taskFile);
  const targetCapacity = config.agentScheduling.targetCapacity;
  const availableSlots = targetCapacity - currentAgents;

  console.log(`Current agents: ${currentAgents}/${targetCapacity}`);
  console.log(`Available slots: ${availableSlots}\n`);

  if (availableSlots <= 0) {
    console.log("Capacity full - no new agents will be launched");
    console.log("\n" + "=".repeat(80) + "\n");
    return;
  }

  // Query for ready work
  const readyTasks = queryReadyTasks(taskFile, {
    minPriority: config.agentScheduling.minPriority,
    excludeLabels: ["test", "review-pending-human"],
    limit: availableSlots,
  });

  console.log(`Ready tasks found (P0-P${config.agentScheduling.minPriority}): ${readyTasks.length}\n`);

  if (readyTasks.length === 0) {
    console.log("No ready tasks to schedule");
    console.log("Check status: bun src/cli/task.ts list --status created");
    console.log("\n" + "=".repeat(80) + "\n");
    return;
  }

  // Display tasks that would be launched
  console.log("Tasks ready for launch:");
  console.log(formatTaskList(readyTasks));
  console.log();

  // NOTE: Actual agent launching is NOT implemented in this hook
  // Agent launching requires integration with Claude Code's background agent system
  // which is beyond the scope of this hook.
  //
  // For now, we display what WOULD be launched if auto-mode were fully integrated.
  // User can manually launch agents with /bg <task-description>

  console.log("NOTE: Auto-mode agent launching not yet implemented");
  console.log("Manual launch: Use /bg <task-description> for each task above");
  console.log("\nTo disable auto-mode: bun src/cli/auto.ts off");
  console.log("\n" + "=".repeat(80) + "\n");
}

// Export for use in post-compaction hooks or manual testing
if (import.meta.main) {
  await handlePostCompaction();
}
