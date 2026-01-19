#!/usr/bin/env bun

/**
 * Session Orientation - Help Claude understand project state after startup/clear
 *
 * Queries tasks.json and presents actionable context:
 * - Blocked tasks (P0 stuck work)
 * - Ready work (P0-P1 unblocked)
 * - Active tasks (in progress)
 * - Recent completions (needs review)
 * - Project health snapshot
 * - Auto-mode status and capacity
 * - Suggested next actions
 *
 * Usage:
 *   bun src/cli/orient.ts [--quiet|--verbose]
 *   bun run orient
 */

import { readFileSync, existsSync } from "fs";
import { resolve, join } from "path";
import { homedir } from "os";
import {
  getCached,
  updateCache,
  clearCache,
  getCacheAge,
} from "../utils/orientation-cache";

const TASKS_FILE = "tasks.json";
const CONFIG_FILE = join(homedir(), ".primer-config.json");

// Verbosity levels
type VerbosityLevel = "quiet" | "normal" | "verbose";

export interface Task {
  id: string;
  goal: string;
  state: string;
  priority?: number;
  startedAt?: string;
  completedAt?: string;
  labels?: string[];
  createdAt?: string;
}

interface AutoModeConfig {
  enabled: boolean;
  targetCapacity: number;
  minPriority: number;
}

interface TaskFile {
  nodes: Task[];
  edges: any[];
}

interface OrientationData {
  blockers: Task[];
  ready: Task[];
  active: Task[];
  recentCompletions: Task[];
  reviews: Task[];
  stats: {
    total: number;
    completed: number;
    active: number;
    ready: number;
    blocked: number;
  };
  autoMode?: {
    enabled: boolean;
    targetCapacity: number;
    currentAgentCount: number;
    availableSlots: number;
  };
}

/**
 * Load auto-mode configuration from ~/.primer-config.json
 */
function loadAutoModeConfig(): AutoModeConfig | null {
  if (!existsSync(CONFIG_FILE)) {
    return null;
  }

  try {
    const content = readFileSync(CONFIG_FILE, "utf-8");
    const config = JSON.parse(content);

    if (!config.agentScheduling) {
      return null;
    }

    return {
      enabled: config.agentScheduling.autoMode ?? false,
      targetCapacity: config.agentScheduling.targetCapacity ?? 10,
      minPriority: config.agentScheduling.minPriority ?? 1,
    };
  } catch (err) {
    // Config file exists but is malformed - ignore and continue
    return null;
  }
}

/**
 * Load tasks from tasks.json file
 */
function loadTasks(): Task[] {
  const filePath = resolve(TASKS_FILE);

  if (!existsSync(filePath)) {
    throw new Error(`${TASKS_FILE} not found. Run 'bun src/cli/task.ts init' first.`);
  }

  const content = readFileSync(filePath, "utf-8");
  const taskFile: TaskFile = JSON.parse(content);

  // Filter to only task nodes (skip other node types)
  return taskFile.nodes.filter((node: any) => node.type === "task");
}

/**
 * Filter tasks by state
 */
function filterByState(tasks: Task[], state: string): Task[] {
  return tasks.filter(t => t.state === state);
}

/**
 * Filter blocked tasks (sorted by priority)
 */
function filterBlocked(tasks: Task[]): Task[] {
  return tasks
    .filter(t => t.state === "blocked")
    .sort((a, b) => (a.priority ?? 99) - (b.priority ?? 99));
}

/**
 * Filter ready tasks (P0-P1 only, unblocked)
 * Note: Using simple state filtering. For true dependency checking,
 * should use 'bun src/cli/task.ts ready' command.
 */
function filterReady(tasks: Task[]): Task[] {
  return tasks
    .filter(t => t.state === "created" || t.state === "ready")
    .filter(t => t.priority !== undefined && t.priority <= 1)
    .sort((a, b) => (a.priority ?? 99) - (b.priority ?? 99));
}

/**
 * Filter active tasks (sorted by start time, oldest first)
 */
function filterActive(tasks: Task[]): Task[] {
  return tasks
    .filter(t => t.state === "active")
    .sort((a, b) => {
      const timeA = a.startedAt ? new Date(a.startedAt).getTime() : 0;
      const timeB = b.startedAt ? new Date(b.startedAt).getTime() : 0;
      return timeA - timeB;
    });
}

/**
 * Filter recently completed tasks (last 24 hours)
 */
function filterRecentCompletions(tasks: Task[]): Task[] {
  const now = Date.now();
  const yesterday = now - (24 * 60 * 60 * 1000);

  return tasks
    .filter(t => t.state === "completed" && t.completedAt)
    .filter(t => {
      const completedTime = new Date(t.completedAt!).getTime();
      return completedTime > yesterday;
    })
    .sort((a, b) => {
      const timeA = new Date(a.completedAt!).getTime();
      const timeB = new Date(b.completedAt!).getTime();
      return timeB - timeA; // Most recent first
    });
}

/**
 * Filter review tasks (pending review)
 */
function filterReviews(tasks: Task[]): Task[] {
  return tasks
    .filter(t => t.labels?.includes("review") && t.state !== "completed")
    .sort((a, b) => (a.priority ?? 99) - (b.priority ?? 99));
}

/**
 * Calculate how long ago a task was started
 */
function formatDuration(startedAt: string): string {
  const start = new Date(startedAt);
  const now = new Date();
  const diffMs = now.getTime() - start.getTime();
  const diffMinutes = Math.floor(diffMs / (1000 * 60));
  const diffHours = Math.floor(diffMinutes / 60);
  const diffDays = Math.floor(diffHours / 24);

  if (diffDays > 0) return `${diffDays}d ago`;
  if (diffHours > 0) return `${diffHours}h ago`;
  if (diffMinutes > 0) return `${diffMinutes}m ago`;
  return "just now";
}

/**
 * Format a task for display
 */
function formatTask(task: Task, showDuration = false): string {
  const priority = task.priority !== undefined ? `P${task.priority}` : "  ";
  const goalPreview = task.goal.length > 50 ? task.goal.slice(0, 47) + "..." : task.goal;
  const duration = showDuration && task.startedAt ? ` (${formatDuration(task.startedAt)})` : "";
  return `${priority}  ${task.id.padEnd(15)} ${goalPreview}${duration}`;
}

/**
 * Print orientation summary
 */
function printOrientation(data: OrientationData, verbosity: VerbosityLevel = "normal"): void {
  console.log("📍 SESSION ORIENTATION");
  console.log("─".repeat(60));
  console.log();

  // Auto-mode status (if enabled)
  if (data.autoMode && verbosity !== "quiet") {
    const { enabled, targetCapacity, currentAgentCount, availableSlots } = data.autoMode;
    if (enabled) {
      console.log(`🤖 AUTO-MODE: Active`);
      console.log(`   Capacity: ${currentAgentCount}/${targetCapacity} agents running`);
      if (availableSlots > 0 && data.ready.length > 0) {
        console.log(`   💡 ${availableSlots} agent slot${availableSlots > 1 ? "s" : ""} available - consider launching ready work`);
      }
      console.log();
    } else if (verbosity === "verbose") {
      console.log(`🤖 AUTO-MODE: Disabled`);
      console.log();
    }
  }

  // Blockers (always show, even if zero)
  console.log(`🚨 BLOCKERS (${data.blockers.length})`);
  if (data.blockers.length === 0) {
    console.log("   None - all clear!");
  } else {
    data.blockers.forEach(t => console.log(`   ${formatTask(t)}`));
  }
  console.log();

  // Ready work (P0-P1 only)
  console.log(`⏭️  READY WORK (P0-P1: ${data.ready.length} tasks)`);
  if (data.ready.length === 0) {
    console.log("   No high-priority tasks ready");
  } else {
    data.ready.slice(0, 5).forEach(t => console.log(`   ${formatTask(t)}`));
    if (data.ready.length > 5) {
      console.log(`   ... and ${data.ready.length - 5} more (use 'bun src/cli/task.ts ready')`);
    }
  }
  console.log();

  // Active tasks
  console.log(`🔄 IN PROGRESS (${data.active.length} tasks)`);
  if (data.active.length === 0) {
    console.log("   No tasks currently active");
  } else {
    data.active.forEach(t => console.log(`   ${formatTask(t, true)}`));
  }
  console.log();

  // Recent completions
  console.log(`✅ COMPLETED RECENTLY (${data.recentCompletions.length} tasks)`);
  if (data.recentCompletions.length === 0) {
    console.log("   No recent completions");
  } else {
    // Show review tasks associated with completions
    if (data.reviews.length > 0) {
      console.log("   Review tasks pending:");
      data.reviews.slice(0, 3).forEach(t => console.log(`   → ${formatTask(t)}`));
      if (data.reviews.length > 3) {
        console.log(`   ... and ${data.reviews.length - 3} more reviews`);
      }
    } else {
      data.recentCompletions.slice(0, 3).forEach(t => console.log(`   ${formatTask(t)}`));
    }
  }
  console.log();

  // Project health
  const { total, completed, active, ready, blocked } = data.stats;
  const completionPct = total > 0 ? Math.round((completed / total) * 100) : 0;
  console.log("📊 PROJECT HEALTH");
  console.log(`   ${total} tasks total | ${completed} done (${completionPct}%) | ${active} active | ${ready} ready | ${blocked} blocked`);
  console.log();

  // Suggested action
  console.log("💡 SUGGESTED ACTION:");
  if (data.blockers.length > 0) {
    const task = data.blockers[0];
    console.log(`   Unblock ${task.id} first (${task.goal.slice(0, 40)}...)`);
  } else if (data.reviews.length > 0 && data.reviews[0].priority === 0) {
    const task = data.reviews[0];
    console.log(`   Review P0 task: ${task.id} (${task.goal.slice(0, 40)}...)`);
  } else if (data.ready.length > 0) {
    const task = data.ready[0];
    console.log(`   Start ${task.id} (P${task.priority}, ${task.goal.slice(0, 40)}...)`);
  } else if (data.active.length > 0) {
    const task = data.active[0];
    console.log(`   Continue ${task.id} (${task.goal.slice(0, 40)}...)`);
  } else {
    console.log("   All caught up! 🎉 (Use 'bun src/cli/task.ts list' to see all tasks)");
  }

  console.log();
  console.log("─".repeat(60));

  if (verbosity === "verbose") {
    const cacheAge = getCacheAge();
    if (cacheAge !== null) {
      console.log(`ℹ️  Using cached data (${cacheAge}s old) - Use 'bun run orient --no-cache' to force refresh`);
    } else {
      console.log(`ℹ️  Fresh data loaded from tasks.json`);
    }
  }

  if (verbosity !== "quiet") {
    console.log("Type 'bun run orient' to refresh this view");
  }
}

/**
 * Parse command-line arguments
 */
function parseArgs(): { verbosity: VerbosityLevel; useCache: boolean } {
  const args = process.argv.slice(2);
  let verbosity: VerbosityLevel = "normal";
  let useCache = true;

  for (const arg of args) {
    if (arg === "--quiet" || arg === "-q") {
      verbosity = "quiet";
    } else if (arg === "--verbose" || arg === "-v") {
      verbosity = "verbose";
    } else if (arg === "--no-cache") {
      useCache = false;
    } else if (arg === "--help" || arg === "-h") {
      console.log(`
Session Orientation - Project status at a glance

Usage:
  bun src/cli/orient.ts [options]
  bun run orient [options]

Options:
  --quiet, -q       Minimal output (no footer, no auto-mode)
  --verbose, -v     Detailed output (cache info, auto-mode status)
  --no-cache        Force refresh (ignore 5-minute cache)
  --help, -h        Show this help message

Examples:
  bun run orient              # Normal orientation
  bun run orient --quiet      # Minimal output
  bun run orient --verbose    # Detailed with cache info
  bun run orient --no-cache   # Force fresh data
`);
      process.exit(0);
    }
  }

  return { verbosity, useCache };
}

/**
 * Main entry point
 */
async function main() {
  try {
    const { verbosity, useCache } = parseArgs();

    // Try cache first if enabled
    let tasks: Task[];
    let autoModeConfig: AutoModeConfig | null = null;

    const cached = useCache ? getCached() : null;
    if (cached) {
      tasks = cached.tasks;
      if (cached.autoMode) {
        autoModeConfig = {
          enabled: cached.autoMode.enabled,
          targetCapacity: cached.autoMode.targetCapacity,
          minPriority: cached.autoMode.minPriority,
        };
      }
    } else {
      // Load fresh data
      if (!useCache) {
        clearCache();
      }
      tasks = loadTasks();
      autoModeConfig = loadAutoModeConfig();

      // Update cache for next time
      updateCache(tasks, autoModeConfig ?? undefined);
    }

    // Calculate auto-mode capacity if enabled
    let autoModeData: OrientationData["autoMode"];
    if (autoModeConfig?.enabled) {
      const agentTasks = tasks.filter(
        t => t.state === "active" && t.labels?.includes("agent")
      );
      const currentAgentCount = agentTasks.length;
      const availableSlots = Math.max(0, autoModeConfig.targetCapacity - currentAgentCount);

      autoModeData = {
        enabled: true,
        targetCapacity: autoModeConfig.targetCapacity,
        currentAgentCount,
        availableSlots,
      };
    }

    const data: OrientationData = {
      blockers: filterBlocked(tasks),
      ready: filterReady(tasks),
      active: filterActive(tasks),
      recentCompletions: filterRecentCompletions(tasks),
      reviews: filterReviews(tasks),
      stats: {
        total: tasks.length,
        completed: tasks.filter(t => t.state === "completed").length,
        active: tasks.filter(t => t.state === "active").length,
        ready: tasks.filter(t => t.state === "ready" || t.state === "created").length,
        blocked: tasks.filter(t => t.state === "blocked").length,
      },
      autoMode: autoModeData,
    };

    printOrientation(data, verbosity);
  } catch (err) {
    console.error("⚠️  Orientation failed:", err instanceof Error ? err.message : String(err));
    console.error("   (tasks.json may be missing or corrupted)");
    process.exit(1);
  }
}

main();
