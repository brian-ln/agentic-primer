#!/usr/bin/env bun
/**
 * CozoDB Schema for Task Graph System
 *
 * Defines the Datalog relations and rules for querying task dependencies,
 * blocked tasks, ready tasks, and dependency graphs.
 *
 * This schema implements the dual-write architecture where:
 * - Graph (in-memory) = source of truth for mutations
 * - EventLog (JSONL) = durability layer
 * - CozoDB = queryable projection for complex Datalog queries
 */

import { createCozoClient } from "./cozo-wasm-client";
import type { CozoWasmClient } from "./cozo-wasm-client";

/**
 * Initialize CozoDB with task tracking schema
 *
 * Creates relations for:
 * - work: Task/work items with id, type, status, priority, title
 * - dependencies: Dependency edges between tasks
 * - task_labels: Labels/tags for tasks
 *
 * @returns Initialized CozoDB client with schema created
 */
export async function initializeTaskSchema(): Promise<CozoWasmClient> {
  const db = await createCozoClient();

  // Create work relation (tasks)
  await db.run(`
    :create work {
      id: String,           # task_1, task_2, etc.
      type: String,         # "task" (future: "knowledge", "artifact")
      status: String,       # created, ready, active, blocked, completed, failed
      priority: Int?,       # 0-4 (0 is highest), nullable
      title: String         # Short goal description
    }
  `);

  // Create dependencies relation
  await db.run(`
    :create dependencies {
      task: String,         # Dependent task ID
      dep: String           # Dependency task ID (task depends on dep)
    }
  `);

  // Create task labels relation
  await db.run(`
    :create task_labels {
      task: String,         # Task ID
      label: String         # Label name
    }
  `);

  return db;
}

/**
 * Define Datalog rules for task queries
 *
 * Note: In CozoDB, rules are defined inline with queries, not as separate statements.
 * This function is a no-op but kept for API consistency.
 *
 * @param db CozoDB client
 */
export async function defineTaskRules(db: CozoWasmClient): Promise<void> {
  // Rules are defined inline with queries in CozoDB
  // See TaskQueries for rule definitions
}

/**
 * Common Datalog queries for task tracking
 */
export const TaskQueries = {
  /**
   * Find ready tasks (no blockers)
   * Uses stratified negation to find tasks not blocked by any dependency
   */
  FIND_READY: `
    # Define blocked tasks (tasks with incomplete dependencies)
    blocked[task, dep] :=
      *work{id: task, status},
      status != 'completed',
      status != 'failed',
      *dependencies{task, dep},
      *work{id: dep, status: dep_status},
      dep_status != 'completed'

    # Define ready tasks (not blocked)
    ready[task, title, priority] :=
      *work{id: task, status, title, priority},
      status != 'completed',
      status != 'failed',
      not blocked[task, _]

    # Query ready tasks
    ?[id, title, priority] := ready[id, title, priority]
  `,

  /**
   * Find blocked tasks with blocker details
   */
  FIND_BLOCKED: `
    # Define blocked tasks
    blocked[task, dep] :=
      *work{id: task, status},
      status != 'completed',
      status != 'failed',
      *dependencies{task, dep},
      *work{id: dep, status: dep_status},
      dep_status != 'completed'

    # Query blocked tasks with details
    ?[task, task_title, dep, dep_title, dep_status] :=
      blocked[task, dep],
      *work{id: task, title: task_title},
      *work{id: dep, title: dep_title, status: dep_status}
  `,

  /**
   * Get task by ID
   */
  GET_TASK: (taskId: string) => `
    ?[id, type, status, priority, title] :=
      *work{id, type, status, priority, title},
      id = '${taskId.replace(/'/g, "''")}'
  `,

  /**
   * Get dependency graph (recursive)
   */
  DEPENDENCY_GRAPH: (taskId: string) => `
    # Recursive transitive closure
    dep_closure[task, dep] := *dependencies{task, dep}
    dep_closure[task, dep] :=
      *dependencies{task, intermediate},
      dep_closure[intermediate, dep]

    ?[task, dep, title, status] :=
      dep_closure['${taskId.replace(/'/g, "''")}', dep],
      *work{id: dep, title, status}
  `,

  /**
   * Search tasks by keyword (case-insensitive substring match)
   */
  SEARCH_TASKS: (keyword: string) => `
    ?[id, title, status] :=
      *work{id, title, status},
      str_includes(lowercase(title), lowercase('${keyword.replace(/'/g, "''")}'))
  `,

  /**
   * List tasks with optional filters
   */
  LIST_TASKS: (filters?: { status?: string; label?: string; priority?: number }) => {
    let conditions = "*work{id, title, status, priority}";

    const whereClauses: string[] = [];

    if (filters?.status) {
      whereClauses.push(`status = '${filters.status.replace(/'/g, "''")}'`);
    }

    if (filters?.priority !== undefined) {
      whereClauses.push(`priority = ${filters.priority}`);
    }

    let query = `?[id, title, status, priority] := ${conditions}`;

    if (whereClauses.length > 0) {
      query += ", " + whereClauses.join(", ");
    }

    if (filters?.label) {
      query = `
        ${query},
        *task_labels{task: id, label},
        label = '${filters.label.replace(/'/g, "''")}'
      `;
    }

    return query;
  },

  /**
   * Get all labels for a task
   */
  GET_TASK_LABELS: (taskId: string) => `
    ?[label] :=
      *task_labels{task: '${taskId.replace(/'/g, "''")}', label}
  `,

  /**
   * Count tasks by status
   */
  COUNT_BY_STATUS: `
    ?[status, count(id)] :=
      *work{id, status}
  `,
};

/**
 * Helper functions for inserting/updating data
 */
export const TaskMutations = {
  /**
   * Insert or update a task
   */
  PUT_TASK: (task: {
    id: string;
    type: string;
    status: string;
    priority?: number | null;
    title: string;
  }) => `
    ?[id, type, status, priority, title] <- [[
      '${task.id.replace(/'/g, "''")}',
      '${task.type.replace(/'/g, "''")}',
      '${task.status.replace(/'/g, "''")}',
      ${task.priority ?? "null"},
      '${task.title.replace(/'/g, "''")}'
    ]]
    :put work {id, type, status, priority, title}
  `,

  /**
   * Update task status (Step 1: Delete old record)
   * Must be called before UPDATE_STATUS_PUT
   */
  UPDATE_STATUS_RM: (taskId: string) => `
    ?[id, type, status, priority, title] :=
      *work{id, type, status, priority, title},
      id = '${taskId.replace(/'/g, "''")}'
    :rm work {id, type, status, priority, title}
  `,

  /**
   * Update task status (Step 2: Insert with new status)
   * Must be called after UPDATE_STATUS_RM
   *
   * Note: Due to CozoDB limitations, updates require two separate queries.
   * Use the async updateTaskStatus() helper function instead of calling these directly.
   */
  UPDATE_STATUS_PUT: (taskId: string, status: string, type: string, priority: number | null, title: string) => `
    ?[id, type, status, priority, title] <- [[
      '${taskId.replace(/'/g, "''")}',
      '${type.replace(/'/g, "''")}',
      '${status.replace(/'/g, "''")}',
      ${priority ?? "null"},
      '${title.replace(/'/g, "''")}'
    ]]
    :put work {id, type, status, priority, title}
  `,

  /**
   * Add dependency
   */
  ADD_DEPENDENCY: (fromId: string, toId: string) => `
    ?[task, dep] <- [['${fromId.replace(/'/g, "''")}', '${toId.replace(/'/g, "''")}']]
    :put dependencies {task, dep}
  `,

  /**
   * Remove dependency
   */
  REMOVE_DEPENDENCY: (fromId: string, toId: string) => `
    ?[task, dep] <- [['${fromId.replace(/'/g, "''")}', '${toId.replace(/'/g, "''")}']]
    :rm dependencies {task, dep}
  `,

  /**
   * Add label to task
   */
  ADD_LABEL: (taskId: string, label: string) => `
    ?[task, label] <- [['${taskId.replace(/'/g, "''")}', '${label.replace(/'/g, "''")}']]
    :put task_labels {task, label}
  `,

  /**
   * Remove label from task
   */
  REMOVE_LABEL: (taskId: string, label: string) => `
    ?[task, label] <- [['${taskId.replace(/'/g, "''")}', '${label.replace(/'/g, "''")}']]
    :rm task_labels {task, label}
  `,

  /**
   * Delete task
   */
  DELETE_TASK: (taskId: string) => `
    ?[id, type, status, priority, title] :=
      *work{id, type, status, priority, title},
      id = '${taskId.replace(/'/g, "''")}'
    :rm work {id, type, status, priority, title}
  `,

  /**
   * Delete all dependencies for a task (both incoming and outgoing)
   */
  DELETE_TASK_DEPENDENCIES: (taskId: string) => `
    ?[task, dep] :=
      *dependencies{task, dep},
      (task = '${taskId.replace(/'/g, "''")}' || dep = '${taskId.replace(/'/g, "''")}')
    :rm dependencies {task, dep}
  `,

  /**
   * Delete all labels for a task
   */
  DELETE_TASK_LABELS: (taskId: string) => `
    ?[task, label] :=
      *task_labels{task, label},
      task = '${taskId.replace(/'/g, "''")}'
    :rm task_labels {task, label}
  `,
};

/**
 * Helper functions for common operations
 */
export async function updateTaskStatus(
  db: CozoWasmClient,
  taskId: string,
  newStatus: string
): Promise<void> {
  // Get current task data
  const result = await db.run(TaskQueries.GET_TASK(taskId));
  if (result.rows.length === 0) {
    throw new Error(`Task not found: ${taskId}`);
  }

  const [id, type, _oldStatus, priority, title] = result.rows[0];

  // Delete old record
  await db.run(TaskMutations.UPDATE_STATUS_RM(taskId));

  // Insert with new status
  await db.run(
    TaskMutations.UPDATE_STATUS_PUT(
      taskId,
      newStatus,
      type as string,
      priority as number | null,
      title as string
    )
  );
}

/**
 * Example usage
 */
if (import.meta.main) {
  console.log("🗄️  CozoDB Schema Setup for Task Tracking\n");
  console.log("=" .repeat(60) + "\n");

  const db = await initializeTaskSchema();
  console.log("✓ Schema created (work, dependencies, task_labels)");

  await defineTaskRules(db);
  console.log("✓ Rules defined (blocked, ready)");

  // Insert example tasks
  await db.run(
    TaskMutations.PUT_TASK({
      id: "task_1",
      type: "task",
      status: "created",
      priority: 1,
      title: "Implement feature A",
    })
  );

  await db.run(
    TaskMutations.PUT_TASK({
      id: "task_2",
      type: "task",
      status: "created",
      priority: 2,
      title: "Write tests for feature A",
    })
  );

  await db.run(TaskMutations.ADD_DEPENDENCY("task_1", "task_2"));
  console.log("✓ Example tasks created with dependency");

  // Query ready tasks
  const readyResult = await db.run(TaskQueries.FIND_READY);
  console.log("\n📋 Ready tasks:");
  console.log(readyResult.rows);

  // Query blocked tasks
  const blockedResult = await db.run(TaskQueries.FIND_BLOCKED);
  console.log("\n🚫 Blocked tasks:");
  console.log(blockedResult.rows);

  // Complete task_2
  await updateTaskStatus(db, "task_2", "completed");
  console.log("\n✓ Completed task_2");

  // Debug: Check all tasks
  const allTasks = await db.run("?[id, status] := *work{id, status}");
  console.log("\n🔍 All tasks after update:");
  console.log(allTasks.rows);

  // Query ready tasks again
  const readyResult2 = await db.run(TaskQueries.FIND_READY);
  console.log("\n📋 Ready tasks (after completion):");
  console.log(readyResult2.rows);

  db.close();
  console.log("\n✅ Schema setup complete!\n");
}
