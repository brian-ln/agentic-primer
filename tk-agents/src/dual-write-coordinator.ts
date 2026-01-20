/**
 * Dual-Write Coordinator
 *
 * Coordinates writes across three systems:
 * - Graph (in-memory actor system) - source of truth for mutations
 * - EventLog (append-only JSONL) - durability via event sourcing
 * - CozoDB (Datalog query engine) - queryability for complex analysis
 *
 * Design Principles:
 * 1. Graph + EventLog are ATOMIC - both succeed or both fail
 * 2. CozoDB writes are BEST-EFFORT - failures logged, don't block operation
 * 3. EventLog is the source of truth for recovery
 * 4. CozoDB can always be rebuilt from EventLog
 *
 * Error Handling:
 * - EventLog failure → rollback Graph, throw error (fail-fast)
 * - CozoDB failure → log to EventLog, continue (eventual consistency)
 *
 * Based on: DUAL_WRITE_API_DESIGN.md
 */

import type { Graph } from "./graph";
import { EventLog, type Event } from "./persistence/event-log";
import type { CozoClient } from "./cozo-client";
import type { CozoWasmClient } from "./cozo-wasm-client";
import type {
  TaskProperties,
  TaskState,
  ObjectiveCriterion,
  SubjectiveCriterion,
  Edge,
  NodeProperties,
} from "./types";
import { TaskQueries, TaskMutations, updateTaskStatus } from "./cozo-schema";
import { SchemaMigrationManager, MIGRATIONS, CozoReconciler } from "./cozo-migrations";

// Re-export CozoReconciler for backward compatibility
export { CozoReconciler };

/**
 * Options for creating a task
 */
export interface CreateTaskOptions {
  goal: string;
  desiredDeliverables: string[];
  objectiveSuccessCriteria: ObjectiveCriterion[];
  subjectiveSuccessCriteria?: SubjectiveCriterion[];
  priority?: 0 | 1 | 2 | 3 | 4;
  labels?: string[];
  parentTaskId?: string;
  knownInformation?: string[];
  informationGaps?: string[];
  toolsAvailable?: string[];
}

/**
 * Options for updating a task
 */
export interface UpdateTaskOptions {
  context?: Record<string, unknown>;
  result?: unknown;
  artifacts?: string[];
  reason?: string;
  requiredKnowledge?: string[];
}

/**
 * Dual-Write Coordinator
 *
 * Ensures Graph, EventLog, and CozoDB stay synchronized during mutations.
 */
export class DualWriteCoordinator {
  private migrationManager: SchemaMigrationManager;

  constructor(
    private graph: Graph,
    private eventLog: EventLog,
    private cozoDB: CozoClient | CozoWasmClient
  ) {
    this.migrationManager = new SchemaMigrationManager(cozoDB);
  }

  /**
   * Initialize CozoDB schema and run migrations
   *
   * Should be called once before using the coordinator.
   */
  async initialize(): Promise<void> {
    // Initialize migration metadata
    await this.migrationManager.initializeMetadata();

    // Apply pending migrations
    const results = await this.migrationManager.migrate(MIGRATIONS);

    // Check for migration failures
    const failures = results.filter((r) => !r.success);
    if (failures.length > 0) {
      console.error("Migration failures detected:", failures);
      throw new Error(
        `Schema migration failed: ${failures.map((f) => f.error).join(", ")}`
      );
    }

    console.log(`✓ CozoDB schema initialized (version ${await this.migrationManager.getCurrentVersion()})`);
  }

  /**
   * Validate CozoDB schema
   *
   * Checks that required relations exist.
   */
  async validateSchema(): Promise<{ valid: boolean; errors: string[] }> {
    const errors: string[] = [];

    const requiredRelations = ["work", "dependencies", "task_labels"];

    for (const relation of requiredRelations) {
      try {
        await this.cozoDB.run(`?[] := *${relation}{}`);
      } catch (error) {
        errors.push(`Missing relation: ${relation}`);
      }
    }

    return { valid: errors.length === 0, errors };
  }

  /**
   * Create a task with dual-write to all three systems
   *
   * Order of operations:
   * 1. Create in Graph (in-memory, fast)
   * 2. Log to EventLog (REQUIRED - throws on failure)
   * 3. Write to CozoDB (BEST-EFFORT - logs failures)
   *
   * @returns Task ID
   */
  async createTask(options: CreateTaskOptions): Promise<string> {
    const taskId = `task_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    let nodeCreated = false;

    try {
      // PHASE 1: Graph write (in-memory)
      const taskProps: TaskProperties = {
        id: taskId,
        type: "task",
        state: "created",
        goal: options.goal,
        desiredDeliverables: options.desiredDeliverables,
        objectiveSuccessCriteria: options.objectiveSuccessCriteria,
        subjectiveSuccessCriteria: options.subjectiveSuccessCriteria,
        knownInformation: options.knownInformation || [],
        informationGaps: options.informationGaps || [],
        toolsAvailable: options.toolsAvailable || [],
        labels: options.labels || [],
        priority: options.priority,
        parentTaskId: options.parentTaskId,
        createdAt: new Date(),
      };

      // Import task actor dynamically to avoid circular deps
      const { TaskActor } = await import("./task");
      const address = TaskActor({ ...taskProps, graph: this.graph });
      this.graph.registerNode(taskId, address, taskProps);
      nodeCreated = true;

      // PHASE 2: EventLog write (REQUIRED - fail-fast)
      try {
        this.eventLog.append({
          timestamp: new Date().toISOString(),
          type: "task_created",
          nodeId: taskId,
          data: {
            goal: taskProps.goal,
            state: taskProps.state,
            priority: taskProps.priority,
            labels: taskProps.labels,
            desiredDeliverables: taskProps.desiredDeliverables,
            objectiveSuccessCriteria: taskProps.objectiveSuccessCriteria,
            subjectiveSuccessCriteria: taskProps.subjectiveSuccessCriteria,
            parentTaskId: taskProps.parentTaskId,
          },
          metadata: {
            operation: "create",
            createdAt: taskProps.createdAt.toISOString(),
          },
        });
      } catch (error) {
        // CRITICAL: EventLog write failed - rollback Graph
        this.graph.removeNode(taskId);
        throw new Error(
          `EventLog write failed for task ${taskId}: ${error instanceof Error ? error.message : String(error)}`,
          { cause: error }
        );
      }

      // PHASE 3: CozoDB write (BEST-EFFORT - log failures, don't throw)
      await this.writeToCozoDB(taskId, taskProps);

      return taskId;
    } catch (error) {
      // If we created node but EventLog failed, it's already rolled back above
      // Re-throw the error for caller to handle
      throw error;
    }
  }

  /**
   * Update task state via actor message
   *
   * @param taskId Task ID
   * @param action Action to perform (start, complete, block)
   * @param options Update options
   */
  async updateTask(
    taskId: string,
    action: "start" | "complete" | "block",
    options: UpdateTaskOptions = {}
  ): Promise<void> {
    // Get current state before mutation
    const beforeProps = this.graph.getNodeProperties(taskId) as TaskProperties | undefined;
    if (!beforeProps) {
      throw new Error(`Task not found: ${taskId}`);
    }

    const previousState = beforeProps.state;

    try {
      // PHASE 1: Apply to Graph via actor message
      await this.graph.send(taskId, action, options);

      // Get updated state
      const afterProps = this.graph.getNodeProperties(taskId) as TaskProperties;

      // PHASE 2: Log to EventLog (REQUIRED)
      this.eventLog.append({
        timestamp: new Date().toISOString(),
        type: `task_${action}`,
        nodeId: taskId,
        data: {
          previousState,
          newState: afterProps.state,
          ...options,
        },
        metadata: {
          operation: "update",
        },
      });

      // PHASE 3: Update CozoDB (BEST-EFFORT)
      try {
        // Use schema helper for status updates (handles two-phase update)
        await updateTaskStatus(this.cozoDB, taskId, afterProps.state);
      } catch (error) {
        console.error(`CozoDB update failed for ${taskId}:`, error);
        this.eventLog.append({
          timestamp: new Date().toISOString(),
          type: "cozo_write_failed",
          nodeId: taskId,
          data: {
            operation: "update",
            error: String(error),
          },
        });
      }
    } catch (error) {
      // Actor message or EventLog failed
      throw new Error(
        `Update task failed for ${taskId}: ${error instanceof Error ? error.message : String(error)}`,
        { cause: error }
      );
    }
  }

  /**
   * Add dependency edge between tasks
   *
   * @param fromId Dependent task ID
   * @param toId Dependency task ID (task that fromId depends on)
   * @returns Edge ID
   */
  async addDependency(fromId: string, toId: string): Promise<string> {
    // Validate both tasks exist
    if (!this.graph.getNode(fromId)) {
      throw new Error(`Task not found: ${fromId}`);
    }
    if (!this.graph.getNode(toId)) {
      throw new Error(`Task not found: ${toId}`);
    }

    // PHASE 1: Add edge to Graph
    const edge = this.graph.addEdge(fromId, toId, "depends_on");

    try {
      // PHASE 2: Log to EventLog (REQUIRED)
      this.eventLog.append({
        timestamp: new Date().toISOString(),
        type: "edge_created",
        nodeId: fromId,
        data: {
          edgeId: edge.id,
          edgeType: "depends_on",
          fromId,
          toId,
        },
        metadata: { operation: "create_edge" },
      });

      // PHASE 3: Write to CozoDB (BEST-EFFORT)
      try {
        await this.cozoDB.run(TaskMutations.ADD_DEPENDENCY(fromId, toId));
      } catch (error) {
        console.error(`CozoDB edge write failed: ${edge.id}`, error);
        this.eventLog.append({
          timestamp: new Date().toISOString(),
          type: "cozo_write_failed",
          nodeId: fromId,
          data: {
            operation: "create_edge",
            error: String(error),
          },
        });
      }

      return edge.id;
    } catch (error) {
      // EventLog failed - rollback edge
      this.graph.removeEdge(edge.id);
      throw new Error(
        `Add dependency failed: ${error instanceof Error ? error.message : String(error)}`,
        { cause: error }
      );
    }
  }

  /**
   * Delete a task and all connected edges
   *
   * @param taskId Task ID to delete
   */
  async deleteTask(taskId: string): Promise<void> {
    const props = this.graph.getNodeProperties(taskId);
    if (!props) {
      throw new Error(`Task not found: ${taskId}`);
    }

    // Get edges before deletion (for event logging)
    const edges = this.graph.getAllEdges(taskId);
    const edgeIds = edges.map((e) => e.id);

    // PHASE 1: Remove from Graph (also removes connected edges)
    const removed = this.graph.removeNode(taskId);
    if (!removed) {
      throw new Error(`Failed to remove task from Graph: ${taskId}`);
    }

    try {
      // PHASE 2: Log to EventLog (REQUIRED)
      this.eventLog.append({
        timestamp: new Date().toISOString(),
        type: "task_deleted",
        nodeId: taskId,
        data: {
          edgesRemoved: edgeIds,
          goal: (props as TaskProperties).goal,
        },
        metadata: { operation: "delete" },
      });

      // PHASE 3: Delete from CozoDB (BEST-EFFORT)
      try {
        // Use schema mutations for deletion
        await this.cozoDB.run(TaskMutations.DELETE_TASK(taskId));
        await this.cozoDB.run(TaskMutations.DELETE_TASK_DEPENDENCIES(taskId));
        await this.cozoDB.run(TaskMutations.DELETE_TASK_LABELS(taskId));
      } catch (error) {
        console.error(`CozoDB delete failed for ${taskId}:`, error);
        this.eventLog.append({
          timestamp: new Date().toISOString(),
          type: "cozo_write_failed",
          nodeId: taskId,
          data: {
            operation: "delete",
            error: String(error),
          },
        });
      }
    } catch (error) {
      // EventLog failed - we can't rollback easily (task already removed from Graph)
      // Log the error but don't throw - Graph is already modified
      console.error(`EventLog write failed during delete of ${taskId}:`, error);
      throw error;
    }
  }

  /**
   * Write task to CozoDB (best-effort)
   *
   * Uses schema-defined mutations for consistency.
   *
   * @param taskId Task ID
   * @param props Task properties
   */
  public async writeToCozoDB(taskId: string, props: TaskProperties): Promise<void> {
    try {
      // Use schema mutation template
      // ESCAPE for CozoScript (double quotes with backslash escape)
      const title = props.goal.replace(/"/g, '\\"');

      await this.cozoDB.run(
        `?[id, type, status, priority, title] <- [[$id, $type, $status, $priority, $title]]
         :put work {id, type, status, priority, title}`,
        {
          id: taskId,
          type: "task",
          status: props.state,
          priority: props.priority ?? null,
          title: props.goal // CozoClient handles params safely
        }
      );

      // Write labels if present
      if (props.labels && props.labels.length > 0) {
        for (const label of props.labels) {
          await this.cozoDB.run(TaskMutations.ADD_LABEL(taskId, label));
        }
      }
    } catch (error) {
      console.error(`CozoDB write failed for ${taskId}:`, error);
      // Log failure to EventLog for later replay
      this.eventLog.append({
        timestamp: new Date().toISOString(),
        type: "cozo_write_failed",
        nodeId: taskId,
        data: {
          operation: "create",
          error: String(error),
        },
      });
      // DO NOT throw - operation succeeded in Graph+EventLog
    }
  }

  /**
   * Get the EventLog instance
   */
  getEventLog(): EventLog {
    return this.eventLog;
  }

  /**
   * Get the Graph instance
   */
  getGraph(): Graph {
    return this.graph;
  }

  /**
   * Get the CozoDB client
   */
  getCozoDB(): CozoClient {
    return this.cozoDB;
  }
}

/**
 * Query Service for CozoDB
 *
 * Complex queries that leverage Datalog rules.
 * Uses schema-defined query templates for consistency.
 */
export class TaskQueryService {
  constructor(private cozoDB: CozoClient | CozoWasmClient) {}

  /**
   * Find ready tasks (no blockers)
   *
   * Uses schema-defined query template.
   */
  async findReadyTasks(): Promise<
    Array<{ id: string; title: string; priority: number | null }>
  > {
    const result = await this.cozoDB.run(TaskQueries.FIND_READY);

    return result.rows.map((row) => ({
      id: row[0] as string,
      title: row[1] as string,
      priority: row[2] as number | null,
    }));
  }

  /**
   * Find blocked tasks with blocker details
   *
   * Uses schema-defined query template.
   */
  async findBlockedTasks(): Promise<
    Array<{
      id: string;
      title: string;
      blockers: Array<{ id: string; title: string; status: string }>;
    }>
  > {
    const result = await this.cozoDB.run(TaskQueries.FIND_BLOCKED);

    // Group by task
    const taskMap = new Map<string, { id: string; title: string; blockers: Array<{ id: string; title: string; status: string }> }>();

    for (const row of result.rows) {
      const [taskId, title, depId, depTitle, depStatus] = row;

      if (!taskMap.has(taskId as string)) {
        taskMap.set(taskId as string, {
          id: taskId as string,
          title: title as string,
          blockers: [],
        });
      }

      taskMap.get(taskId as string)!.blockers.push({
        id: depId as string,
        title: depTitle as string,
        status: depStatus as string,
      });
    }

    return Array.from(taskMap.values());
  }

  /**
   * Get task dependency graph (recursive)
   *
   * Uses schema-defined query template.
   */
  async getDependencyGraph(taskId: string): Promise<
    Array<{ taskId: string; depId: string; depTitle: string; depStatus: string }>
  > {
    const result = await this.cozoDB.run(TaskQueries.DEPENDENCY_GRAPH(taskId));

    return result.rows.map((row) => ({
      taskId: row[0] as string,
      depId: row[1] as string,
      depTitle: row[2] as string,
      depStatus: row[3] as string,
    }));
  }

  /**
   * Search tasks by keyword (case-insensitive substring match)
   *
   * Uses schema-defined query template.
   */
  async searchTasks(
    keyword: string
  ): Promise<Array<{ id: string; title: string; status: string }>> {
    const result = await this.cozoDB.run(TaskQueries.SEARCH_TASKS(keyword));

    return result.rows.map((row) => ({
      id: row[0] as string,
      title: row[1] as string,
      status: row[2] as string,
    }));
  }

  /**
   * Get task by ID (read-only from CozoDB)
   *
   * Uses schema-defined query template.
   */
  async getTask(taskId: string): Promise<{
    id: string;
    type: string;
    status: string;
    priority: number | null;
    title: string;
  } | null> {
    const result = await this.cozoDB.run(TaskQueries.GET_TASK(taskId));

    if (result.rows.length === 0) {
      return null;
    }

    const [id, type, status, priority, title] = result.rows[0];
    return {
      id: id as string,
      type: type as string,
      status: status as string,
      priority: priority as number | null,
      title: title as string,
    };
  }
}

// CozoReconciler is now imported from cozo-migrations.ts
// No need to duplicate it here
