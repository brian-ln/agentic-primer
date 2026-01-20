/**
 * Built-in Lifecycle Hooks
 *
 * Pre-defined hooks for common automation patterns:
 * 1. auto-create-review-task - Create review task on agent completion
 * 2. check-dependency-satisfaction - Transition tasks when dependencies complete
 * 3. track-agent-task-lifecycle - Log agent metrics
 * 4. auto-label-by-keyword - Auto-label tasks based on goal keywords
 */

import { BaseHook, type HookAction, type HookContext } from "./hooks.ts";
import type { Event } from "../persistence/event-log.ts";
import { EVENT_TYPES } from "../events/task-events.ts";

/**
 * Hook 1: Auto-Create Review Task
 *
 * When an agent task completes, automatically create a review task.
 *
 * Priority: 10 (runs early)
 * Subscribes to: task_completed
 */
export class AutoCreateReviewTaskHook extends BaseHook {
  name = "auto-create-review-task";
  eventTypes = [EVENT_TYPES.TASK_COMPLETED];

  constructor() {
    super({
      priority: 10,
      enabled: true,
      metadata: {
        description: "Automatically create review task when agent task completes",
        version: "1.0.0",
      },
    });
  }

  async handler(event: Event, context: HookContext): Promise<HookAction[]> {
    const taskProps = this.getTaskProps(context, event.nodeId);

    // Guard: Task not found
    if (!taskProps) {
      return [];
    }

    // Guard: Only for agent tasks
    if (!this.hasLabel(context, event.nodeId, "agent")) {
      return [];
    }

    // Guard: Don't create review for review tasks
    if (this.hasLabel(context, event.nodeId, "review")) {
      return [];
    }

    // Create review task
    const reviewGoal = `Review: ${taskProps.goal}`;
    const reviewLabels = ["review", ...(taskProps.labels as string[] || [])];
    const reviewPriority = (taskProps.priority as number) || 2;

    return [
      this.createTaskAction(reviewGoal, {
        deliverables: ["Review completed", "Feedback provided"],
        labels: reviewLabels,
        priority: reviewPriority,
        parentTaskId: event.nodeId,
      }),
    ];
  }
}

/**
 * Hook 2: Check Dependency Satisfaction
 *
 * When a task completes, check if dependent tasks can transition to ready.
 *
 * Priority: 20 (runs after review creation)
 * Subscribes to: task_completed
 */
export class CheckDependencySatisfactionHook extends BaseHook {
  name = "check-dependency-satisfaction";
  eventTypes = [EVENT_TYPES.TASK_COMPLETED];

  constructor() {
    super({
      priority: 20,
      enabled: true,
      metadata: {
        description: "Check if dependent tasks can transition to ready when dependencies complete",
        version: "1.0.0",
      },
    });
  }

  async handler(event: Event, context: HookContext): Promise<HookAction[]> {
    const actions: HookAction[] = [];

    try {
      // Find tasks that depend on this completed task
      const edgesTo = context.graph.getEdgesTo(event.nodeId);
      const dependentTaskIds = edgesTo
        .filter((e) => e.type === "depends_on")
        .map((e) => e.fromId);

      for (const taskId of dependentTaskIds) {
        const taskProps = this.getTaskProps(context, taskId);

        if (!taskProps) continue;

        // Only transition if task is in 'created' state
        if (taskProps.state !== "created") {
          continue;
        }

        // Check if ALL dependencies are satisfied
        const edgesFrom = context.graph.getEdgesFrom(taskId);
        const dependencies = edgesFrom.filter((e) => e.type === "depends_on");

        const allSatisfied = dependencies.every((dep) => {
          const depProps = this.getTaskProps(context, dep.toId);
          return depProps?.state === "completed";
        });

        // If all dependencies satisfied, transition to ready
        if (allSatisfied) {
          actions.push(this.updateTaskAction(taskId, "transition_to_ready"));

          // Emit dependency_satisfied event
          actions.push(
            this.createLogAction(EVENT_TYPES.DEPENDENCY_SATISFIED, taskId, {
              dependentTaskId: taskId,
              completedTaskId: event.nodeId,
            })
          );
        }
      }
    } catch (error) {
      console.error("Error in check-dependency-satisfaction hook:", error);
    }

    return actions;
  }
}

/**
 * Hook 3: Track Agent Task Lifecycle
 *
 * Track metrics for agent tasks (duration, deliverables, etc.)
 *
 * Priority: 50 (runs after critical hooks)
 * Subscribes to: task_created, task_started, task_completed
 */
export class TrackAgentTaskLifecycleHook extends BaseHook {
  name = "track-agent-task-lifecycle";
  eventTypes = [EVENT_TYPES.TASK_CREATED, EVENT_TYPES.TASK_STARTED, EVENT_TYPES.TASK_COMPLETED];

  constructor() {
    super({
      priority: 50,
      enabled: true,
      metadata: {
        description: "Track agent task metrics (duration, deliverables)",
        version: "1.0.0",
      },
    });
  }

  async handler(event: Event, context: HookContext): Promise<HookAction[]> {
    const taskProps = this.getTaskProps(context, event.nodeId);

    // Guard: Task not found
    if (!taskProps) {
      return [];
    }

    // Guard: Only for agent tasks
    if (!this.hasLabel(context, event.nodeId, "agent")) {
      return [];
    }

    const actions: HookAction[] = [];

    // Only log metrics on completion
    if (event.type === EVENT_TYPES.TASK_COMPLETED) {
      // Calculate duration if both timestamps exist
      let durationMs: number | null = null;
      if (taskProps.startedAt && taskProps.completedAt) {
        const startTime = new Date(taskProps.startedAt as string).getTime();
        const endTime = new Date(taskProps.completedAt as string).getTime();
        durationMs = endTime - startTime;
      }

      // Log agent metrics
      actions.push(
        this.createLogAction("agent_metrics", event.nodeId, {
          goal: taskProps.goal,
          labels: taskProps.labels,
          priority: taskProps.priority,
          durationMs,
          deliverables: taskProps.desiredDeliverables,
          startedAt: taskProps.startedAt,
          completedAt: taskProps.completedAt,
        })
      );
    }

    return actions;
  }
}

/**
 * Hook 4: Auto-Label by Keyword
 *
 * Automatically add labels to tasks based on keywords in goal.
 *
 * Priority: 5 (runs very early)
 * Subscribes to: task_created
 */
export class AutoLabelByKeywordHook extends BaseHook {
  name = "auto-label-by-keyword";
  eventTypes = [EVENT_TYPES.TASK_CREATED];

  private keywordMappings: Map<string, string[]>;

  constructor() {
    super({
      priority: 5,
      enabled: true,
      metadata: {
        description: "Auto-label tasks based on keywords in goal",
        version: "1.0.0",
      },
    });

    // Define keyword → label mappings
    this.keywordMappings = new Map([
      // Technical domains
      ["research", ["research", "investigation"]],
      ["implement", ["implementation", "coding"]],
      ["test", ["testing", "qa"]],
      ["debug", ["debugging", "bugfix"]],
      ["refactor", ["refactoring", "cleanup"]],
      ["document", ["documentation", "docs"]],
      ["review", ["review", "code-review"]],

      // Project phases
      ["design", ["design", "architecture"]],
      ["plan", ["planning", "roadmap"]],
      ["deploy", ["deployment", "release"]],

      // Priorities (implicit)
      ["critical", ["high-priority", "urgent"]],
      ["urgent", ["high-priority", "urgent"]],
      ["fix", ["bugfix", "maintenance"]],

      // Work types
      ["agent:", ["agent"]],
      ["automation", ["automation", "workflow"]],
      ["integration", ["integration"]],
      ["api", ["api", "backend"]],
      ["ui", ["frontend", "ui"]],
      ["database", ["database", "persistence"]],
    ]);
  }

  async handler(event: Event, context: HookContext): Promise<HookAction[]> {
    const taskProps = this.getTaskProps(context, event.nodeId);

    // Guard: Task not found
    if (!taskProps) {
      return [];
    }

    const goal = (taskProps.goal as string || "").toLowerCase();
    const existingLabels = new Set(taskProps.labels as string[] || []);
    const newLabels = new Set<string>();

    // Check for keyword matches
    for (const [keyword, labels] of this.keywordMappings.entries()) {
      if (goal.includes(keyword.toLowerCase())) {
        // Add labels if not already present
        for (const label of labels) {
          if (!existingLabels.has(label)) {
            newLabels.add(label);
          }
        }
      }
    }

    // If we found new labels, return update action
    if (newLabels.size > 0) {
      const allLabels = [...existingLabels, ...newLabels];

      // Note: This would require a "set_labels" update action
      // For now, we'll just log it as a suggestion
      return [
        this.createLogAction("auto_label_suggestion", event.nodeId, {
          suggestedLabels: Array.from(newLabels),
          existingLabels: Array.from(existingLabels),
          goal: taskProps.goal,
        }),
      ];
    }

    return [];
  }
}

/**
 * Register all built-in hooks
 *
 * @param registry HookRegistry to register hooks with
 */
export function registerBuiltinHooks(registry: { register: (hook: BaseHook) => void }): void {
  registry.register(new AutoCreateReviewTaskHook());
  registry.register(new CheckDependencySatisfactionHook());
  registry.register(new TrackAgentTaskLifecycleHook());
  registry.register(new AutoLabelByKeywordHook());
}
