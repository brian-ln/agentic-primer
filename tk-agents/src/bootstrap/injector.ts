// Task Injector - Dynamic task creation and dependency management

import type { Graph } from "../graph";
import { TaskNode } from "../task";
import type { ObjectiveCriterion } from "../types";

/**
 * When to inject the task
 */
export type TriggerCondition = "immediate" | "before_complete" | "on_block";

/**
 * Task injection specification
 */
export interface TaskInjection {
  parentTaskId: string;
  goal: string;
  deliverables: string[];
  criteria: ObjectiveCriterion[];
  triggerCondition?: TriggerCondition;
  makeDependency?: boolean; // Should parent depend on this task?
}

/**
 * Injection rule - pre-registered conditions
 */
export interface InjectionRule {
  name: string;
  condition: string;
  factory: () => Omit<TaskInjection, "parentTaskId">;
}

/**
 * TaskInjector - Manages dynamic task creation
 */
export class TaskInjector {
  private rules: Map<string, InjectionRule> = new Map();

  /**
   * Register an injection rule for later use
   */
  registerRule(rule: InjectionRule): void {
    this.rules.set(rule.name, rule);
  }

  /**
   * Inject a task dynamically
   */
  inject(injection: TaskInjection, graph: Graph): TaskNode {
    // Validate parent exists
    const parent = graph.getNode(injection.parentTaskId);
    if (!parent) {
      throw new Error(`Parent task not found: ${injection.parentTaskId}`);
    }

    // Create child task
    const childTask = new TaskNode({
      goal: injection.goal,
      desiredDeliverables: injection.deliverables,
      objectiveSuccessCriteria: injection.criteria,
      parentTaskId: injection.parentTaskId,
    });

    graph.registerNode(childTask);

    // Link child to parent
    graph.addEdge(childTask.properties.id, injection.parentTaskId, "spawned_by");

    // If makeDependency, parent depends on child completing
    if (injection.makeDependency !== false) { // Default true
      graph.addEdge(injection.parentTaskId, childTask.properties.id, "depends_on");
    }

    return childTask;
  }

  /**
   * Apply a pre-registered rule
   */
  applyRule(ruleName: string, parentTaskId: string, graph: Graph): TaskNode {
    const rule = this.rules.get(ruleName);
    if (!rule) {
      throw new Error(`Injection rule not found: ${ruleName}`);
    }

    const injection: TaskInjection = {
      parentTaskId,
      ...rule.factory(),
    };

    return this.inject(injection, graph);
  }

  /**
   * Check if any rules should trigger for a task
   */
  checkTriggers(
    taskId: string,
    event: "before_complete" | "on_block",
    graph: Graph
  ): TaskNode[] {
    const injectedTasks: TaskNode[] = [];

    for (const rule of this.rules.values()) {
      // Simple condition matching (can be expanded)
      if (rule.condition.includes(event)) {
        try {
          const task = this.applyRule(rule.name, taskId, graph);
          injectedTasks.push(task);
        } catch (error) {
          // Rule might not apply, continue
          console.warn(`Failed to apply rule ${rule.name}:`, error);
        }
      }
    }

    return injectedTasks;
  }

  /**
   * List all registered rules
   */
  listRules(): InjectionRule[] {
    return Array.from(this.rules.values());
  }

  /**
   * Remove a rule
   */
  removeRule(ruleName: string): boolean {
    return this.rules.delete(ruleName);
  }
}

// Singleton for convenience
export const taskInjector = new TaskInjector();
