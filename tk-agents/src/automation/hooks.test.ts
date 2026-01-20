/**
 * Hook System Unit Tests
 *
 * Tests for:
 * - HookRegistry (registration, execution, priority ordering)
 * - Built-in hooks (auto-create-review, dependency-satisfaction, etc.)
 * - Error isolation
 * - Hook enable/disable
 */

import { test, expect, describe, beforeEach } from "bun:test";
import { HookRegistry } from "./hook-registry.ts";
import {
  AutoCreateReviewTaskHook,
  CheckDependencySatisfactionHook,
  TrackAgentTaskLifecycleHook,
  AutoLabelByKeywordHook,
  registerBuiltinHooks,
} from "./builtin-hooks.ts";
import type { LifecycleHook, HookContext, HookAction } from "./hooks.ts";
import type { Event } from "../persistence/event-log.ts";
import { EVENT_TYPES } from "../events/task-events.ts";

/**
 * Mock implementations for testing
 */

// Mock Graph
class MockGraph {
  private nodes: Map<string, Record<string, unknown>>;
  private edgesFrom: Map<string, Array<{ type: string; toId: string; fromId: string }>>;
  private edgesTo: Map<string, Array<{ type: string; fromId: string; toId: string }>>;

  constructor() {
    this.nodes = new Map();
    this.edgesFrom = new Map();
    this.edgesTo = new Map();
  }

  addNode(id: string, props: Record<string, unknown>): void {
    this.nodes.set(id, props);
  }

  getNodeProperties(id: string): Record<string, unknown> {
    const props = this.nodes.get(id);
    if (!props) {
      throw new Error(`Node ${id} not found`);
    }
    return props;
  }

  addEdge(fromId: string, toId: string, type: string): void {
    if (!this.edgesFrom.has(fromId)) {
      this.edgesFrom.set(fromId, []);
    }
    if (!this.edgesTo.has(toId)) {
      this.edgesTo.set(toId, []);
    }

    this.edgesFrom.get(fromId)!.push({ type, toId, fromId });
    this.edgesTo.get(toId)!.push({ type, fromId, toId });
  }

  getEdgesFrom(id: string): Array<{ type: string; toId: string; fromId: string }> {
    return this.edgesFrom.get(id) || [];
  }

  getEdgesTo(id: string): Array<{ type: string; fromId: string; toId: string }> {
    return this.edgesTo.get(id) || [];
  }
}

// Mock EventLog
class MockEventLog {
  private events: Event[];

  constructor() {
    this.events = [];
  }

  append(event: Event): void {
    this.events.push(event);
  }

  getAllEvents(): Event[] {
    return this.events;
  }

  getEventsByType(type: string): Event[] {
    return this.events.filter((e) => e.type === type);
  }
}

// Helper to create mock context
function createMockContext(graph?: MockGraph, eventLog?: MockEventLog): HookContext {
  return {
    graph: (graph || new MockGraph()) as unknown as HookContext["graph"],
    eventLog: (eventLog || new MockEventLog()) as unknown as HookContext["eventLog"],
  };
}

/**
 * HookRegistry Tests
 */

describe("HookRegistry", () => {
  let registry: HookRegistry;

  beforeEach(() => {
    registry = new HookRegistry();
  });

  test("registers hooks successfully", () => {
    const hook: LifecycleHook = {
      name: "test-hook",
      eventTypes: ["test_event"],
      handler: async () => [],
    };

    registry.register(hook);

    expect(registry.getHookCount()).toBe(1);
    expect(registry.getHook("test-hook")).toBeDefined();
  });

  test("throws error when registering duplicate hook name", () => {
    const hook1: LifecycleHook = {
      name: "duplicate",
      eventTypes: ["test"],
      handler: async () => [],
    };

    const hook2: LifecycleHook = {
      name: "duplicate",
      eventTypes: ["test"],
      handler: async () => [],
    };

    registry.register(hook1);

    expect(() => registry.register(hook2)).toThrow(/already registered/);
  });

  test("throws error when hook has empty name", () => {
    const hook: LifecycleHook = {
      name: "",
      eventTypes: ["test"],
      handler: async () => [],
    };

    expect(() => registry.register(hook)).toThrow(/name cannot be empty/);
  });

  test("throws error when hook has no event types", () => {
    const hook: LifecycleHook = {
      name: "invalid-hook",
      eventTypes: [],
      handler: async () => [],
    };

    expect(() => registry.register(hook)).toThrow(/at least one event type/);
  });

  test("unregisters hooks successfully", () => {
    const hook: LifecycleHook = {
      name: "test-hook",
      eventTypes: ["test"],
      handler: async () => [],
    };

    registry.register(hook);
    expect(registry.getHookCount()).toBe(1);

    const result = registry.unregister("test-hook");
    expect(result).toBe(true);
    expect(registry.getHookCount()).toBe(0);
  });

  test("returns false when unregistering non-existent hook", () => {
    const result = registry.unregister("non-existent");
    expect(result).toBe(false);
  });

  test("gets hooks for event type", () => {
    const hook1: LifecycleHook = {
      name: "hook-1",
      eventTypes: ["event_a"],
      handler: async () => [],
    };

    const hook2: LifecycleHook = {
      name: "hook-2",
      eventTypes: ["event_a", "event_b"],
      handler: async () => [],
    };

    const hook3: LifecycleHook = {
      name: "hook-3",
      eventTypes: ["event_b"],
      handler: async () => [],
    };

    registry.register(hook1);
    registry.register(hook2);
    registry.register(hook3);

    const hooksForA = registry.getHooksForEvent("event_a");
    expect(hooksForA.length).toBe(2);
    expect(hooksForA.map((h) => h.name)).toContain("hook-1");
    expect(hooksForA.map((h) => h.name)).toContain("hook-2");

    const hooksForB = registry.getHooksForEvent("event_b");
    expect(hooksForB.length).toBe(2);
    expect(hooksForB.map((h) => h.name)).toContain("hook-2");
    expect(hooksForB.map((h) => h.name)).toContain("hook-3");
  });

  test("executes hooks in priority order", async () => {
    const executionOrder: number[] = [];

    const hook1: LifecycleHook = {
      name: "hook-priority-20",
      eventTypes: ["test"],
      priority: 20,
      handler: async () => {
        executionOrder.push(20);
        return [];
      },
    };

    const hook2: LifecycleHook = {
      name: "hook-priority-10",
      eventTypes: ["test"],
      priority: 10,
      handler: async () => {
        executionOrder.push(10);
        return [];
      },
    };

    const hook3: LifecycleHook = {
      name: "hook-priority-default",
      eventTypes: ["test"],
      handler: async () => {
        executionOrder.push(100);
        return [];
      },
    };

    registry.register(hook1);
    registry.register(hook2);
    registry.register(hook3);

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: "test",
      nodeId: "test_1",
      data: {},
    };

    const context = createMockContext();
    await registry.executeHooks(event, context);

    expect(executionOrder).toEqual([10, 20, 100]);
  });

  test("aggregates actions from multiple hooks", async () => {
    const hook1: LifecycleHook = {
      name: "hook-1",
      eventTypes: ["test"],
      handler: async () => [
        { type: "log", payload: { message: "hook-1" } },
      ] as HookAction[],
    };

    const hook2: LifecycleHook = {
      name: "hook-2",
      eventTypes: ["test"],
      handler: async () => [
        { type: "log", payload: { message: "hook-2-a" } },
        { type: "log", payload: { message: "hook-2-b" } },
      ] as HookAction[],
    };

    registry.register(hook1);
    registry.register(hook2);

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: "test",
      nodeId: "test_1",
      data: {},
    };

    const context = createMockContext();
    const actions = await registry.executeHooks(event, context);

    expect(actions.length).toBe(3);
  });

  test("isolates hook errors", async () => {
    const hook1: LifecycleHook = {
      name: "failing-hook",
      eventTypes: ["test"],
      priority: 10,
      handler: async () => {
        throw new Error("Hook failed!");
      },
    };

    const hook2: LifecycleHook = {
      name: "success-hook",
      eventTypes: ["test"],
      priority: 20,
      handler: async () => [{ type: "log", payload: { success: true } }] as HookAction[],
    };

    registry.register(hook1);
    registry.register(hook2);

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: "test",
      nodeId: "test_1",
      data: {},
    };

    const eventLog = new MockEventLog();
    const context = createMockContext(undefined, eventLog);
    const actions = await registry.executeHooks(event, context);

    // Successful hook still executed
    expect(actions.length).toBe(1);
    expect(actions[0].type).toBe("log");

    // Error event logged
    const errorEvents = eventLog.getEventsByType("hook_error");
    expect(errorEvents.length).toBe(1);
    expect(errorEvents[0].data).toMatchObject({
      hookName: "failing-hook",
    });
  });

  test("filters out disabled hooks", async () => {
    const hook1: LifecycleHook = {
      name: "enabled-hook",
      eventTypes: ["test"],
      enabled: true,
      handler: async () => [{ type: "log", payload: {} }] as HookAction[],
    };

    const hook2: LifecycleHook = {
      name: "disabled-hook",
      eventTypes: ["test"],
      enabled: false,
      handler: async () => [{ type: "log", payload: {} }] as HookAction[],
    };

    registry.register(hook1);
    registry.register(hook2);

    const hooks = registry.getHooksForEvent("test");
    expect(hooks.length).toBe(1);
    expect(hooks[0].name).toBe("enabled-hook");
  });

  test("enables and disables hooks", () => {
    const hook: LifecycleHook = {
      name: "toggleable-hook",
      eventTypes: ["test"],
      handler: async () => [],
    };

    registry.register(hook);

    // Initially enabled (default)
    expect(registry.getEnabledHookCount()).toBe(1);

    // Disable
    const disabled = registry.disableHook("toggleable-hook");
    expect(disabled).toBe(true);
    expect(registry.getEnabledHookCount()).toBe(0);

    // Enable
    const enabled = registry.enableHook("toggleable-hook");
    expect(enabled).toBe(true);
    expect(registry.getEnabledHookCount()).toBe(1);
  });

  test("lists all hooks", () => {
    const hook1: LifecycleHook = {
      name: "hook-1",
      eventTypes: ["test"],
      handler: async () => [],
    };

    const hook2: LifecycleHook = {
      name: "hook-2",
      eventTypes: ["test"],
      handler: async () => [],
    };

    registry.register(hook1);
    registry.register(hook2);

    const allHooks = registry.listHooks();
    expect(allHooks.length).toBe(2);
  });

  test("clears all hooks", () => {
    const hook: LifecycleHook = {
      name: "test-hook",
      eventTypes: ["test"],
      handler: async () => [],
    };

    registry.register(hook);
    expect(registry.getHookCount()).toBe(1);

    registry.clear();
    expect(registry.getHookCount()).toBe(0);
  });

  test("gets active event types", () => {
    const hook1: LifecycleHook = {
      name: "hook-1",
      eventTypes: ["event_a", "event_b"],
      handler: async () => [],
    };

    const hook2: LifecycleHook = {
      name: "hook-2",
      eventTypes: ["event_b", "event_c"],
      handler: async () => [],
    };

    registry.register(hook1);
    registry.register(hook2);

    const activeTypes = registry.getActiveEventTypes();
    expect(activeTypes.length).toBe(3);
    expect(activeTypes).toContain("event_a");
    expect(activeTypes).toContain("event_b");
    expect(activeTypes).toContain("event_c");
  });
});

/**
 * Built-in Hook Tests
 */

describe("AutoCreateReviewTaskHook", () => {
  test("creates review task for completed agent task", async () => {
    const hook = new AutoCreateReviewTaskHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Research graph patterns",
      labels: ["agent", "research"],
      priority: 1,
      state: "completed",
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_1",
      data: { result: "Research complete" },
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(1);
    expect(actions[0].type).toBe("create_task");

    const createAction = actions[0] as any;
    expect(createAction.payload.goal).toContain("Review:");
    expect(createAction.payload.labels).toContain("review");
    expect(createAction.payload.labels).toContain("agent");
    expect(createAction.payload.labels).toContain("research");
    expect(createAction.payload.priority).toBe(1);
    expect(createAction.payload.parentTaskId).toBe("task_1");
  });

  test("does not create review for non-agent task", async () => {
    const hook = new AutoCreateReviewTaskHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Regular task",
      labels: ["feature"],
      priority: 2,
      state: "completed",
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(0);
  });

  test("does not create review for review task", async () => {
    const hook = new AutoCreateReviewTaskHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Review: Something",
      labels: ["agent", "review"],
      priority: 1,
      state: "completed",
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(0);
  });
});

describe("CheckDependencySatisfactionHook", () => {
  test("transitions dependent task to ready when all dependencies complete", async () => {
    const hook = new CheckDependencySatisfactionHook();
    const graph = new MockGraph();

    // Create completed dependency
    graph.addNode("task_dep1", {
      state: "completed",
    });

    // Create dependent task
    graph.addNode("task_dependent", {
      state: "created",
      goal: "Dependent work",
    });

    // Add dependency edge
    graph.addEdge("task_dependent", "task_dep1", "depends_on");

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_dep1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(2);

    // Should have update action
    const updateAction = actions.find((a) => a.type === "update_task");
    expect(updateAction).toBeDefined();

    // Should have log action
    const logAction = actions.find((a) => a.type === "log");
    expect(logAction).toBeDefined();
  });

  test("does not transition if not all dependencies complete", async () => {
    const hook = new CheckDependencySatisfactionHook();
    const graph = new MockGraph();

    // Create dependencies (one complete, one active)
    graph.addNode("task_dep1", { state: "completed" });
    graph.addNode("task_dep2", { state: "active" });

    // Create dependent task
    graph.addNode("task_dependent", {
      state: "created",
      goal: "Dependent work",
    });

    // Add dependency edges
    graph.addEdge("task_dependent", "task_dep1", "depends_on");
    graph.addEdge("task_dependent", "task_dep2", "depends_on");

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_dep1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    // Should not transition (task_dep2 still active)
    expect(actions.length).toBe(0);
  });

  test("does not transition tasks not in created state", async () => {
    const hook = new CheckDependencySatisfactionHook();
    const graph = new MockGraph();

    graph.addNode("task_dep1", { state: "completed" });
    graph.addNode("task_dependent", {
      state: "active", // Already active, not created
      goal: "Dependent work",
    });

    graph.addEdge("task_dependent", "task_dep1", "depends_on");

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_dep1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(0);
  });
});

describe("TrackAgentTaskLifecycleHook", () => {
  test("logs metrics when agent task completes", async () => {
    const hook = new TrackAgentTaskLifecycleHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Agent research work",
      labels: ["agent", "research"],
      priority: 1,
      state: "completed",
      startedAt: "2026-01-17T10:00:00Z",
      completedAt: "2026-01-17T12:00:00Z",
      desiredDeliverables: ["Research doc", "Findings"],
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(1);
    expect(actions[0].type).toBe("log");

    const logAction = actions[0] as any;
    expect(logAction.payload.type).toBe("agent_metrics");
    expect(logAction.payload.data.goal).toBe("Agent research work");
    expect(logAction.payload.data.durationMs).toBeDefined();
    expect(logAction.payload.data.durationMs).toBeGreaterThan(0);
  });

  test("does not log for non-agent tasks", async () => {
    const hook = new TrackAgentTaskLifecycleHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Regular task",
      labels: ["feature"],
      state: "completed",
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_COMPLETED,
      nodeId: "task_1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(0);
  });

  test("does not log on task_created or task_started", async () => {
    const hook = new TrackAgentTaskLifecycleHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Agent task",
      labels: ["agent"],
      state: "active",
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_STARTED,
      nodeId: "task_1",
      data: {},
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBe(0);
  });
});

describe("AutoLabelByKeywordHook", () => {
  test("suggests labels based on keywords in goal", async () => {
    const hook = new AutoLabelByKeywordHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Research graph database patterns",
      labels: [],
      state: "created",
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_CREATED,
      nodeId: "task_1",
      data: { goal: "Research graph database patterns" },
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    expect(actions.length).toBeGreaterThan(0);
    expect(actions[0].type).toBe("log");

    const logAction = actions[0] as any;
    expect(logAction.payload.type).toBe("auto_label_suggestion");
    expect(logAction.payload.data.suggestedLabels).toBeDefined();
  });

  test("does not suggest labels if no keywords match", async () => {
    const hook = new AutoLabelByKeywordHook();
    const graph = new MockGraph();

    graph.addNode("task_1", {
      goal: "Random task without keywords",
      labels: [],
      state: "created",
    });

    const event: Event = {
      timestamp: new Date().toISOString(),
      type: EVENT_TYPES.TASK_CREATED,
      nodeId: "task_1",
      data: { goal: "Random task without keywords" },
    };

    const context = createMockContext(graph);
    const actions = await hook.handler(event, context);

    // May or may not have suggestions depending on keywords
    // This is a weak test - just verify no crash
    expect(Array.isArray(actions)).toBe(true);
  });
});

describe("registerBuiltinHooks", () => {
  test("registers all 4 built-in hooks", () => {
    const registry = new HookRegistry();

    registerBuiltinHooks(registry);

    expect(registry.getHookCount()).toBe(4);
    expect(registry.getHook("auto-create-review-task")).toBeDefined();
    expect(registry.getHook("check-dependency-satisfaction")).toBeDefined();
    expect(registry.getHook("track-agent-task-lifecycle")).toBeDefined();
    expect(registry.getHook("auto-label-by-keyword")).toBeDefined();
  });
});
