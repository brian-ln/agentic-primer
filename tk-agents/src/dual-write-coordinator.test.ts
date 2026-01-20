/**
 * Dual-Write Coordinator Tests
 *
 * Tests the synchronization across Graph, EventLog, and CozoDB.
 */

import { test, expect, describe, beforeEach, afterEach } from "bun:test";
import { DualWriteCoordinator, TaskQueryService, CozoReconciler } from "./dual-write-coordinator";
import { Graph } from "./graph";
import { EventLog } from "./persistence/event-log";
import { CozoClient } from "./cozo-client";
import { unlinkSync, existsSync } from "node:fs";

describe("DualWriteCoordinator", () => {
  let graph: Graph;
  let eventLog: EventLog;
  let cozoDB: CozoClient;
  let coordinator: DualWriteCoordinator;
  let queryService: TaskQueryService;

  const testLogPath = "./test-dual-write.jsonl";
  const testDbUrl = "http://127.0.0.1:9070"; // Assumes CozoDB server running

  beforeEach(async () => {
    // Clean up test files
    if (existsSync(testLogPath)) {
      unlinkSync(testLogPath);
    }

    // Initialize components
    graph = new Graph();
    eventLog = new EventLog(testLogPath);
    cozoDB = new CozoClient(testDbUrl);
    coordinator = new DualWriteCoordinator(graph, eventLog, cozoDB);
    queryService = new TaskQueryService(cozoDB);

    // Initialize CozoDB schema using new migration-based approach
    try {
      await coordinator.initialize();
    } catch (error) {
      // If migrations already applied, just clear data
      await cozoDB.run(`:clear work`);
      await cozoDB.run(`:clear dependencies`);
      await cozoDB.run(`:clear task_labels`);
    }
  });

  afterEach(async () => {
    // Clean up test files
    if (existsSync(testLogPath)) {
      unlinkSync(testLogPath);
    }

    // Clear CozoDB
    try {
      await cozoDB.run(`:clear work`);
      await cozoDB.run(`:clear dependencies`);
      await cozoDB.run(`:clear task_labels`);
    } catch (error) {
      // Ignore errors during cleanup
    }
  });

  describe("Schema Integration", () => {
    test("initialize() runs migrations", async () => {
      // Coordinator should already be initialized from beforeEach
      const validation = await coordinator.validateSchema();
      expect(validation.valid).toBe(true);
      expect(validation.errors.length).toBe(0);
    });

    test("validateSchema() detects missing relations", async () => {
      // Create a fresh coordinator without calling initialize
      const freshGraph = new Graph();
      const freshLog = new EventLog("./test-fresh.jsonl");
      const freshCozo = new CozoClient(testDbUrl);
      const freshCoordinator = new DualWriteCoordinator(freshGraph, freshLog, freshCozo);

      // Clear all relations to simulate missing schema
      await freshCozo.run(`:remove work`).catch(() => {});
      await freshCozo.run(`:remove dependencies`).catch(() => {});
      await freshCozo.run(`:remove task_labels`).catch(() => {});

      const validation = await freshCoordinator.validateSchema();
      expect(validation.valid).toBe(false);
      expect(validation.errors.length).toBeGreaterThan(0);

      // Clean up
      if (existsSync("./test-fresh.jsonl")) {
        unlinkSync("./test-fresh.jsonl");
      }
    });
  });

  describe("createTask", () => {
    test("writes to all three systems", async () => {
      const taskId = await coordinator.createTask({
        goal: "Test task",
        desiredDeliverables: ["Test deliverable"],
        objectiveSuccessCriteria: [
          { criterion: "Test passes", measure: "count", threshold: 1 },
        ],
        priority: 1,
        labels: ["test"],
      });

      // Verify Graph
      const graphProps = graph.getNodeProperties(taskId);
      expect(graphProps).toBeDefined();
      expect(graphProps?.goal).toBe("Test task");

      // Verify EventLog
      const events = eventLog.getEventsByNode(taskId);
      expect(events.length).toBeGreaterThanOrEqual(1);
      expect(events[0].type).toBe("task_created");

      // Verify CozoDB
      const cozoTask = await queryService.getTask(taskId);
      expect(cozoTask).toBeDefined();
      expect(cozoTask?.title).toBe("Test task");
      expect(cozoTask?.status).toBe("created");
    });

    test("rolls back Graph if EventLog fails", async () => {
      // Create a coordinator with a broken EventLog
      const brokenLog = new EventLog("/invalid/path/file.jsonl");
      const brokenCoordinator = new DualWriteCoordinator(graph, brokenLog, cozoDB);

      await expect(
        brokenCoordinator.createTask({
          goal: "Should fail",
          desiredDeliverables: ["Test"],
          objectiveSuccessCriteria: [],
        })
      ).rejects.toThrow();

      // Verify Graph rollback - no nodes should exist
      expect(graph.getNodeIds().length).toBe(0);
    });

    test("continues if CozoDB fails (logged to EventLog)", async () => {
      // Create a coordinator with broken CozoDB (wrong port)
      const brokenCozoDB = new CozoClient("http://127.0.0.1:9999");
      const partialCoordinator = new DualWriteCoordinator(graph, eventLog, brokenCozoDB);

      // Should succeed despite CozoDB failure
      const taskId = await partialCoordinator.createTask({
        goal: "Test task with CozoDB failure",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      // Verify Graph succeeded
      expect(graph.getNodeProperties(taskId)).toBeDefined();

      // Verify EventLog has both create and failure events
      const events = eventLog.getEventsByNode(taskId);
      expect(events.length).toBeGreaterThanOrEqual(2);
      expect(events[0].type).toBe("task_created");
      expect(events.some((e) => e.type === "cozo_write_failed")).toBe(true);
    });
  });

  describe("updateTask", () => {
    test("updates state in all three systems", async () => {
      const taskId = await coordinator.createTask({
        goal: "Test update",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      // Start the task
      await coordinator.updateTask(taskId, "start");

      // Verify Graph
      const graphProps = graph.getNodeProperties(taskId);
      expect(graphProps?.state).toBe("active");

      // Verify EventLog
      const events = eventLog.getEventsByNode(taskId);
      const startEvent = events.find((e) => e.type === "task_start");
      expect(startEvent).toBeDefined();
      expect(startEvent?.data).toHaveProperty("newState", "active");

      // Verify CozoDB
      const cozoTask = await queryService.getTask(taskId);
      expect(cozoTask?.status).toBe("active");
    });

    test("handles complete action", async () => {
      const taskId = await coordinator.createTask({
        goal: "Test complete",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      await coordinator.updateTask(taskId, "start");
      await coordinator.updateTask(taskId, "complete", { result: "success" });

      const graphProps = graph.getNodeProperties(taskId);
      expect(graphProps?.state).toBe("completed");

      const cozoTask = await queryService.getTask(taskId);
      expect(cozoTask?.status).toBe("completed");
    });
  });

  describe("addDependency", () => {
    test("creates dependency in all three systems", async () => {
      const task1 = await coordinator.createTask({
        goal: "Task 1",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task2 = await coordinator.createTask({
        goal: "Task 2",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const edgeId = await coordinator.addDependency(task1, task2);

      // Verify Graph
      const edges = graph.getEdgesFrom(task1);
      expect(edges.length).toBe(1);
      expect(edges[0].toId).toBe(task2);

      // Verify EventLog
      const events = eventLog.getEventsByType("edge_created");
      expect(events.length).toBeGreaterThanOrEqual(1);

      // Verify CozoDB - task1 should be blocked
      const blockedTasks = await queryService.findBlockedTasks();
      const blocked = blockedTasks.find((t) => t.id === task1);
      expect(blocked).toBeDefined();
      expect(blocked?.blockers[0].id).toBe(task2);
    });

    test("validates task existence", async () => {
      await expect(
        coordinator.addDependency("invalid_task", "another_invalid")
      ).rejects.toThrow("Task not found");
    });
  });

  describe("deleteTask", () => {
    test("removes task from all three systems", async () => {
      const taskId = await coordinator.createTask({
        goal: "Task to delete",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      await coordinator.deleteTask(taskId);

      // Verify Graph
      expect(graph.getNodeProperties(taskId)).toBeUndefined();

      // Verify EventLog
      const events = eventLog.getEventsByNode(taskId);
      const deleteEvent = events.find((e) => e.type === "task_deleted");
      expect(deleteEvent).toBeDefined();

      // Verify CozoDB
      const cozoTask = await queryService.getTask(taskId);
      expect(cozoTask).toBeNull();
    });

    test("removes connected edges", async () => {
      const task1 = await coordinator.createTask({
        goal: "Task 1",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task2 = await coordinator.createTask({
        goal: "Task 2",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      await coordinator.addDependency(task1, task2);
      await coordinator.deleteTask(task1);

      // Verify edges removed from Graph
      expect(graph.getAllEdges(task1).length).toBe(0);

      // Verify edges removed from CozoDB
      const blockedTasks = await queryService.findBlockedTasks();
      expect(blockedTasks.find((t) => t.id === task1)).toBeUndefined();
    });
  });

  describe("TaskQueryService", () => {
    test("findReadyTasks returns tasks without blockers", async () => {
      const task1 = await coordinator.createTask({
        goal: "Task 1",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task2 = await coordinator.createTask({
        goal: "Task 2",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task3 = await coordinator.createTask({
        goal: "Task 3",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      // task1 depends on task2
      await coordinator.addDependency(task1, task2);

      const readyTasks = await queryService.findReadyTasks();

      // task2 and task3 should be ready (no blockers)
      expect(readyTasks.length).toBeGreaterThanOrEqual(2);
      expect(readyTasks.some((t) => t.id === task2)).toBe(true);
      expect(readyTasks.some((t) => t.id === task3)).toBe(true);

      // task1 should NOT be ready (blocked by task2)
      expect(readyTasks.some((t) => t.id === task1)).toBe(false);
    });

    test("findBlockedTasks identifies blockers", async () => {
      const task1 = await coordinator.createTask({
        goal: "Task 1",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task2 = await coordinator.createTask({
        goal: "Task 2",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      await coordinator.addDependency(task1, task2);

      const blockedTasks = await queryService.findBlockedTasks();
      const blocked = blockedTasks.find((t) => t.id === task1);

      expect(blocked).toBeDefined();
      expect(blocked?.blockers.length).toBe(1);
      expect(blocked?.blockers[0].id).toBe(task2);
    });

    test("getDependencyGraph returns transitive dependencies", async () => {
      const task1 = await coordinator.createTask({
        goal: "Task 1",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task2 = await coordinator.createTask({
        goal: "Task 2",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task3 = await coordinator.createTask({
        goal: "Task 3",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      // task1 -> task2 -> task3
      await coordinator.addDependency(task1, task2);
      await coordinator.addDependency(task2, task3);

      const depGraph = await queryService.getDependencyGraph(task1);

      // Should include both task2 and task3 (transitive closure)
      expect(depGraph.length).toBeGreaterThanOrEqual(2);
      expect(depGraph.some((d) => d.depId === task2)).toBe(true);
      expect(depGraph.some((d) => d.depId === task3)).toBe(true);
    });

    test("searchTasks finds tasks by keyword", async () => {
      await coordinator.createTask({
        goal: "Implement authentication",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      await coordinator.createTask({
        goal: "Write tests",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const results = await queryService.searchTasks("auth");
      expect(results.length).toBeGreaterThanOrEqual(1);
      expect(results[0].title.toLowerCase()).toContain("auth");
    });
  });

  describe("CozoReconciler", () => {
    test("rebuildFromEventLog reconstructs CozoDB from events", async () => {
      // Create some tasks
      const task1 = await coordinator.createTask({
        goal: "Task 1",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      const task2 = await coordinator.createTask({
        goal: "Task 2",
        desiredDeliverables: ["Test"],
        objectiveSuccessCriteria: [],
      });

      await coordinator.addDependency(task1, task2);
      await coordinator.updateTask(task1, "start");

      // Clear CozoDB to simulate corruption
      await cozoDB.run(`:clear work`);
      await cozoDB.run(`:clear dependencies`);

      // Verify CozoDB is empty
      let tasks = await queryService.findReadyTasks();
      expect(tasks.length).toBe(0);

      // Rebuild from EventLog
      const reconciler = new CozoReconciler(eventLog, cozoDB);
      await reconciler.rebuildFromEventLog();

      // Verify CozoDB is restored
      tasks = await queryService.findReadyTasks();
      expect(tasks.length).toBeGreaterThanOrEqual(1);

      const task1Cozo = await queryService.getTask(task1);
      expect(task1Cozo).toBeDefined();
      expect(task1Cozo?.status).toBe("active"); // Should reflect the start update

      const blockedTasks = await queryService.findBlockedTasks();
      const blocked = blockedTasks.find((t) => t.id === task1);
      expect(blocked).toBeDefined(); // Dependency should be restored
    });
  });

  describe("Integration: Full workflow", () => {
    test("create → query → update → query flow", async () => {
      // Create tasks with dependencies
      const taskA = await coordinator.createTask({
        goal: "Implement feature A",
        desiredDeliverables: ["Feature A completed"],
        objectiveSuccessCriteria: [
          { criterion: "Tests passing", measure: "count", threshold: 10 },
        ],
        priority: 1,
        labels: ["feature"],
      });

      const taskB = await coordinator.createTask({
        goal: "Write tests for feature A",
        desiredDeliverables: ["Tests written"],
        objectiveSuccessCriteria: [
          { criterion: "Coverage", measure: "percent", threshold: 80 },
        ],
        priority: 2,
        labels: ["testing"],
      });

      // taskA depends on taskB
      await coordinator.addDependency(taskA, taskB);

      // Query ready tasks - only taskB should be ready
      let readyTasks = await queryService.findReadyTasks();
      expect(readyTasks.some((t) => t.id === taskB)).toBe(true);
      expect(readyTasks.some((t) => t.id === taskA)).toBe(false);

      // Start and complete taskB
      await coordinator.updateTask(taskB, "start");
      await coordinator.updateTask(taskB, "complete", { result: "Tests completed" });

      // Query ready tasks again - now taskA should be ready
      readyTasks = await queryService.findReadyTasks();
      expect(readyTasks.some((t) => t.id === taskA)).toBe(true);

      // Verify EventLog has complete history
      const allEvents = eventLog.getAllEvents();
      expect(allEvents.length).toBeGreaterThanOrEqual(7); // 2 creates + 1 edge + 2 starts + 2 completes
    });
  });
});
