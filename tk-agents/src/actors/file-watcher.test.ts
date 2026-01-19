/**
 * File Watcher Actor Tests
 *
 * Tests for FileWatcherActor hot reload functionality
 */

import { describe, test, expect, beforeEach, afterEach } from "bun:test";
import { FileWatcherActor } from "./file-watcher-actor";
import { Graph } from "../graph";
import { ActorSystem } from "./actor-system";
import { existsSync, unlinkSync, writeFileSync, appendFileSync } from "node:fs";

describe("FileWatcherActor", () => {
  const testFilePath = "./test-watched-file.jsonl";
  let graph: Graph;
  let actorSystem: ActorSystem;
  let watcher: FileWatcherActor;

  beforeEach(() => {
    // Clean up test file
    if (existsSync(testFilePath)) {
      unlinkSync(testFilePath);
    }

    // Create test file
    writeFileSync(testFilePath, '{"type":"initial","timestamp":"2026-01-17T00:00:00.000Z"}\n');

    // Initialize dependencies
    graph = new Graph();
    actorSystem = new ActorSystem();

    // Create watcher
    watcher = new FileWatcherActor({
      id: "fw_test_1",
      filePath: testFilePath,
      fileType: "session",
      graph,
      actorSystem,
    });

    // Register watcher in graph (required for updateNode to work)
    graph.registerNode(
      "fw_test_1",
      {} as any, // Dummy address - not used in file watcher tests
      {
        id: "fw_test_1",
        type: "file_watcher",
        filePath: testFilePath,
        fileType: "session",
        status: "stopped",
        createdAt: new Date().toISOString(),
        lastActivity: new Date().toISOString(),
        eventCount: 0,
        subscriberCount: 0,
        actorAddress: "fw_test_1",
      } as any
    );

    // Register watcher in actor system
    actorSystem.register("fw_test_1", async (msg) => watcher.handleMessage(msg));
  });

  afterEach(async () => {
    // Stop watching if active
    if (watcher.getStatus() === "watching") {
      await watcher.handleMessage({ type: "unwatch", payload: {} });
    }

    // Clean up test file
    if (existsSync(testFilePath)) {
      unlinkSync(testFilePath);
    }
  });

  test("starts watching file successfully", async () => {
    const response = await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
      },
    });

    expect(response.success).toBe(true);
    expect(response.status).toBe("watching");
    expect(response.watcherId).toBe("fw_test_1");
    expect(watcher.getStatus()).toBe("watching");
  });

  test("detects file changes and broadcasts events", async () => {
    // Start watching
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
      },
    });

    // Create subscriber
    const receivedEvents: any[] = [];
    actorSystem.register("subscriber_1", async (msg) => {
      if (msg.type === "file_event") {
        receivedEvents.push(msg.payload.event);
      }
      return { success: true };
    });

    // Subscribe to events
    await watcher.handleMessage({
      type: "subscribe",
      payload: {
        subscriberId: "subscriber_1",
      },
    });

    // Modify file
    await new Promise((resolve) => setTimeout(resolve, 50)); // Wait for watcher to settle
    appendFileSync(testFilePath, '{"type":"test_event","timestamp":"2026-01-17T00:01:00.000Z"}\n');

    // Wait for debounce and event processing
    await new Promise((resolve) => setTimeout(resolve, 500));

    // Verify event was broadcast
    expect(receivedEvents.length).toBeGreaterThan(0);
    const event = receivedEvents[0];
    expect(event.eventType).toBe("file_change");
    expect(event.watcherId).toBe("fw_test_1");
    expect(event.changes.bytesAdded).toBeGreaterThan(0);
  });

  test("debounces rapid file changes", async () => {
    // Start watching with custom debounce
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
        options: {
          debounceMs: 200,
        },
      },
    });

    // Subscribe
    const receivedEvents: any[] = [];
    actorSystem.register("subscriber_2", async (msg) => {
      if (msg.type === "file_event") {
        receivedEvents.push(msg.payload.event);
      }
      return { success: true };
    });

    await watcher.handleMessage({
      type: "subscribe",
      payload: {
        subscriberId: "subscriber_2",
      },
    });

    // Make rapid changes
    await new Promise((resolve) => setTimeout(resolve, 50));
    appendFileSync(testFilePath, '{"type":"change1"}\n');
    await new Promise((resolve) => setTimeout(resolve, 50));
    appendFileSync(testFilePath, '{"type":"change2"}\n');
    await new Promise((resolve) => setTimeout(resolve, 50));
    appendFileSync(testFilePath, '{"type":"change3"}\n');

    // Wait for debounce to settle
    await new Promise((resolve) => setTimeout(resolve, 500));

    // Should have debounced to fewer events than changes
    expect(receivedEvents.length).toBeLessThan(3);
    expect(receivedEvents.length).toBeGreaterThan(0);
  });

  test("suspends and resumes watching", async () => {
    // Start watching
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
      },
    });

    expect(watcher.getStatus()).toBe("watching");

    // Suspend
    const suspendResponse = await watcher.handleMessage({
      type: "suspend",
      payload: {},
    });

    expect(suspendResponse.success).toBe(true);
    expect(suspendResponse.status).toBe("suspended");
    expect(watcher.getStatus()).toBe("suspended");

    // Resume
    const resumeResponse = await watcher.handleMessage({
      type: "resume",
      payload: {},
    });

    expect(resumeResponse.success).toBe(true);
    expect(resumeResponse.status).toBe("watching");
    expect(watcher.getStatus()).toBe("watching");
  });

  test("searches event history", async () => {
    // Start watching
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
      },
    });

    // Wait and add some changes
    await new Promise((resolve) => setTimeout(resolve, 50));
    appendFileSync(testFilePath, '{"type":"llm_response","timestamp":"2026-01-17T00:01:00.000Z"}\n');
    await new Promise((resolve) => setTimeout(resolve, 300));
    appendFileSync(testFilePath, '{"type":"tool_result","timestamp":"2026-01-17T00:02:00.000Z"}\n');
    await new Promise((resolve) => setTimeout(resolve, 300));

    // Search for specific event type
    const searchResponse = await watcher.handleMessage({
      type: "search",
      payload: {
        query: {
          eventType: ["file_change"],
          limit: 10,
        },
      },
    });

    expect(searchResponse.success).toBe(true);
    expect(Array.isArray(searchResponse.events)).toBe(true);
    expect(searchResponse.totalCount).toBeGreaterThan(0);
  });

  test("handles file not found error", async () => {
    const nonExistentPath = "./non-existent-file.jsonl";

    const response = await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: nonExistentPath,
        fileType: "session",
      },
    });

    expect(response.success).toBe(false);
    expect(response.error).toContain("not found");
  });

  test("unsubscribes correctly", async () => {
    // Start watching
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
      },
    });

    // Subscribe
    const subResponse = await watcher.handleMessage({
      type: "subscribe",
      payload: {
        subscriberId: "subscriber_3",
      },
    });

    expect(subResponse.subscriberCount).toBe(1);

    // Unsubscribe
    const unsubResponse = await watcher.handleMessage({
      type: "unsubscribe",
      payload: {
        subscriptionId: subResponse.subscriptionId,
      },
    });

    expect(unsubResponse.success).toBe(true);
    expect(unsubResponse.subscriberCount).toBe(0);
  });

  test("emits initial event when requested", async () => {
    // Create subscriber first
    const receivedEvents: any[] = [];
    actorSystem.register("subscriber_initial", async (msg) => {
      if (msg.type === "file_event") {
        receivedEvents.push(msg.payload.event);
      }
      return { success: true };
    });

    // Subscribe before watching
    await watcher.handleMessage({
      type: "subscribe",
      payload: {
        subscriberId: "subscriber_initial",
      },
    });

    // Start watching with includeInitial
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
        options: {
          includeInitial: true,
        },
      },
    });

    // Wait for initial event
    await new Promise((resolve) => setTimeout(resolve, 200));

    // Should have received initial event
    expect(receivedEvents.length).toBeGreaterThan(0);
    const initialEvent = receivedEvents[0];
    expect(initialEvent.eventType).toBe("file_created");
  });

  test("handles unwatching correctly", async () => {
    // Start watching
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: testFilePath,
        fileType: "session",
      },
    });

    expect(watcher.getStatus()).toBe("watching");

    // Unwatch
    const unwatchResponse = await watcher.handleMessage({
      type: "unwatch",
      payload: {},
    });

    expect(unwatchResponse.success).toBe(true);
    expect(unwatchResponse.status).toBe("stopped");
    expect(watcher.getStatus()).toBe("stopped");
  });
});

describe("FileWatcherActor - Daemon Integration", () => {
  const tasksFilePath = "./test-tasks-daemon.json";
  let graph: Graph;
  let actorSystem: ActorSystem;
  let watcher: FileWatcherActor;

  beforeEach(() => {
    // Clean up test file
    if (existsSync(tasksFilePath)) {
      unlinkSync(tasksFilePath);
    }

    // Create test tasks.json
    writeFileSync(
      tasksFilePath,
      JSON.stringify(
        {
          nodes: [
            {
              id: "task_1",
              type: "task",
              goal: "Test task",
              state: "created",
            },
          ],
          edges: [],
        },
        null,
        2
      )
    );

    // Initialize dependencies
    graph = new Graph();
    actorSystem = new ActorSystem();

    // Create watcher
    watcher = new FileWatcherActor({
      id: "fw_daemon_1",
      filePath: tasksFilePath,
      fileType: "task_graph",
      graph,
      actorSystem,
    });

    // Register watcher in graph
    graph.registerNode(
      "fw_daemon_1",
      {} as any,
      {
        id: "fw_daemon_1",
        type: "file_watcher",
        filePath: tasksFilePath,
        fileType: "task_graph",
        status: "stopped",
        createdAt: new Date().toISOString(),
        lastActivity: new Date().toISOString(),
        eventCount: 0,
        subscriberCount: 0,
        actorAddress: "fw_daemon_1",
      } as any
    );

    // Register watcher in actor system
    actorSystem.register("fw_daemon_1", async (msg) => watcher.handleMessage(msg));
  });

  afterEach(async () => {
    // Stop watching if active
    if (watcher.getStatus() === "watching") {
      await watcher.handleMessage({ type: "unwatch", payload: {} });
    }

    // Clean up test file
    if (existsSync(tasksFilePath)) {
      unlinkSync(tasksFilePath);
    }
  });

  test("detects tasks.json changes and triggers reload", async () => {
    // Start watching
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: tasksFilePath,
        fileType: "task_graph",
        options: {
          debounceMs: 200,
        },
      },
    });

    // Track reload events
    const reloadEvents: any[] = [];
    actorSystem.register("daemon_reload_handler", async (msg) => {
      if (msg.type === "file_event") {
        reloadEvents.push(msg.payload.event);
      }
      return { success: true };
    });

    await watcher.handleMessage({
      type: "subscribe",
      payload: {
        subscriberId: "daemon_reload_handler",
      },
    });

    // Simulate CLI adding a task (modify tasks.json)
    await new Promise((resolve) => setTimeout(resolve, 50));
    writeFileSync(
      tasksFilePath,
      JSON.stringify(
        {
          nodes: [
            {
              id: "task_1",
              type: "task",
              goal: "Test task",
              state: "created",
            },
            {
              id: "task_2",
              type: "task",
              goal: "New task from CLI",
              state: "created",
            },
          ],
          edges: [],
        },
        null,
        2
      )
    );

    // Wait for debounce and event processing
    await new Promise((resolve) => setTimeout(resolve, 500));

    // Verify reload event was triggered
    expect(reloadEvents.length).toBeGreaterThan(0);
    const reloadEvent = reloadEvents[0];
    expect(reloadEvent.eventType).toBe("file_change");
    expect(reloadEvent.fileType).toBe("task_graph");
    expect(reloadEvent.changes.bytesAdded).toBeGreaterThan(0);
  });

  test("broadcasts WebSocket update on file change", async () => {
    // Start watching
    await watcher.handleMessage({
      type: "watch",
      payload: {
        filePath: tasksFilePath,
        fileType: "task_graph",
        options: {
          debounceMs: 200,
        },
      },
    });

    // Track WebSocket broadcasts
    const broadcasts: any[] = [];
    actorSystem.register("ws_broadcast_handler", async (msg) => {
      if (msg.type === "file_event") {
        broadcasts.push({
          type: "graph_reloaded",
          data: {
            timestamp: msg.payload.event.timestamp,
            source: "file_watcher",
          },
        });
      }
      return { success: true };
    });

    await watcher.handleMessage({
      type: "subscribe",
      payload: {
        subscriberId: "ws_broadcast_handler",
      },
    });

    // Trigger change
    await new Promise((resolve) => setTimeout(resolve, 50));
    appendFileSync(tasksFilePath, "\n");

    // Wait for processing
    await new Promise((resolve) => setTimeout(resolve, 500));

    // Verify broadcast occurred
    expect(broadcasts.length).toBeGreaterThan(0);
    expect(broadcasts[0].type).toBe("graph_reloaded");
  });
});
