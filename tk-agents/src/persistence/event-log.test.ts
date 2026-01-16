// Tests for EventLog

import { describe, test, expect, beforeEach, afterEach } from "bun:test";
import { EventLog, type Event } from "./event-log";
import { existsSync, unlinkSync } from "node:fs";

describe("EventLog", () => {
  const testLogPath = "./test-event-log.jsonl";

  // Clean up test file before and after each test
  beforeEach(() => {
    if (existsSync(testLogPath)) {
      unlinkSync(testLogPath);
    }
  });

  afterEach(() => {
    if (existsSync(testLogPath)) {
      unlinkSync(testLogPath);
    }
  });

  test("creates new log file", () => {
    const log = new EventLog(testLogPath);

    expect(existsSync(testLogPath)).toBe(true);
    expect(log.getPath()).toBe(testLogPath);
  });

  test("appends events to log", () => {
    const log = new EventLog(testLogPath);

    const event: Event = {
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "test_event",
      nodeId: "node-1",
      data: { message: "Hello, world!" },
    };

    log.append(event);

    const events = log.getAllEvents();
    expect(events.length).toBe(1);
    expect(events[0].type).toBe("test_event");
    expect(events[0].nodeId).toBe("node-1");
  });

  test("appends multiple events", () => {
    const log = new EventLog(testLogPath);

    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "event_1",
      nodeId: "node-1",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:01.000Z",
      type: "event_2",
      nodeId: "node-2",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:02.000Z",
      type: "event_3",
      nodeId: "node-1",
      data: {},
    });

    const events = log.getAllEvents();
    expect(events.length).toBe(3);
  });

  test("replay calls handler for each event", () => {
    const log = new EventLog(testLogPath);

    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "event_1",
      nodeId: "node-1",
      data: { value: 1 },
    });

    log.append({
      timestamp: "2026-01-16T12:00:01.000Z",
      type: "event_2",
      nodeId: "node-1",
      data: { value: 2 },
    });

    const collected: Event[] = [];
    log.replay((event) => {
      collected.push(event);
    });

    expect(collected.length).toBe(2);
    expect(collected[0].type).toBe("event_1");
    expect(collected[1].type).toBe("event_2");
  });

  test("filters events by type", () => {
    const log = new EventLog(testLogPath);

    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "actor_registered",
      nodeId: "actor-1",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:01.000Z",
      type: "message_sent",
      nodeId: "actor-1",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:02.000Z",
      type: "actor_registered",
      nodeId: "actor-2",
      data: {},
    });

    const registrations = log.getEventsByType("actor_registered");
    expect(registrations.length).toBe(2);

    const messages = log.getEventsByType("message_sent");
    expect(messages.length).toBe(1);
  });

  test("filters events by nodeId", () => {
    const log = new EventLog(testLogPath);

    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "event_1",
      nodeId: "actor-1",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:01.000Z",
      type: "event_2",
      nodeId: "actor-2",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:02.000Z",
      type: "event_3",
      nodeId: "actor-1",
      data: {},
    });

    const actor1Events = log.getEventsByNode("actor-1");
    expect(actor1Events.length).toBe(2);

    const actor2Events = log.getEventsByNode("actor-2");
    expect(actor2Events.length).toBe(1);
  });

  test("auto-generates timestamp if not provided", () => {
    const log = new EventLog(testLogPath);

    const beforeAppend = new Date().toISOString();

    log.append({
      timestamp: "", // Will be auto-generated
      type: "test",
      nodeId: "node-1",
      data: {},
    });

    const events = log.getAllEvents();
    expect(events[0].timestamp).toBeDefined();
    expect(events[0].timestamp.length).toBeGreaterThan(0);
    expect(events[0].timestamp >= beforeAppend).toBe(true);
  });

  test("handles empty log file", () => {
    const log = new EventLog(testLogPath);

    const events = log.getAllEvents();
    expect(events.length).toBe(0);
  });

  test("persists across instances", () => {
    // First instance writes
    const log1 = new EventLog(testLogPath);
    log1.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "test",
      nodeId: "node-1",
      data: { message: "persisted" },
    });

    // Second instance reads
    const log2 = new EventLog(testLogPath);
    const events = log2.getAllEvents();

    expect(events.length).toBe(1);
    expect((events[0].data as { message: string }).message).toBe("persisted");
  });

  test("includes metadata when provided", () => {
    const log = new EventLog(testLogPath);

    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "test",
      nodeId: "node-1",
      data: {},
      metadata: {
        source: "test-suite",
        version: 1,
      },
    });

    const events = log.getAllEvents();
    expect(events[0].metadata).toBeDefined();
    expect((events[0].metadata as { source: string }).source).toBe("test-suite");
    expect((events[0].metadata as { version: number }).version).toBe(1);
  });

  test("handles complex data structures", () => {
    const log = new EventLog(testLogPath);

    const complexData = {
      nested: {
        array: [1, 2, 3],
        object: { key: "value" },
      },
      nullValue: null,
      boolValue: true,
      numberValue: 42.5,
    };

    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "complex",
      nodeId: "node-1",
      data: complexData,
    });

    const events = log.getAllEvents();
    expect(events[0].data).toEqual(complexData);
  });
});

describe("EventLog Replay Scenarios", () => {
  const testLogPath = "./test-replay.jsonl";

  afterEach(() => {
    if (existsSync(testLogPath)) {
      unlinkSync(testLogPath);
    }
  });

  test("can rebuild state from events", () => {
    const log = new EventLog(testLogPath);

    // Simulate a series of state changes
    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "counter_set",
      nodeId: "counter-1",
      data: { value: 0 },
    });

    log.append({
      timestamp: "2026-01-16T12:00:01.000Z",
      type: "counter_increment",
      nodeId: "counter-1",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:02.000Z",
      type: "counter_increment",
      nodeId: "counter-1",
      data: {},
    });

    log.append({
      timestamp: "2026-01-16T12:00:03.000Z",
      type: "counter_increment",
      nodeId: "counter-1",
      data: {},
    });

    // Replay to rebuild state
    let counter = 0;
    log.replay((event) => {
      if (event.type === "counter_set") {
        counter = (event.data as { value: number }).value;
      } else if (event.type === "counter_increment") {
        counter++;
      }
    });

    expect(counter).toBe(3);
  });

  test("can reconstruct actor registry from events", () => {
    const log = new EventLog(testLogPath);

    log.append({
      timestamp: "2026-01-16T12:00:00.000Z",
      type: "actor_registered",
      nodeId: "actor-1",
      data: { type: "bash" },
    });

    log.append({
      timestamp: "2026-01-16T12:00:01.000Z",
      type: "actor_registered",
      nodeId: "actor-2",
      data: { type: "claude" },
    });

    log.append({
      timestamp: "2026-01-16T12:00:02.000Z",
      type: "actor_unregistered",
      nodeId: "actor-1",
      data: {},
    });

    // Rebuild registry state
    const actors = new Set<string>();
    log.replay((event) => {
      if (event.type === "actor_registered") {
        actors.add(event.nodeId);
      } else if (event.type === "actor_unregistered") {
        actors.delete(event.nodeId);
      }
    });

    expect(actors.has("actor-1")).toBe(false);
    expect(actors.has("actor-2")).toBe(true);
    expect(actors.size).toBe(1);
  });
});
