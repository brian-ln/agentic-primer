/**
 * EventBus Unit Tests
 *
 * Tests for the publish/subscribe event system
 */

import { test, expect, describe, beforeEach } from "bun:test";
import { EventBus, type Subscription } from "./event-bus.ts";
import type { Event } from "../persistence/event-log.ts";
import { EVENT_TYPES } from "./task-events.ts";

describe("EventBus", () => {
  let eventBus: EventBus;

  beforeEach(() => {
    eventBus = new EventBus();
  });

  describe("subscribe", () => {
    test("should subscribe to event type and return subscription", () => {
      const listener = () => {};
      const subscription = eventBus.subscribe("test_event", listener);

      expect(subscription).toBeDefined();
      expect(subscription.id).toMatch(/^sub_\d+$/);
      expect(subscription.eventType).toBe("test_event");
      expect(subscription.unsubscribe).toBeFunction();
    });

    test("should increment subscription counter", () => {
      const sub1 = eventBus.subscribe("test_event", () => {});
      const sub2 = eventBus.subscribe("test_event", () => {});

      // IDs should be unique and incrementing
      expect(sub1.id).not.toBe(sub2.id);
      expect(parseInt(sub1.id.replace("sub_", ""))).toBeLessThan(
        parseInt(sub2.id.replace("sub_", ""))
      );
    });

    test("should allow multiple subscribers to same event type", () => {
      eventBus.subscribe("test_event", () => {});
      eventBus.subscribe("test_event", () => {});

      expect(eventBus.getSubscriberCount("test_event")).toBe(2);
    });
  });

  describe("subscribeMany", () => {
    test("should subscribe to multiple event types", () => {
      const listener = () => {};
      const subscriptions = eventBus.subscribeMany(
        ["event1", "event2", "event3"],
        listener
      );

      expect(subscriptions).toHaveLength(3);
      expect(eventBus.getSubscriberCount("event1")).toBe(1);
      expect(eventBus.getSubscriberCount("event2")).toBe(1);
      expect(eventBus.getSubscriberCount("event3")).toBe(1);
    });
  });

  describe("publish", () => {
    test("should call listener when event is published", async () => {
      let called = false;
      let receivedEvent: Event | null = null;

      eventBus.subscribe("test_event", (event) => {
        called = true;
        receivedEvent = event;
      });

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "test_event",
        nodeId: "test_node",
        data: { message: "hello" },
      };

      await eventBus.publish(testEvent);

      expect(called).toBe(true);
      expect(receivedEvent).toEqual(testEvent);
    });

    test("should call all subscribers for event type", async () => {
      const calls: number[] = [];

      eventBus.subscribe("test_event", () => calls.push(1));
      eventBus.subscribe("test_event", () => calls.push(2));
      eventBus.subscribe("test_event", () => calls.push(3));

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "test_event",
        nodeId: "test_node",
        data: {},
      };

      await eventBus.publish(testEvent);

      expect(calls).toHaveLength(3);
      expect(calls).toContain(1);
      expect(calls).toContain(2);
      expect(calls).toContain(3);
    });

    test("should not call listeners for different event types", async () => {
      let called = false;

      eventBus.subscribe("event_a", () => {
        called = true;
      });

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "event_b",
        nodeId: "test_node",
        data: {},
      };

      await eventBus.publish(testEvent);

      expect(called).toBe(false);
    });

    test("should handle async listeners", async () => {
      let asyncCompleted = false;

      eventBus.subscribe("test_event", async () => {
        await new Promise((resolve) => setTimeout(resolve, 10));
        asyncCompleted = true;
      });

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "test_event",
        nodeId: "test_node",
        data: {},
      };

      await eventBus.publish(testEvent);

      expect(asyncCompleted).toBe(true);
    });

    test("should handle listener errors gracefully", async () => {
      let errorListenerCalled = false;
      let normalListenerCalled = false;

      // Listener that throws error
      eventBus.subscribe("test_event", () => {
        errorListenerCalled = true;
        throw new Error("Test error");
      });

      // Normal listener should still execute
      eventBus.subscribe("test_event", () => {
        normalListenerCalled = true;
      });

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "test_event",
        nodeId: "test_node",
        data: {},
      };

      await eventBus.publish(testEvent);

      expect(errorListenerCalled).toBe(true);
      expect(normalListenerCalled).toBe(true);
    });

    test("should do nothing when no subscribers exist", async () => {
      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "test_event",
        nodeId: "test_node",
        data: {},
      };

      // Should not throw
      await expect(eventBus.publish(testEvent)).resolves.toBeUndefined();
    });
  });

  describe("unsubscribe", () => {
    test("should remove subscription when unsubscribe is called", async () => {
      let called = false;

      const subscription = eventBus.subscribe("test_event", () => {
        called = true;
      });

      subscription.unsubscribe();

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "test_event",
        nodeId: "test_node",
        data: {},
      };

      await eventBus.publish(testEvent);

      expect(called).toBe(false);
      expect(eventBus.getSubscriberCount("test_event")).toBe(0);
    });

    test("should only remove specific subscription", async () => {
      let call1 = false;
      let call2 = false;

      const sub1 = eventBus.subscribe("test_event", () => {
        call1 = true;
      });
      eventBus.subscribe("test_event", () => {
        call2 = true;
      });

      sub1.unsubscribe();

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "test_event",
        nodeId: "test_node",
        data: {},
      };

      await eventBus.publish(testEvent);

      expect(call1).toBe(false);
      expect(call2).toBe(true);
      expect(eventBus.getSubscriberCount("test_event")).toBe(1);
    });

    test("should clean up empty listener sets", () => {
      const subscription = eventBus.subscribe("test_event", () => {});

      expect(eventBus.getActiveEventTypes()).toContain("test_event");

      subscription.unsubscribe();

      expect(eventBus.getActiveEventTypes()).not.toContain("test_event");
    });
  });

  describe("getSubscriberCount", () => {
    test("should return 0 for event type with no subscribers", () => {
      expect(eventBus.getSubscriberCount("test_event")).toBe(0);
    });

    test("should return correct count", () => {
      eventBus.subscribe("test_event", () => {});
      eventBus.subscribe("test_event", () => {});
      eventBus.subscribe("test_event", () => {});

      expect(eventBus.getSubscriberCount("test_event")).toBe(3);
    });
  });

  describe("getActiveEventTypes", () => {
    test("should return empty array when no subscriptions", () => {
      expect(eventBus.getActiveEventTypes()).toEqual([]);
    });

    test("should return all event types with subscribers", () => {
      eventBus.subscribe("event1", () => {});
      eventBus.subscribe("event2", () => {});
      eventBus.subscribe("event3", () => {});

      const activeTypes = eventBus.getActiveEventTypes();
      expect(activeTypes).toHaveLength(3);
      expect(activeTypes).toContain("event1");
      expect(activeTypes).toContain("event2");
      expect(activeTypes).toContain("event3");
    });
  });

  describe("clear", () => {
    test("should remove all subscriptions", async () => {
      let called = false;

      eventBus.subscribe("event1", () => {
        called = true;
      });
      eventBus.subscribe("event2", () => {
        called = true;
      });

      eventBus.clear();

      expect(eventBus.getActiveEventTypes()).toEqual([]);

      const testEvent: Event = {
        timestamp: new Date().toISOString(),
        type: "event1",
        nodeId: "test_node",
        data: {},
      };

      await eventBus.publish(testEvent);

      expect(called).toBe(false);
    });
  });

  describe("clearEventType", () => {
    test("should remove all subscribers for specific event type", async () => {
      let call1 = false;
      let call2 = false;

      eventBus.subscribe("event1", () => {
        call1 = true;
      });
      eventBus.subscribe("event2", () => {
        call2 = true;
      });

      eventBus.clearEventType("event1");

      await eventBus.publish({
        timestamp: new Date().toISOString(),
        type: "event1",
        nodeId: "test",
        data: {},
      });

      await eventBus.publish({
        timestamp: new Date().toISOString(),
        type: "event2",
        nodeId: "test",
        data: {},
      });

      expect(call1).toBe(false);
      expect(call2).toBe(true);
    });
  });

  describe("integration with task events", () => {
    test("should handle task_created event", async () => {
      let receivedData: any = null;

      eventBus.subscribe(EVENT_TYPES.TASK_CREATED, (event) => {
        receivedData = event.data;
      });

      await eventBus.publish({
        timestamp: new Date().toISOString(),
        type: EVENT_TYPES.TASK_CREATED,
        nodeId: "task_1",
        data: {
          goal: "Test task",
          deliverables: ["Test deliverable"],
          labels: ["test"],
          priority: 1,
        },
      });

      expect(receivedData).toBeDefined();
      expect(receivedData.goal).toBe("Test task");
      expect(receivedData.labels).toContain("test");
      expect(receivedData.priority).toBe(1);
    });

    test("should handle multiple event types", async () => {
      const events: string[] = [];

      eventBus.subscribe(EVENT_TYPES.TASK_CREATED, () =>
        events.push("created")
      );
      eventBus.subscribe(EVENT_TYPES.TASK_STARTED, () =>
        events.push("started")
      );
      eventBus.subscribe(EVENT_TYPES.TASK_COMPLETED, () =>
        events.push("completed")
      );

      await eventBus.publish({
        timestamp: new Date().toISOString(),
        type: EVENT_TYPES.TASK_CREATED,
        nodeId: "task_1",
        data: {},
      });

      await eventBus.publish({
        timestamp: new Date().toISOString(),
        type: EVENT_TYPES.TASK_STARTED,
        nodeId: "task_1",
        data: {},
      });

      await eventBus.publish({
        timestamp: new Date().toISOString(),
        type: EVENT_TYPES.TASK_COMPLETED,
        nodeId: "task_1",
        data: {},
      });

      expect(events).toEqual(["created", "started", "completed"]);
    });
  });
});
