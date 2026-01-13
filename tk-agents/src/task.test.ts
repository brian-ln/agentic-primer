// Tests for TaskNode state machine

import { describe, test, expect, beforeEach } from "bun:test";
import { Graph } from "./graph";
import { TaskNode, createTask } from "./task";
import type { TaskState } from "./types";

describe("TaskNode State Machine", () => {
  let graph: Graph;

  beforeEach(() => {
    graph = new Graph();
  });

  describe("Initial State", () => {
    test("new task starts in created state", () => {
      const task = createTask({ goal: "Test task" }, graph);
      expect(task.properties.state).toBe("created");
    });

    test("has default values for retry and priority", () => {
      const task = createTask({ goal: "Test task" }, graph);
      expect(task.properties.attemptCount).toBe(0);
      expect(task.properties.maxAttempts).toBe(1);
      expect(task.properties.priority).toBe(50);
    });
  });

  describe("Planning Phase", () => {
    test("created -> planning via plan", () => {
      const task = createTask({ goal: "Test" }, graph);
      const result = graph.send(task.properties.id, "plan", {}) as { success: boolean; state: TaskState };
      expect(result.success).toBe(true);
      expect(result.state).toBe("planning");
    });

    test("planning -> ready via define", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "plan", {});
      const result = graph.send(task.properties.id, "define", {
        criteria: [{ criterion: "Done", measure: "done", threshold: true }],
      }) as { success: boolean; state: TaskState };
      expect(result.success).toBe(true);
      expect(result.state).toBe("ready");
    });

    test("created -> ready via define (skip planning)", () => {
      const task = createTask({ goal: "Test" }, graph);
      const result = graph.send(task.properties.id, "define", {}) as { success: boolean; state: TaskState };
      expect(result.success).toBe(true);
      expect(result.state).toBe("ready");
    });
  });

  describe("Assignment Phase", () => {
    test("ready -> assigned via assign", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      const result = graph.send(task.properties.id, "assign", { actorId: "agent-1" }) as {
        success: boolean;
        state: TaskState;
      };
      expect(result.success).toBe(true);
      expect(result.state).toBe("assigned");
      expect(task.properties.assignedTo).toBe("agent-1");
    });

    test("assigned -> ready via release", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      const result = graph.send(task.properties.id, "release", {}) as { success: boolean; state: TaskState };
      expect(result.success).toBe(true);
      expect(result.state).toBe("ready");
      expect(task.properties.assignedTo).toBeUndefined();
    });

    test("assigned -> active via start", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      const result = graph.send(task.properties.id, "start", {}) as { success: boolean; state: TaskState };
      expect(result.success).toBe(true);
      expect(result.state).toBe("active");
      expect(task.properties.startedAt).toBeDefined();
    });
  });

  describe("Scheduling", () => {
    test("ready -> scheduled via schedule", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      const futureTime = new Date(Date.now() + 60000);
      const result = graph.send(task.properties.id, "schedule", { time: futureTime }) as {
        success: boolean;
        state: TaskState;
      };
      expect(result.success).toBe(true);
      expect(result.state).toBe("scheduled");
      expect(task.properties.scheduledFor).toEqual(futureTime);
    });

    test("scheduled -> ready via unschedule", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "schedule", { time: new Date() });
      const result = graph.send(task.properties.id, "unschedule", {}) as { success: boolean; state: TaskState };
      expect(result.success).toBe(true);
      expect(result.state).toBe("ready");
    });
  });

  describe("Pause and Resume", () => {
    test("active -> paused via pause", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result = graph.send(task.properties.id, "pause", {
        reason: "Need review",
        checkpoint: { progress: 50 },
      }) as { success: boolean; state: TaskState };

      expect(result.success).toBe(true);
      expect(result.state).toBe("paused");
      expect(task.properties.checkpoint).toEqual({ progress: 50 });
    });

    test("paused -> active via resume (returns checkpoint)", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});
      graph.send(task.properties.id, "pause", { checkpoint: { progress: 50 } });

      const result = graph.send(task.properties.id, "resume", {}) as {
        success: boolean;
        state: TaskState;
        checkpoint: unknown;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("active");
      expect(result.checkpoint).toEqual({ progress: 50 });
    });
  });

  describe("Block and Unblock", () => {
    test("active -> blocked via block", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result = graph.send(task.properties.id, "block", {
        reason: "Waiting for data",
        dependencies: ["task-2"],
      }) as { success: boolean; state: TaskState };

      expect(result.success).toBe(true);
      expect(result.state).toBe("blocked");
      expect(task.properties.blockReason).toBe("Waiting for data");
    });

    test("blocked -> active via unblock", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});
      graph.send(task.properties.id, "block", { reason: "Waiting" });

      const result = graph.send(task.properties.id, "unblock", { resolution: "Data received" }) as {
        success: boolean;
        state: TaskState;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("active");
      expect(task.properties.blockReason).toBeUndefined();
    });
  });

  describe("Completion", () => {
    test("active -> completed via complete (no criteria)", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result = graph.send(task.properties.id, "complete", { result: { done: true } }) as {
        success: boolean;
        state: TaskState;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("completed");
      expect(task.properties.result).toEqual({ done: true });
    });

    test("complete fails if criteria not met", () => {
      const task = createTask({
        goal: "Test",
        objectiveSuccessCriteria: [{ criterion: "Must pass", measure: "passed", threshold: true }],
      }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result = graph.send(task.properties.id, "complete", { result: {} }) as {
        success: boolean;
        state: TaskState;
        error?: string;
      };

      expect(result.success).toBe(false);
      expect(result.state).toBe("active");
      expect(result.error).toContain("Cannot complete");
    });
  });

  describe("Failure and Retry", () => {
    test("active -> failed via fail", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result = graph.send(task.properties.id, "fail", { error: "Something went wrong" }) as {
        success: boolean;
        state: TaskState;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("failed");
      expect(task.properties.error).toBe("Something went wrong");
    });

    test("failed -> active via retry (with maxAttempts > 1)", () => {
      const task = createTask({ goal: "Test", maxAttempts: 3 }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});
      graph.send(task.properties.id, "fail", { error: "First failure" });

      const result = graph.send(task.properties.id, "retry", {}) as {
        success: boolean;
        state: TaskState;
        attempt: number;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("active");
      expect(result.attempt).toBe(1);
      expect(task.properties.attemptCount).toBe(1);
    });

    test("retry fails when maxAttempts reached", () => {
      const task = createTask({ goal: "Test", maxAttempts: 2 }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});
      graph.send(task.properties.id, "fail", { error: "First" });
      graph.send(task.properties.id, "retry", {});
      graph.send(task.properties.id, "fail", { error: "Second" });

      const result = graph.send(task.properties.id, "retry", {}) as {
        success: boolean;
        error?: string;
      };

      expect(result.success).toBe(false);
      expect(result.error).toContain("Max attempts");
    });
  });

  describe("Cancellation", () => {
    test("any non-terminal -> cancelled via cancel", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result = graph.send(task.properties.id, "cancel", { reason: "User requested" }) as {
        success: boolean;
        state: TaskState;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("cancelled");
      expect(task.properties.cancelReason).toBe("User requested");
    });

    test("cancel propagates to children", () => {
      const parent = createTask({ goal: "Parent" }, graph);
      graph.send(parent.properties.id, "define", {});
      graph.send(parent.properties.id, "assign", { actorId: "agent-1" });
      graph.send(parent.properties.id, "start", {});

      // Spawn child
      const spawnResult = graph.send(parent.properties.id, "spawn", {
        goal: "Child",
        deliverables: [],
        criteria: [],
      }) as { childTaskId: string };

      const childId = spawnResult.childTaskId;
      graph.send(childId, "define", {});
      graph.send(childId, "assign", { actorId: "agent-2" });
      graph.send(childId, "start", {});

      // Cancel parent
      const result = graph.send(parent.properties.id, "cancel", { reason: "Abort" }) as {
        cancelledChildren: string[];
      };

      expect(result.cancelledChildren).toContain(childId);
      const child = graph.getNode(childId);
      expect((child?.properties as { state: string }).state).toBe("cancelled");
    });

    test("cannot cancel already completed task", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});
      graph.send(task.properties.id, "complete", { result: {} });

      const result = graph.send(task.properties.id, "cancel", {}) as {
        success: boolean;
        error?: string;
      };

      expect(result.success).toBe(false);
      expect(result.error).toContain("terminal state");
    });
  });

  describe("Skip", () => {
    test("ready -> skipped via skip", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});

      const result = graph.send(task.properties.id, "skip", { reason: "Not needed" }) as {
        success: boolean;
        state: TaskState;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("skipped");
    });
  });

  describe("Timeout", () => {
    test("active -> timed_out via timeout", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result = graph.send(task.properties.id, "timeout", {}) as {
        success: boolean;
        state: TaskState;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("timed_out");
      expect(task.properties.error).toBe("Deadline exceeded");
    });

    test("blocked -> timed_out via timeout", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});
      graph.send(task.properties.id, "block", { reason: "Waiting" });

      const result = graph.send(task.properties.id, "timeout", {}) as {
        success: boolean;
        state: TaskState;
      };

      expect(result.success).toBe(true);
      expect(result.state).toBe("timed_out");
    });
  });

  describe("Invalid Transitions", () => {
    test("cannot start from created (must be assigned)", () => {
      const task = createTask({ goal: "Test" }, graph);

      const result = graph.send(task.properties.id, "start", {}) as {
        success: boolean;
        error?: string;
      };

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid transition");
    });

    test("cannot pause from ready", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});

      const result = graph.send(task.properties.id, "pause", {}) as {
        success: boolean;
        error?: string;
      };

      expect(result.success).toBe(false);
    });
  });

  describe("Checkpoint", () => {
    test("checkpoint saves data with version", () => {
      const task = createTask({ goal: "Test" }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});

      const result1 = graph.send(task.properties.id, "checkpoint", { data: { step: 1 } }) as {
        success: boolean;
        version: number;
      };
      expect(result1.version).toBe(1);

      const result2 = graph.send(task.properties.id, "checkpoint", { data: { step: 2 } }) as {
        success: boolean;
        version: number;
      };
      expect(result2.version).toBe(2);
      expect(task.properties.checkpoint).toEqual({ step: 2 });
    });
  });

  describe("Query Status", () => {
    test("query_status includes retry info and checkpoint", () => {
      const task = createTask({ goal: "Test", maxAttempts: 3 }, graph);
      graph.send(task.properties.id, "define", {});
      graph.send(task.properties.id, "assign", { actorId: "agent-1" });
      graph.send(task.properties.id, "start", {});
      graph.send(task.properties.id, "checkpoint", { data: { x: 1 } });

      const status = graph.send(task.properties.id, "query_status", {}) as {
        attemptCount: number;
        maxAttempts: number;
        checkpoint: unknown;
      };

      expect(status.attemptCount).toBe(0);
      expect(status.maxAttempts).toBe(3);
      expect(status.checkpoint).toEqual({ x: 1 });
    });
  });
});
