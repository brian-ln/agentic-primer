#!/usr/bin/env bun
/**
 * Demo: Task/Knowledge Graph Actor Protocol
 *
 * Demonstrates the complete task lifecycle:
 * created → planning → ready → assigned → active → paused → active → completed
 *
 * Also shows: spawn, checkpoint, cancel propagation
 */

import { Graph } from "./src/graph";
import { createKnowledge } from "./src/knowledge";
import { createTask } from "./src/task";
import type { EvalResponse, StatusResponse, TaskProperties, TaskState } from "./src/types";

// Helper for pretty printing
function log(label: string, data?: unknown) {
  console.log(`\n${"=".repeat(60)}`);
  console.log(`📍 ${label}`);
  console.log("=".repeat(60));
  if (data !== undefined) {
    console.log(typeof data === "string" ? data : JSON.stringify(data, null, 2));
  }
}

function stateChange(taskId: string, result: { success: boolean; state: TaskState }) {
  console.log(`  ${taskId}: ${result.success ? "✓" : "✗"} → ${result.state}`);
}

function separator() {
  console.log("\n" + "-".repeat(60) + "\n");
}

// Create a fresh graph
const graph = new Graph();

log("DEMO: Complete Task Lifecycle State Machine");

// ============================================================
// Phase 1: Create Knowledge Context
// ============================================================

log("Phase 1: Creating Knowledge Context");

const projectKnowledge = createKnowledge(
  {
    title: "Project Requirements",
    content: "Build a REST API for user management. Must support CRUD operations.",
    sources: ["requirements.md"],
  },
  graph
);
console.log(`Created: ${projectKnowledge.properties.id} - "${projectKnowledge.properties.title}"`);

// ============================================================
// Phase 2: Create & Plan Task
// ============================================================

log("Phase 2: Create & Plan Task");

const mainTask = createTask(
  {
    goal: "Implement User Management API",
    knowledge: [projectKnowledge],
    maxAttempts: 2, // Allow 1 retry
  },
  graph
);

console.log(`Created: ${mainTask.properties.id}`);
console.log(`State: ${mainTask.properties.state}`);

// Planning phase
const planResult = graph.send(mainTask.properties.id, "plan", { agent: "planner-agent" }) as {
  success: boolean;
  state: TaskState;
};
stateChange(mainTask.properties.id, planResult);

// Define criteria (complete planning)
const defineResult = graph.send(mainTask.properties.id, "define", {
  criteria: [
    { criterion: "All endpoints implemented", measure: "endpoint_count", threshold: 4 },
    { criterion: "Tests passing", measure: "test_pass_rate", threshold: 1.0 },
  ],
  deliverables: ["User CRUD endpoints", "Tests"],
}) as { success: boolean; state: TaskState };
stateChange(mainTask.properties.id, defineResult);

// ============================================================
// Phase 3: Assign & Start
// ============================================================

log("Phase 3: Assign & Start");

const assignResult = graph.send(mainTask.properties.id, "assign", { actorId: "executor-agent" }) as {
  success: boolean;
  state: TaskState;
};
stateChange(mainTask.properties.id, assignResult);
console.log(`  Assigned to: ${mainTask.properties.assignedTo}`);

const startResult = graph.send(mainTask.properties.id, "start", {}) as {
  success: boolean;
  state: TaskState;
};
stateChange(mainTask.properties.id, startResult);
console.log(`  Started at: ${mainTask.properties.startedAt}`);

// ============================================================
// Phase 4: Spawn Children & Use Checkpoints
// ============================================================

log("Phase 4: Spawn Children with Checkpointing");

const endpoints = ["GET /users", "POST /users"];
const childIds: string[] = [];

for (const endpoint of endpoints) {
  const spawnResult = graph.send(mainTask.properties.id, "spawn", {
    goal: `Implement ${endpoint}`,
    deliverables: [`${endpoint} handler`],
    criteria: [{ criterion: "Handler implemented", measure: "done", threshold: true }],
  }) as { childTaskId: string };

  childIds.push(spawnResult.childTaskId);
  console.log(`  Spawned: ${spawnResult.childTaskId} - "${endpoint}"`);
}

// Save checkpoint with progress
graph.send(mainTask.properties.id, "checkpoint", {
  data: { phase: "spawning", childrenSpawned: childIds.length },
});
console.log(`  Checkpoint saved: phase=spawning, children=${childIds.length}`);

// ============================================================
// Phase 5: Pause & Resume (with checkpoint)
// ============================================================

log("Phase 5: Pause & Resume");

const pauseResult = graph.send(mainTask.properties.id, "pause", {
  reason: "Awaiting review",
  checkpoint: { phase: "paused_for_review", progress: 50 },
}) as { success: boolean; state: TaskState };
stateChange(mainTask.properties.id, pauseResult);
console.log(`  Paused at: ${mainTask.properties.pausedAt}`);
console.log(`  Checkpoint: ${JSON.stringify(mainTask.properties.checkpoint)}`);

separator();
console.log("... (simulating pause for review) ...");
separator();

const resumeResult = graph.send(mainTask.properties.id, "resume", {}) as {
  success: boolean;
  state: TaskState;
  checkpoint: unknown;
};
stateChange(mainTask.properties.id, resumeResult);
console.log(`  Resumed with checkpoint: ${JSON.stringify(resumeResult.checkpoint)}`);

// ============================================================
// Phase 6: Complete Children
// ============================================================

log("Phase 6: Complete Children");

for (const childId of childIds) {
  // Move through lifecycle
  graph.send(childId, "define", {});
  graph.send(childId, "assign", { actorId: "worker-agent" });
  graph.send(childId, "start", {});

  // Mark criteria as met
  graph.send(childId, "update", {
    properties: {
      objectiveSuccessCriteria: [{ criterion: "Handler implemented", measure: "done", threshold: true, actual: true }],
    },
  });

  // Complete
  const result = graph.send(childId, "complete", { result: { status: "done" } }) as {
    success: boolean;
    state: TaskState;
  };
  stateChange(childId, result);
}

// ============================================================
// Phase 7: Complete Parent
// ============================================================

log("Phase 7: Complete Parent");

// Update criteria
graph.send(mainTask.properties.id, "update", {
  properties: {
    objectiveSuccessCriteria: [
      { criterion: "All endpoints implemented", measure: "endpoint_count", threshold: 4, actual: 4 },
      { criterion: "Tests passing", measure: "test_pass_rate", threshold: 1.0, actual: 1.0 },
    ],
  },
});

const evalResult = graph.send(mainTask.properties.id, "eval", {}) as EvalResponse;
console.log(`  Eval: score=${evalResult.score}, passed=${evalResult.passed}`);

const completeResult = graph.send(mainTask.properties.id, "complete", {
  result: { summary: "API implemented" },
}) as { success: boolean; state: TaskState };
stateChange(mainTask.properties.id, completeResult);

// ============================================================
// Phase 8: Demonstrate Cancel Propagation
// ============================================================

log("Phase 8: Cancel Propagation (New Task)");

const cancelDemo = createTask({ goal: "Task to cancel" }, graph);
graph.send(cancelDemo.properties.id, "define", {});
graph.send(cancelDemo.properties.id, "assign", { actorId: "agent" });
graph.send(cancelDemo.properties.id, "start", {});

// Spawn a child
const { childTaskId } = graph.send(cancelDemo.properties.id, "spawn", {
  goal: "Child to cancel",
  deliverables: [],
  criteria: [],
}) as { childTaskId: string };

graph.send(childTaskId, "define", {});
graph.send(childTaskId, "assign", { actorId: "agent" });
graph.send(childTaskId, "start", {});

console.log(`  Parent: ${cancelDemo.properties.id} (${cancelDemo.properties.state})`);
console.log(`  Child:  ${childTaskId} (active)`);

// Cancel parent - should propagate to child
const cancelResult = graph.send(cancelDemo.properties.id, "cancel", {
  reason: "User cancelled",
  cancelChildren: true,
}) as { success: boolean; state: TaskState; cancelledChildren: string[] };

console.log(`  Cancel result: ${cancelResult.state}`);
console.log(`  Cancelled children: ${cancelResult.cancelledChildren.join(", ")}`);

const childState = (graph.getNode(childTaskId)?.properties as TaskProperties).state;
console.log(`  Child state after: ${childState}`);

// ============================================================
// Phase 9: Final Summary
// ============================================================

log("Phase 9: Final Summary");

const graphDump = graph.dump();
console.log(`Total nodes: ${graphDump.nodes.length}`);
console.log(`Total edges: ${graphDump.edges.length}`);

separator();
console.log("Task States:");
for (const node of graphDump.nodes) {
  if (node.type === "task") {
    const t = node as TaskProperties;
    console.log(`  ${t.id}: ${t.state.padEnd(10)} "${t.goal}"`);
  }
}

separator();
const finalStatus = graph.send(mainTask.properties.id, "query_status", {}) as StatusResponse;
console.log("Main task final status:");
console.log(`  State: ${finalStatus.state}`);
console.log(`  Progress: ${finalStatus.progress * 100}%`);
console.log(`  Attempts: ${finalStatus.attemptCount}/${finalStatus.maxAttempts}`);

log("Demo Complete!", "Full lifecycle demonstrated including pause, resume, checkpoint, and cancel.");
