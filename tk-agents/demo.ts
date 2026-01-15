#!/usr/bin/env bun
/**
 * Demo: Task/Knowledge Graph Actor Protocol
 *
 * This demonstrates the full lifecycle:
 * 1. Create knowledge context
 * 2. Create a parent task with goals and criteria
 * 3. Start the task
 * 4. Spawn child tasks
 * 5. Complete children with evals
 * 6. Complete parent
 * 7. Observe graph growth
 */

import { Graph } from "./src/graph";
import { createKnowledge } from "./src/knowledge";
import { createTask } from "./src/task";
import type { EvalResponse, StatusResponse, TaskProperties } from "./src/types";

// Helper for pretty printing
function log(label: string, data?: unknown) {
  console.log(`\n${"=".repeat(60)}`);
  console.log(`📍 ${label}`);
  console.log("=".repeat(60));
  if (data !== undefined) {
    console.log(typeof data === "string" ? data : JSON.stringify(data, null, 2));
  }
}

function separator() {
  console.log("\n" + "-".repeat(60) + "\n");
}

// Create a fresh graph
const graph = new Graph();

log("DEMO: Task/Knowledge Graph Actor Protocol");

// ============================================================
// Phase 1: Create Knowledge Context
// ============================================================

log("Phase 1: Creating Knowledge Context");

const projectKnowledge = createKnowledge({
  title: "Project Requirements",
  content: "Build a REST API for user management. Must support CRUD operations. Use TypeScript and follow REST conventions.",
  sources: ["requirements.md"],
}, graph);

console.log(`Created: ${projectKnowledge.properties.id} - "${projectKnowledge.properties.title}"`);

const techKnowledge = createKnowledge({
  title: "Tech Stack",
  content: "Using Bun runtime, Hono framework for HTTP, and SQLite for persistence.",
  sources: ["tech-decisions.md"],
}, graph);

console.log(`Created: ${techKnowledge.properties.id} - "${techKnowledge.properties.title}"`);

// Query knowledge
const queryResult = graph.send(projectKnowledge.properties.id, "query", {
  question: "What operations are needed?",
});
console.log("\nQuery 'What operations are needed?':");
console.log(queryResult);

// ============================================================
// Phase 2: Create Parent Task
// ============================================================

log("Phase 2: Creating Parent Task");

const mainTask = createTask({
  goal: "Implement User Management API",
  desiredDeliverables: [
    "User CRUD endpoints",
    "Input validation",
    "API documentation",
  ],
  objectiveSuccessCriteria: [
    { criterion: "All endpoints implemented", measure: "endpoint_count", threshold: 4 },
    { criterion: "Tests passing", measure: "test_pass_rate", threshold: 1.0 },
  ],
  subjectiveSuccessCriteria: [
    { criterion: "Code quality", evaluationGuidance: "Clean, readable, follows conventions" },
  ],
  knowledge: [projectKnowledge, techKnowledge],
  informationGaps: ["Database schema design"],
  toolsAvailable: ["code_editor", "test_runner", "http_client"],
}, graph);

console.log(`Created main task: ${mainTask.properties.id}`);
console.log(`Goal: "${mainTask.properties.goal}"`);
console.log(`State: ${mainTask.properties.state}`);

// ============================================================
// Phase 3: Start Task
// ============================================================

log("Phase 3: Starting Task");

const startResult = graph.send(mainTask.properties.id, "start", {
  context: { developer: "human", priority: "high" },
});
console.log("Start result:", startResult);

const observeResult = graph.send(mainTask.properties.id, "observe", {});
console.log("Observation:", observeResult);

// ============================================================
// Phase 4: Create Child Tasks
// ============================================================

log("Phase 4: Creating Child Tasks");

// Create subtasks for each endpoint
const endpoints = ["GET /users", "POST /users", "PUT /users/:id", "DELETE /users/:id"];

const childTaskIds: string[] = [];

for (const endpoint of endpoints) {
  const createResult = graph.send(mainTask.properties.id, "create_task", {
    goal: `Implement ${endpoint}`,
    deliverables: [`${endpoint} endpoint handler`, "Input validation", "Tests"],
    criteria: [
      { criterion: "Handler implemented", measure: "implemented", threshold: true },
      { criterion: "Tests written", measure: "has_tests", threshold: true },
    ],
  }) as { childTaskId: string; success: boolean };

  childTaskIds.push(createResult.childTaskId);
  console.log(`Created: ${createResult.childTaskId} - "Implement ${endpoint}"`);
}

// Check parent status
separator();
const statusAfterSpawn = graph.send(mainTask.properties.id, "query_status", {}) as StatusResponse;
console.log("Parent task status after creating child tasks:");
console.log(statusAfterSpawn);

// ============================================================
// Phase 5: Execute Child Tasks
// ============================================================

log("Phase 5: Executing Child Tasks");

for (const childId of childTaskIds) {
  // Start child
  graph.send(childId, "start", {});

  // Simulate work - set criteria as met
  graph.send(childId, "update", {
    properties: {
      objectiveSuccessCriteria: [
        { criterion: "Handler implemented", measure: "implemented", threshold: true, actual: true },
        { criterion: "Tests written", measure: "has_tests", threshold: true, actual: true },
      ],
    },
  });

  // Eval child
  const evalResult = graph.send(childId, "eval", {}) as EvalResponse;
  console.log(`${childId} eval: score=${evalResult.score}, passed=${evalResult.passed}`);

  // Complete if passed
  if (evalResult.passed) {
    const completeResult = graph.send(childId, "complete", {
      result: { status: "implemented" },
    });
    console.log(`${childId} completed:`, completeResult);
  }
}

// ============================================================
// Phase 6: Complete Parent Task
// ============================================================

log("Phase 6: Evaluating & Completing Parent Task");

// Update parent criteria with actual values
graph.send(mainTask.properties.id, "update", {
  properties: {
    objectiveSuccessCriteria: [
      { criterion: "All endpoints implemented", measure: "endpoint_count", threshold: 4, actual: 4 },
      { criterion: "Tests passing", measure: "test_pass_rate", threshold: 1.0, actual: 1.0 },
    ],
  },
});

// Eval parent
const parentEval = graph.send(mainTask.properties.id, "eval", {}) as EvalResponse;
console.log("Parent eval result:");
console.log(parentEval);

// Complete parent
if (parentEval.passed) {
  const completeResult = graph.send(mainTask.properties.id, "complete", {
    result: {
      summary: "User Management API implemented with 4 endpoints",
      artifacts: ["src/routes/users.ts", "tests/users.test.ts"],
    },
  });
  console.log("\nParent completion:", completeResult);
}

// ============================================================
// Phase 7: Observe Final Graph State
// ============================================================

log("Phase 7: Final Graph State");

const graphDump = graph.dump();
console.log(`\nTotal nodes: ${graphDump.nodes.length}`);
console.log(`Total edges: ${graphDump.edges.length}`);

separator();
console.log("Nodes:");
for (const node of graphDump.nodes) {
  const state = (node as TaskProperties).state || "n/a";
  const type = node.type;
  const name = (node as TaskProperties).goal || (node as { title?: string }).title || "unnamed";
  console.log(`  - ${node.id} [${type}] "${name}" (${state})`);
}

separator();
console.log("Edges:");
for (const edge of graphDump.edges) {
  console.log(`  - ${edge.fromId} --${edge.type}--> ${edge.toId}`);
}

// Final status
separator();
const finalStatus = graph.send(mainTask.properties.id, "query_status", {}) as StatusResponse;
console.log("Final parent task status:");
console.log(finalStatus);

log("Demo Complete!", "All tasks completed through the full lifecycle.");
