#!/usr/bin/env bun
// Self-Managing Development Example
//
// This demonstrates the "aha!" moment: the system managing its own development.
// Background agents become tasks. Progress is visible. Coordination is automatic.
// The ultimate dog-fooding.

import type { Actor, Message, Response } from "../src/actors/base";
import { createBootstrap } from "../src/bootstrap";
import { Graph } from "../src/graph";
import { createTask } from "../src/task";

// ============================================================================
// Simulated Development Agents
// ============================================================================

/**
 * DocumentationAgent - Creates documentation files
 */
class DocumentationAgent implements Actor {
  readonly id: string;
  readonly type = "agent" as const;
  private progress = 0;
  private state: "idle" | "working" | "completed" = "idle";

  constructor(id: string) {
    this.id = id;
  }

  async send(message: Message): Promise<Response> {
    if (message.type === "ping") {
      return { success: true, data: { alive: true, state: this.state } };
    }

    if (message.type === "start_work") {
      this.state = "working";
      console.log(`[${this.id}] Starting documentation work...`);
      return { success: true, data: { started: true } };
    }

    if (message.type === "get_progress") {
      // Simulate incremental progress
      if (this.state === "working" && this.progress < 100) {
        this.progress += 10;
        console.log(`[${this.id}] Progress: ${this.progress}%`);
      }
      if (this.progress >= 100) {
        this.state = "completed";
      }
      return { success: true, data: { progress: this.progress, state: this.state } };
    }

    return { success: false, error: `Unknown message type: ${message.type}` };
  }
}

/**
 * TestingAgent - Writes and runs tests
 */
class TestingAgent implements Actor {
  readonly id: string;
  readonly type = "agent" as const;
  private progress = 0;
  private state: "idle" | "working" | "completed" = "idle";

  constructor(id: string) {
    this.id = id;
  }

  async send(message: Message): Promise<Response> {
    if (message.type === "ping") {
      return { success: true, data: { alive: true, state: this.state } };
    }

    if (message.type === "start_work") {
      this.state = "working";
      console.log(`[${this.id}] Starting test work...`);
      return { success: true, data: { started: true } };
    }

    if (message.type === "get_progress") {
      if (this.state === "working" && this.progress < 100) {
        this.progress += 15; // Tests progress faster
        console.log(`[${this.id}] Progress: ${this.progress}%`);
      }
      if (this.progress >= 100) {
        this.state = "completed";
      }
      return { success: true, data: { progress: this.progress, state: this.state } };
    }

    return { success: false, error: `Unknown message type: ${message.type}` };
  }
}

/**
 * BugFixAgent - Fixes bugs
 */
class BugFixAgent implements Actor {
  readonly id: string;
  readonly type = "agent" as const;
  private progress = 0;
  private state: "idle" | "working" | "completed" = "idle";

  constructor(id: string) {
    this.id = id;
  }

  async send(message: Message): Promise<Response> {
    if (message.type === "ping") {
      return { success: true, data: { alive: true, state: this.state } };
    }

    if (message.type === "start_work") {
      this.state = "working";
      console.log(`[${this.id}] Starting bug fix work...`);
      return { success: true, data: { started: true } };
    }

    if (message.type === "get_progress") {
      if (this.state === "working" && this.progress < 100) {
        this.progress += 20; // Bug fixes can be quick
        console.log(`[${this.id}] Progress: ${this.progress}%`);
      }
      if (this.progress >= 100) {
        this.state = "completed";
      }
      return { success: true, data: { progress: this.progress, state: this.state } };
    }

    return { success: false, error: `Unknown message type: ${message.type}` };
  }
}

// ============================================================================
// The Magic: Bootstrap in Action
// ============================================================================

async function demonstrateBootstrap() {
  console.log("🎯 Self-Managing Development Demo\n");
  console.log("The system uses ITSELF to manage its own development.\n");

  // Create graph and bootstrap
  const graph = new Graph();
  const bootstrap = createBootstrap(graph);

  console.log("📊 Step 1: Create root development task\n");

  // Create root task for overall project
  const rootTask = createTask(
    {
      goal: "Improve tk-agents system",
      desiredDeliverables: ["working code", "documentation", "passing tests"],
      objectiveSuccessCriteria: [
        { criterion: "code complete", measure: "files created", threshold: true },
        { criterion: "docs complete", measure: "documentation exists", threshold: true },
        { criterion: "tests pass", measure: "bun test", threshold: true },
      ],
    },
    graph
  );

  console.log(`✅ Created root task: ${rootTask.properties.id}`);
  console.log(`   Goal: "${rootTask.properties.goal}"\n`);

  console.log("🤖 Step 2: Register agents as agent-tasks\n");

  // Agent 1: Documentation (simulating agent a448d35)
  const docsAgent = new DocumentationAgent("agent_docs");
  const docsAgentTask = bootstrap.create({
    agent: docsAgent,
    goal: "Create actor interface documentation",
    deliverables: ["ACTOR_INTERFACE.md", "examples/minimal-actors.ts"],
    criteria: [
      { criterion: "documentation complete", measure: "file exists", threshold: true },
    ],
    parentTaskId: rootTask.properties.id,
  });

  console.log(`✅ Created agent-task: ${docsAgentTask.taskId}`);
  console.log(`   Agent: ${docsAgentTask.agentId}`);
  console.log(`   Goal: "Create actor interface documentation"\n`);

  // Agent 2: Bug Fix (simulating agent a35bcf5)
  const bugFixAgent = new BugFixAgent("agent_bugfix");
  const bugFixAgentTask = bootstrap.create({
    agent: bugFixAgent,
    goal: "Fix Playwright concept graph tests",
    deliverables: ["passing browser tests"],
    criteria: [{ criterion: "tests pass", measure: "playwright test", threshold: true }],
    parentTaskId: rootTask.properties.id,
  });

  console.log(`✅ Created agent-task: ${bugFixAgentTask.taskId}`);
  console.log(`   Agent: ${bugFixAgentTask.agentId}`);
  console.log(`   Goal: "Fix Playwright concept graph tests"\n`);

  console.log("▶️  Step 3: Start agent work\n");

  // Start agents
  await bootstrap.registry.sendTo(docsAgent.id, "start_work", {});
  await bootstrap.registry.sendTo(bugFixAgent.id, "start_work", {});

  // Simulate some progress
  await sleep(100);
  await bootstrap.registry.sendTo(docsAgent.id, "get_progress", {});
  await bootstrap.registry.sendTo(bugFixAgent.id, "get_progress", {});

  console.log("\n📈 Step 4: View task tree (automatic progress tracking!)\n");

  const status1 = bootstrap.projectStatus(rootTask.properties.id);
  printTaskTree(status1, 0);

  console.log("\n💡 Step 5: DYNAMIC TASK INJECTION (The Aha! Moment)\n");
  console.log('User says: "Don\'t forget tests for the documentation!"\n');

  // Inject test task dynamically
  const testTask = bootstrap.inject({
    parentTaskId: docsAgentTask.taskId,
    goal: "Write tests for minimal actors",
    deliverables: ["examples/minimal-actors.test.ts"],
    criteria: [{ criterion: "tests pass", measure: "bun test", threshold: true }],
    triggerCondition: "immediate",
    makeDependency: true, // Docs now depend on tests
  });

  console.log(`✅ Injected test task: ${testTask.properties.id}`);
  console.log(`   Parent: ${docsAgentTask.taskId}`);
  console.log(`   Dependency created: docs now depend on tests!\n`);

  // Create agent for test task
  const testAgent = new TestingAgent("agent_tests");
  const testAgentTask = bootstrap.create({
    agent: testAgent,
    goal: "Write tests for minimal actors",
    deliverables: ["examples/minimal-actors.test.ts"],
    criteria: [{ criterion: "tests pass", measure: "bun test", threshold: true }],
    parentTaskId: docsAgentTask.taskId,
  });

  console.log(`✅ Created test agent: ${testAgentTask.agentId}\n`);

  await bootstrap.registry.sendTo(testAgent.id, "start_work", {});
  await sleep(100);
  await bootstrap.registry.sendTo(testAgent.id, "get_progress", {});

  console.log("📈 Step 6: Updated task tree (shows dynamic injection!)\n");

  const status2 = bootstrap.projectStatus(rootTask.properties.id);
  printTaskTree(status2, 0);

  console.log("\n⏩ Step 7: Simulate completion\n");

  // Simulate more progress
  for (let i = 0; i < 8; i++) {
    await sleep(200);
    await bootstrap.registry.sendTo(docsAgent.id, "get_progress", {});
    await bootstrap.registry.sendTo(bugFixAgent.id, "get_progress", {});
    await bootstrap.registry.sendTo(testAgent.id, "get_progress", {});
  }

  console.log("\n📈 Step 8: Final status\n");

  const finalStatus = bootstrap.projectStatus(rootTask.properties.id);
  printTaskTree(finalStatus, 0);

  console.log("\n✨ THE AHA! MOMENT:\n");
  console.log("   ✓ Agents automatically became tasks");
  console.log("   ✓ Task tree shows real-time progress");
  console.log("   ✓ Dynamic task injection worked seamlessly");
  console.log("   ✓ Dependencies propagated automatically");
  console.log("   ✓ Zero manual polling needed");
  console.log("   ✓ The system managed ITSELF!\n");

  console.log("🎉 Bootstrap successful! The system is now self-aware.\n");

  // Cleanup
  bootstrap.registry.clear();
}

// ============================================================================
// Helpers
// ============================================================================

function printTaskTree(task: any, indent: number) {
  const prefix = "  ".repeat(indent);
  const stateEmoji = {
    created: "⚪",
    ready: "🟡",
    active: "🟢",
    blocked: "🔴",
    completed: "✅",
    failed: "❌",
  }[task.state] || "❓";

  const progressBar = "█".repeat(Math.floor(task.progress * 10)) + "░".repeat(10 - Math.floor(task.progress * 10));
  const progressPercent = Math.floor(task.progress * 100);

  console.log(`${prefix}${stateEmoji} ${task.taskId} [${task.state}] ${progressBar} ${progressPercent}%`);
  if (task.agentId) {
    console.log(`${prefix}   Agent: ${task.agentId}`);
  }
  if (task.blockers.length > 0) {
    console.log(`${prefix}   Blockers: ${task.blockers.join(", ")}`);
  }

  for (const child of task.children) {
    printTaskTree(child, indent + 1);
  }
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

// ============================================================================
// Run Demo
// ============================================================================

if (import.meta.main) {
  demonstrateBootstrap().catch(console.error);
}
