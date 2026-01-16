/**
 * Task Graph Actor System Example
 *
 * Demonstrates:
 * - Pure function actors with explicit dependencies
 * - Send injection for actor-to-actor communication
 * - Task dependencies and notifications
 */

import { createSystem } from "../src/actors/index.ts";
import type { ActorFactory, Message } from "../src/actors/index.ts";

// Task data structure
interface TaskData {
  id: string;
  name: string;
  status: "pending" | "running" | "done";
  dependents: string[]; // IDs of tasks that depend on this one
  dependencies: string[]; // IDs of tasks this one depends on
  blockedBy: Set<string>; // IDs of incomplete dependencies
}

// TaskActor - pure function that creates an actor
const TaskActor: ActorFactory<TaskData> = (data, send) => {
  return {
    send: async (message: Message) => {
      switch (message.type) {
        case "start": {
          if (data.blockedBy.size > 0) {
            return {
              success: false,
              error: `Task ${data.id} is blocked by: ${Array.from(data.blockedBy).join(", ")}`,
            };
          }
          data.status = "running";
          console.log(`[${data.id}] Started: ${data.name}`);
          return { success: true };
        }

        case "complete": {
          if (data.status !== "running") {
            return {
              success: false,
              error: `Task ${data.id} is not running`,
            };
          }

          data.status = "done";
          console.log(`[${data.id}] Completed: ${data.name}`);

          // Notify all dependent tasks (using send from scope)
          for (const dependentId of data.dependents) {
            await send(dependentId, {
              id: crypto.randomUUID(),
              type: "unblock",
              payload: { completedTask: data.id },
            });
          }

          return { success: true };
        }

        case "unblock": {
          const { completedTask } = message.payload as { completedTask: string };
          data.blockedBy.delete(completedTask);
          console.log(
            `[${data.id}] Unblocked by ${completedTask}. Remaining blockers: ${data.blockedBy.size}`
          );

          // Auto-start if no longer blocked
          if (data.blockedBy.size === 0 && data.status === "pending") {
            await send(data.id, {
              id: crypto.randomUUID(),
              type: "start",
              payload: {},
            });
          }

          return { success: true };
        }

        case "status": {
          return {
            success: true,
            data: {
              id: data.id,
              name: data.name,
              status: data.status,
              blockedBy: Array.from(data.blockedBy),
            },
          };
        }

        default:
          return {
            success: false,
            error: `Unknown message type: ${message.type}`,
          };
      }
    },
  };
};

// Example usage: Build a simple task graph
async function main() {
  console.log("=== Task Graph Actor System ===\n");

  const system = createSystem();

  // Define task graph:
  // setup -> [build, test] -> deploy
  //
  // setup completes first, unblocking both build and test
  // deploy waits for both build and test

  const tasks = [
    {
      id: "setup",
      name: "Setup environment",
      status: "pending" as const,
      dependents: ["build", "test"],
      dependencies: [],
      blockedBy: new Set<string>(),
    },
    {
      id: "build",
      name: "Build application",
      status: "pending" as const,
      dependents: ["deploy"],
      dependencies: ["setup"],
      blockedBy: new Set(["setup"]),
    },
    {
      id: "test",
      name: "Run tests",
      status: "pending" as const,
      dependents: ["deploy"],
      dependencies: ["setup"],
      blockedBy: new Set(["setup"]),
    },
    {
      id: "deploy",
      name: "Deploy to production",
      status: "pending" as const,
      dependents: [],
      dependencies: ["build", "test"],
      blockedBy: new Set(["build", "test"]),
    },
  ];

  // Create and register all actors
  const actors = new Map();
  for (const taskData of tasks) {
    const actor = TaskActor(taskData, system.send);
    system.register(taskData.id, actor);
    actors.set(taskData.id, actor);
  }

  console.log("Task graph created. Starting execution...\n");

  // Start the workflow by completing setup
  const setupActor = actors.get("setup")!;

  // Bridge into actor world: external code uses actor.send()
  await setupActor.send({
    id: crypto.randomUUID(),
    type: "start",
    payload: {},
  });

  await setupActor.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  // Simulate build and test completing
  await new Promise((resolve) => setTimeout(resolve, 100));

  const buildActor = actors.get("build")!;
  await buildActor.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  const testActor = actors.get("test")!;
  await testActor.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  // Wait for deploy to auto-start and then complete it
  await new Promise((resolve) => setTimeout(resolve, 100));

  const deployActor = actors.get("deploy")!;
  await deployActor.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  console.log("\n=== Final Status ===");
  for (const [id, actor] of actors) {
    const result = await actor.send({
      id: crypto.randomUUID(),
      type: "status",
      payload: {},
    });
    console.log(`${id}: ${JSON.stringify(result.data)}`);
  }
}

main().catch(console.error);
