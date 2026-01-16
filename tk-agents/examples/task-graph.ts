/**
 * Task Graph Actor System Example
 *
 * Demonstrates:
 * - Address proxy pattern with ergonomic .send()
 * - Actor factories returning addresses
 * - Task dependencies using Address references
 * - Pure functions with explicit system dependency
 */

import { System } from "../src/actors/index.ts";
import type { ActorFactory, Address, Message, System as SystemType } from "../src/actors/index.ts";

// Task data structure
interface TaskData {
  id: string;
  name: string;
  status: "pending" | "running" | "done";
  dependents: Address[]; // Addresses of tasks that depend on this one
  dependencies: Address[]; // Addresses of tasks this one depends on
  blockedBy: Set<Address>; // Addresses of incomplete dependencies
  system: SystemType; // System for sending messages
}

// TaskActor - pure function that creates an actor and returns Address
const TaskActor: ActorFactory<TaskData> = (data) => {
  const actor = {
    send: async (message: Message) => {
      switch (message.type) {
        case "start": {
          if (data.blockedBy.size > 0) {
            return {
              success: false,
              error: `Task ${data.id} is blocked by ${data.blockedBy.size} dependencies`,
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

          // Notify all dependent tasks using ergonomic Address.send()
          for (const dependent of data.dependents) {
            await dependent.send({
              id: crypto.randomUUID(),
              type: "unblock",
              payload: { completedTask: data.id },
            });
          }

          return { success: true };
        }

        case "unblock": {
          const { completedTask } = message.payload as { completedTask: string };

          // Remove the dependency that completed (find by comparing)
          for (const dep of data.dependencies) {
            // We need to track which address completed - for now just reduce count
            if (data.blockedBy.size > 0) {
              data.blockedBy.delete(Array.from(data.blockedBy)[0]);
              break;
            }
          }

          console.log(
            `[${data.id}] Unblocked by ${completedTask}. Remaining blockers: ${data.blockedBy.size}`
          );

          // Auto-start if no longer blocked
          if (data.blockedBy.size === 0 && data.status === "pending") {
            // Use system.send() to send to self (we need our own address)
            // For now, just change status directly
            data.status = "running";
            console.log(`[${data.id}] Auto-started: ${data.name}`);
          }

          return { success: true };
        }

        case "addDependent": {
          const { dependent } = message.payload as { dependent: Address };
          data.dependents.push(dependent);
          console.log(`[${data.id}] Added dependent`);
          return { success: true };
        }

        case "status": {
          return {
            success: true,
            data: {
              id: data.id,
              name: data.name,
              status: data.status,
              blockedBy: data.blockedBy.size,
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

  return data.system.register(actor);
};

// Example usage: Build a simple task graph
async function main() {
  console.log("=== Task Graph Actor System (Address Proxy Pattern) ===\n");

  const system = System();

  // Create tasks in dependency order
  // setup -> [build, test] -> deploy

  const setupAddr = TaskActor({
    id: "setup",
    name: "Setup environment",
    status: "pending",
    dependents: [], // Will add build and test
    dependencies: [],
    blockedBy: new Set(),
    system,
  });

  const buildAddr = TaskActor({
    id: "build",
    name: "Build application",
    status: "pending",
    dependents: [], // Will add deploy
    dependencies: [setupAddr],
    blockedBy: new Set([setupAddr]),
    system,
  });

  const testAddr = TaskActor({
    id: "test",
    name: "Run tests",
    status: "pending",
    dependents: [], // Will add deploy
    dependencies: [setupAddr],
    blockedBy: new Set([setupAddr]),
    system,
  });

  const deployAddr = TaskActor({
    id: "deploy",
    name: "Deploy to production",
    status: "pending",
    dependents: [],
    dependencies: [buildAddr, testAddr],
    blockedBy: new Set([buildAddr, testAddr]),
    system,
  });

  // Link the dependency graph by adding dependents
  await setupAddr.send({
    id: crypto.randomUUID(),
    type: "addDependent",
    payload: { dependent: buildAddr },
  });

  await setupAddr.send({
    id: crypto.randomUUID(),
    type: "addDependent",
    payload: { dependent: testAddr },
  });

  await buildAddr.send({
    id: crypto.randomUUID(),
    type: "addDependent",
    payload: { dependent: deployAddr },
  });

  await testAddr.send({
    id: crypto.randomUUID(),
    type: "addDependent",
    payload: { dependent: deployAddr },
  });

  console.log("\nTask graph created. Starting execution...\n");

  // Start the workflow - setup has no dependencies
  await setupAddr.send({
    id: crypto.randomUUID(),
    type: "start",
    payload: {},
  });

  await setupAddr.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  // Simulate build and test completing
  await new Promise((resolve) => setTimeout(resolve, 100));

  await buildAddr.send({
    id: crypto.randomUUID(),
    type: "start",
    payload: {},
  });

  await buildAddr.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  await testAddr.send({
    id: crypto.randomUUID(),
    type: "start",
    payload: {},
  });

  await testAddr.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  // Wait for deploy to be unblocked
  await new Promise((resolve) => setTimeout(resolve, 100));

  await deployAddr.send({
    id: crypto.randomUUID(),
    type: "start",
    payload: {},
  });

  await deployAddr.send({
    id: crypto.randomUUID(),
    type: "complete",
    payload: {},
  });

  console.log("\n=== Final Status ===");
  const addresses = [
    { name: "setup", addr: setupAddr },
    { name: "build", addr: buildAddr },
    { name: "test", addr: testAddr },
    { name: "deploy", addr: deployAddr },
  ];

  for (const { name, addr } of addresses) {
    const result = await addr.send({
      id: crypto.randomUUID(),
      type: "status",
      payload: {},
    });
    console.log(`${name}: ${JSON.stringify(result.data)}`);
  }
}

main().catch(console.error);
