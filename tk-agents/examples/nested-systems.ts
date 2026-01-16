/**
 * Hierarchical Actor Organization Example
 *
 * Demonstrates:
 * - Organizing actors into logical groups
 * - Router actors for subsystem communication
 * - Address-based cross-group messaging
 * - Pure functional composition
 */

import { System } from "../src/actors/index.ts";
import type { ActorFactory, Address, Message, System as SystemType } from "../src/actors/index.ts";

// Simple echo actor
const EchoActor: ActorFactory<{ prefix: string; system: SystemType }> = (data) => {
  const actor = {
    send: async (message: Message) => {
      console.log(`[${data.prefix}] Received: ${message.type}`);
      return {
        success: true,
        data: `${data.prefix}: ${JSON.stringify(message.payload)}`,
      };
    },
  };
  return data.system.register(actor);
};

// Router actor that forwards messages to actors in its group
const RouterActor: ActorFactory<{
  name: string;
  actors: Map<string, Address>;
  system: SystemType;
}> = (data) => {
  const actor = {
    send: async (message: Message) => {
      switch (message.type) {
        case "register": {
          const { name, addr } = message.payload as { name: string; addr: Address };
          data.actors.set(name, addr);
          console.log(`[${data.name}] Registered ${name}`);
          return { success: true };
        }

        case "list": {
          return {
            success: true,
            data: {
              group: data.name,
              actors: Array.from(data.actors.keys()),
              count: data.actors.size,
            },
          };
        }

        case "route": {
          const { targetName, message: innerMsg } = message.payload as {
            targetName: string;
            message: Message;
          };

          const targetAddr = data.actors.get(targetName);
          if (!targetAddr) {
            return {
              success: false,
              error: `Actor ${targetName} not found in ${data.name}`,
            };
          }

          // Forward message using Address.send()
          return targetAddr.send(innerMsg);
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

// Logger actor that can send to any address
const LoggerActor: ActorFactory<{
  name: string;
  targets: Map<string, Address>;
  system: SystemType;
}> = (data) => {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "log") {
        const { targetName, text } = message.payload as {
          targetName: string;
          text: string;
        };

        const targetAddr = data.targets.get(targetName);
        if (!targetAddr) {
          return {
            success: false,
            error: `Target ${targetName} not found`,
          };
        }

        console.log(`[${data.name}] Logging to ${targetName}: ${text}`);

        // Send to target using Address.send()
        const result = await targetAddr.send({
          id: crypto.randomUUID(),
          type: "echo",
          payload: { text },
        });

        return result;
      }

      return { success: true };
    },
  };
  return data.system.register(actor);
};

async function main() {
  console.log("=== Hierarchical Actor Organization (Address Proxy Pattern) ===\n");

  const system = System();

  // Create router actors for logical grouping
  console.log("Creating routers for Service A and Service B...");

  const serviceAActors = new Map<string, Address>();
  const serviceBActors = new Map<string, Address>();

  const serviceARouter = RouterActor({
    name: "ServiceA-Router",
    actors: serviceAActors,
    system,
  });

  const serviceBRouter = RouterActor({
    name: "ServiceB-Router",
    actors: serviceBActors,
    system,
  });

  // Create echo actors in Service A
  const echoA1 = EchoActor({ prefix: "ServiceA-Echo1", system });
  const echoA2 = EchoActor({ prefix: "ServiceA-Echo2", system });

  // Register in Service A group
  await serviceARouter.send({
    id: crypto.randomUUID(),
    type: "register",
    payload: { name: "echo-1", addr: echoA1 },
  });

  await serviceARouter.send({
    id: crypto.randomUUID(),
    type: "register",
    payload: { name: "echo-2", addr: echoA2 },
  });

  console.log("Created actors in Service A");

  // Create echo actor and logger in Service B
  const echoB1 = EchoActor({ prefix: "ServiceB-Echo1", system });

  const loggerBTargets = new Map<string, Address>();
  loggerBTargets.set("echo-1", echoB1);
  loggerBTargets.set("serviceA-router", serviceARouter);

  const loggerB = LoggerActor({
    name: "ServiceB-Logger",
    targets: loggerBTargets,
    system,
  });

  await serviceBRouter.send({
    id: crypto.randomUUID(),
    type: "register",
    payload: { name: "echo-1", addr: echoB1 },
  });

  await serviceBRouter.send({
    id: crypto.randomUUID(),
    type: "register",
    payload: { name: "logger", addr: loggerB },
  });

  console.log("Created actors in Service B\n");

  // Test 1: Query router stats
  console.log("Test 1: Query router stats");
  const statsA = await serviceARouter.send({
    id: crypto.randomUUID(),
    type: "list",
    payload: {},
  });
  console.log("ServiceA:", statsA.data);

  const statsB = await serviceBRouter.send({
    id: crypto.randomUUID(),
    type: "list",
    payload: {},
  });
  console.log("ServiceB:", statsB.data);

  // Test 2: Route message through router to actor
  console.log("\nTest 2: Route through router to actor");
  const routeResult = await serviceARouter.send({
    id: crypto.randomUUID(),
    type: "route",
    payload: {
      targetName: "echo-1",
      message: {
        id: crypto.randomUUID(),
        type: "echo",
        payload: { text: "Hello from router!" },
      },
    },
  });
  console.log("Route result:", routeResult.data);

  // Test 3: Direct actor-to-actor communication (no router needed!)
  console.log("\nTest 3: Direct actor-to-actor communication");
  console.log("Actors hold Address references - can send directly!");

  const directResult = await echoA1.send({
    id: crypto.randomUUID(),
    type: "echo",
    payload: { text: "Direct message!" },
  });
  console.log("Direct result:", directResult.data);

  // Test 4: Cross-group communication via logger
  console.log("\nTest 4: Cross-group communication");
  await loggerB.send({
    id: crypto.randomUUID(),
    type: "log",
    payload: {
      targetName: "echo-1", // Sends to its own echo-1
      text: "Hello from logger",
    },
  });

  // Test 5: Create global logger with access to all addresses
  console.log("\nTest 5: Global logger with all addresses");
  const globalTargets = new Map<string, Address>();
  globalTargets.set("serviceA-router", serviceARouter);
  globalTargets.set("serviceB-router", serviceBRouter);
  globalTargets.set("echoA1", echoA1);
  globalTargets.set("echoA2", echoA2);
  globalTargets.set("echoB1", echoB1);

  const globalLogger = LoggerActor({
    name: "Global-Logger",
    targets: globalTargets,
    system,
  });

  await globalLogger.send({
    id: crypto.randomUUID(),
    type: "log",
    payload: {
      targetName: "echoA1",
      text: "Hello from global logger",
    },
  });

  // Can also route through routers
  await globalLogger.send({
    id: crypto.randomUUID(),
    type: "log",
    payload: {
      targetName: "serviceA-router",
      text: "Hello to Service A",
    },
  });

  console.log("\n=== Key Insights ===");
  console.log("1. Router actors provide logical grouping and routing");
  console.log("2. Actors can communicate directly via Address references");
  console.log("3. No magic strings - all references are type-safe Addresses");
  console.log("4. Composition through Address references, not system nesting");
  console.log("5. Single System, multiple organizational patterns");
}

main().catch(console.error);
