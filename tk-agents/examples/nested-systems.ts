/**
 * Nested Systems Example
 *
 * Demonstrates:
 * - Systems ARE actors (uniform composition)
 * - Subsystems can be registered in parent systems
 * - Message routing through hierarchy
 */

import { createSystem } from "../src/actors-new/index.ts";
import type { ActorFactory, Message } from "../src/actors-new/index.ts";

// Simple echo actor
const EchoActor: ActorFactory<{ prefix: string }> = (data, send) => ({
  send: async (message: Message) => {
    console.log(`[${data.prefix}] Received: ${message.type}`);
    return {
      success: true,
      data: `${data.prefix}: ${JSON.stringify(message.payload)}`,
    };
  },
});

// Logger actor that sends to other actors
const LoggerActor: ActorFactory<{ name: string }> = (data, send) => ({
  send: async (message: Message) => {
    if (message.type === "log") {
      const { targetId, text } = message.payload as {
        targetId: string;
        text: string;
      };
      console.log(`[${data.name}] Logging to ${targetId}: ${text}`);

      // Send to target using injected send
      const result = await send(targetId, {
        id: crypto.randomUUID(),
        type: "echo",
        payload: { text },
      });

      return result;
    }

    return { success: true };
  },
});

async function main() {
  console.log("=== Nested Systems Example ===\n");

  // Create root system
  const root = createSystem();
  console.log("Created root system");

  // Create subsystem for service A
  const serviceA = createSystem();
  console.log("Created serviceA subsystem");

  // Create subsystem for service B
  const serviceB = createSystem();
  console.log("Created serviceB subsystem");

  // Create actors in each subsystem
  const echoA1 = EchoActor({ prefix: "ServiceA-Echo1" }, serviceA.send);
  const echoA2 = EchoActor({ prefix: "ServiceA-Echo2" }, serviceA.send);
  serviceA.register("echo-1", echoA1);
  serviceA.register("echo-2", echoA2);
  console.log("Registered actors in serviceA");

  const echoB1 = EchoActor({ prefix: "ServiceB-Echo1" }, serviceB.send);
  const loggerB = LoggerActor({ name: "ServiceB-Logger" }, serviceB.send);
  serviceB.register("echo-1", echoB1);
  serviceB.register("logger", loggerB);
  console.log("Registered actors in serviceB");

  // Register subsystems in root (systems ARE actors!)
  root.register("service-a", serviceA);
  root.register("service-b", serviceB);
  console.log("Registered subsystems in root\n");

  // Test 1: Send directly to subsystem
  console.log("Test 1: Query subsystem stats");
  const statsA = await root.send("service-a", {
    id: crypto.randomUUID(),
    type: "stats",
    payload: {},
  });
  console.log("ServiceA stats:", statsA.data);

  const statsB = await root.send("service-b", {
    id: crypto.randomUUID(),
    type: "stats",
    payload: {},
  });
  console.log("ServiceB stats:", statsB.data);

  // Test 2: Route message through subsystem to actor
  console.log("\nTest 2: Route through subsystem hierarchy");
  const routeResult = await root.send("service-a", {
    id: crypto.randomUUID(),
    type: "route",
    payload: {
      targetId: "echo-1",
      message: {
        id: crypto.randomUUID(),
        type: "echo",
        payload: { text: "Hello from root!" },
      },
    },
  });
  console.log("Route result:", routeResult);

  // Test 3: Actor in one subsystem cannot directly reach another subsystem
  // (they would need access to root's send function)
  console.log(
    "\nTest 3: Cross-subsystem communication requires shared send scope"
  );
  console.log(
    "Actors in serviceA have serviceA.send, so they can only reach serviceA actors"
  );
  console.log(
    "To enable cross-subsystem communication, inject root.send instead"
  );

  // Test 4: Create actor with root.send for cross-subsystem communication
  console.log("\nTest 4: Cross-subsystem communication with root.send");
  const crossLogger = LoggerActor({ name: "Cross-Logger" }, root.send);
  root.register("cross-logger", crossLogger);

  await crossLogger.send({
    id: crypto.randomUUID(),
    type: "log",
    payload: {
      targetId: "service-a", // Can reach subsystems!
      text: "Hello from cross-logger",
    },
  });

  // Test 5: List actors in subsystems
  console.log("\nTest 5: List actors in subsystems");
  const listA = await serviceA.send({
    id: crypto.randomUUID(),
    type: "list",
    payload: {},
  });
  console.log("ServiceA actors:", listA.data);

  const listB = await serviceB.send({
    id: crypto.randomUUID(),
    type: "list",
    payload: {},
  });
  console.log("ServiceB actors:", listB.data);

  console.log("\n=== Key Insights ===");
  console.log("1. Systems ARE actors - they have .send() method");
  console.log("2. Systems can be registered in other systems");
  console.log(
    "3. Send scope determines reachability - inject appropriate send function"
  );
  console.log("4. Uniform composition enables hierarchical organization");
}

main().catch(console.error);
