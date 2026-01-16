// System as Function - System is an actor that wraps a Map
// System is itself just an actor function, not a special type

import type { Message, Response } from "../src/actors/base";

// Actor = just a function
type Actor = (message: Message) => Promise<Response>;

// ============================================================================
// System - An Actor Function that manages other actors
// ============================================================================

function System(): Actor {
  // Private state (closure)
  const actors = new Map<string, Actor>();

  // Return the actor function
  return async (message: Message): Promise<Response> => {
    switch (message.type) {
      case 'ping':
        return { success: true, data: { alive: true, timestamp: Date.now() } };

      case 'register': {
        const { id, actor } = message.payload as { id: string; actor: Actor };
        if (actors.has(id)) {
          return { success: false, error: `Actor already registered: ${id}` };
        }
        actors.set(id, actor);
        return { success: true, data: { actorId: id } };
      }

      case 'unregister': {
        const { id } = message.payload as { id: string };
        const existed = actors.delete(id);
        return existed
          ? { success: true, data: { actorId: id } }
          : { success: false, error: `Actor not found: ${id}` };
      }

      case 'list':
        return {
          success: true,
          data: { actors: Array.from(actors.keys()) }
        };

      case 'route': {
        const { targetId, message: innerMessage } = message.payload as {
          targetId: string;
          message: Message;
        };
        const actor = actors.get(targetId);
        if (!actor) {
          return { success: false, error: `Actor not found: ${targetId}` };
        }
        return actor(innerMessage);
      }

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  };
}

// ============================================================================
// Echo Actor
// ============================================================================

function EchoActor(id: string): Actor {
  return async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    return {
      success: true,
      data: {
        echo: message.payload,
        receivedFrom: message.sender,
        actorId: id
      }
    };
  };
}

// ============================================================================
// Coordinator Actor
// ============================================================================

function CoordinatorActor(id: string, workerIds: string[], system: Actor): Actor {
  return async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    if (message.type !== 'coordinate') {
      return { success: false, error: 'Unknown message type' };
    }

    console.log(`  ${id}: coordinating ${workerIds.length} workers`);

    // Send to all workers via system
    const results = await Promise.all(
      workerIds.map(async (workerId, index) => {
        console.log(`  ${id}: sending to worker ${workerId}`);

        const response = await system({
          id: crypto.randomUUID(),
          type: 'route',
          payload: {
            targetId: workerId,
            message: {
              id: crypto.randomUUID(),
              type: 'work',
              payload: { task: message.payload, workerId: index },
              sender: id
            }
          }
        });

        return { workerId, response: response.data };
      })
    );

    return {
      success: true,
      data: {
        coordinatedBy: id,
        workerCount: workerIds.length,
        results
      }
    };
  };
}

// ============================================================================
// Chain Actor
// ============================================================================

function ChainActor(id: string, nextActorId: string | undefined, system: Actor): Actor {
  return async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    const processed = `[${id}] processed: ${String(message.payload)}`;
    console.log(`  ${id}: received from ${message.sender}`);
    console.log(`  ${id}: processing...`);

    if (nextActorId) {
      console.log(`  ${id}: forwarding to ${nextActorId}`);

      const response = await system({
        id: crypto.randomUUID(),
        type: 'route',
        payload: {
          targetId: nextActorId,
          message: {
            id: crypto.randomUUID(),
            type: 'process',
            payload: processed,
            sender: id
          }
        }
      });

      return {
        success: true,
        data: {
          processed,
          forwardedTo: nextActorId,
          finalResult: response.data
        }
      };
    }

    console.log(`  ${id}: end of chain`);
    return { success: true, data: { processed, endOfChain: true } };
  };
}

// ============================================================================
// Demos
// ============================================================================

async function demo1_SystemIsActor() {
  console.log("\n=== Demo 1: System IS an Actor ===\n");

  // System is just an actor function
  const system = System();

  // Ping the system directly
  console.log("Ping system:");
  const pingResponse = await system({
    id: "ping-1",
    type: "ping",
    payload: {}
  });
  console.log(pingResponse);

  // Register an actor
  const echo = EchoActor("echo-1");
  console.log("\nRegister echo-1:");
  const regResponse = await system({
    id: "reg-1",
    type: "register",
    payload: { id: "echo-1", actor: echo }
  });
  console.log(regResponse);

  // List actors
  console.log("\nList actors:");
  const listResponse = await system({
    id: "list-1",
    type: "list",
    payload: {}
  });
  console.log(listResponse);

  // Route message to actor
  console.log("\nRoute to echo-1:");
  const routeResponse = await system({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-1",
        type: "echo",
        payload: "Hello!",
        sender: "external"
      }
    }
  });
  console.log(routeResponse);
}

async function demo2_Coordination() {
  console.log("\n=== Demo 2: Coordination ===\n");

  const system = System();

  // Create and register workers
  const worker1 = EchoActor("worker-1");
  const worker2 = EchoActor("worker-2");
  const worker3 = EchoActor("worker-3");

  await system({ id: "r1", type: "register", payload: { id: "worker-1", actor: worker1 } });
  await system({ id: "r2", type: "register", payload: { id: "worker-2", actor: worker2 } });
  await system({ id: "r3", type: "register", payload: { id: "worker-3", actor: worker3 } });

  // Create and register coordinator
  const coordinator = CoordinatorActor("coordinator", ["worker-1", "worker-2", "worker-3"], system);
  await system({ id: "r4", type: "register", payload: { id: "coordinator", actor: coordinator } });

  console.log("Sending coordination request...\n");
  const response = await system({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "coordinator",
      message: {
        id: "msg-1",
        type: "coordinate",
        payload: "Task for all workers",
        sender: "external"
      }
    }
  });

  console.log("\nResult:", JSON.stringify(response, null, 2));
}

async function demo3_ChainPattern() {
  console.log("\n=== Demo 3: Chain Pattern ===\n");

  const system = System();

  // Create chain actors
  const actor3 = ChainActor("chain-3", undefined, system);
  const actor2 = ChainActor("chain-2", "chain-3", system);
  const actor1 = ChainActor("chain-1", "chain-2", system);

  // Register all
  await system({ id: "r1", type: "register", payload: { id: "chain-1", actor: actor1 } });
  await system({ id: "r2", type: "register", payload: { id: "chain-2", actor: actor2 } });
  await system({ id: "r3", type: "register", payload: { id: "chain-3", actor: actor3 } });

  console.log("Sending to chain-1...\n");
  const response = await system({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "chain-1",
      message: {
        id: "msg-1",
        type: "process",
        payload: "Initial data",
        sender: "external"
      }
    }
  });

  console.log("\nFinal Response:", JSON.stringify(response, null, 2));
}

async function demo4_NestedSystems() {
  console.log("\n=== Demo 4: Nested Systems (Systems ARE Actors!) ===\n");

  // Create root system
  const rootSystem = System();

  // Create subsystems (they're just actors!)
  const subsystem1 = System();
  const subsystem2 = System();

  // Register subsystems IN the root system
  await rootSystem({
    id: "reg-sub1",
    type: "register",
    payload: { id: "subsystem-1", actor: subsystem1 }
  });

  await rootSystem({
    id: "reg-sub2",
    type: "register",
    payload: { id: "subsystem-2", actor: subsystem2 }
  });

  // Register actors in subsystems
  const actor1 = EchoActor("actor-1");
  const actor2 = EchoActor("actor-2");

  await subsystem1({
    id: "reg-a1",
    type: "register",
    payload: { id: "actor-1", actor: actor1 }
  });

  await subsystem2({
    id: "reg-a2",
    type: "register",
    payload: { id: "actor-2", actor: actor2 }
  });

  // List actors in root
  console.log("Root system actors:");
  const rootList = await rootSystem({ id: "list-1", type: "list", payload: {} });
  console.log(rootList);

  // List actors in subsystem1
  console.log("\nSubsystem-1 actors:");
  const sub1List = await subsystem1({ id: "list-2", type: "list", payload: {} });
  console.log(sub1List);

  // Route through root -> subsystem -> actor
  console.log("\nRoute root -> subsystem-1 -> actor-1:");
  const response = await rootSystem({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "subsystem-1",
      message: {
        id: "route-2",
        type: "route",
        payload: {
          targetId: "actor-1",
          message: {
            id: "msg-1",
            type: "echo",
            payload: "Hello from nested system!",
            sender: "root"
          }
        }
      }
    }
  });

  console.log(response);
}

async function demo5_SystemComposition() {
  console.log("\n=== Demo 5: System Composition (Higher-Order Systems) ===\n");

  // Create a logging wrapper for any actor
  function withLogging(name: string, actor: Actor): Actor {
    return async (message: Message): Promise<Response> => {
      console.log(`[${name}] ← ${message.type}`);
      const response = await actor(message);
      console.log(`[${name}] → ${response.success ? "✓" : "✗"}`);
      return response;
    };
  }

  // Wrap a system with logging
  const baseSystem = System();
  const loggedSystem = withLogging("system", baseSystem);

  // Register actor
  const echo = EchoActor("echo-1");
  await loggedSystem({
    id: "reg-1",
    type: "register",
    payload: { id: "echo-1", actor: echo }
  });

  console.log("\nRoute to echo-1:");
  const response = await loggedSystem({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-1",
        type: "echo",
        payload: "Test logging",
        sender: "external"
      }
    }
  });

  console.log("\nResponse:", response);
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║           System as Actor Function (Pure Functions)           ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_SystemIsActor();
  await demo2_Coordination();
  await demo3_ChainPattern();
  await demo4_NestedSystems();
  await demo5_SystemComposition();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Key Benefits:                                                 ║");
  console.log("║                                                                ║");
  console.log("║  1. System IS an actor (just a function)                      ║");
  console.log("║  2. Actors are JUST FUNCTIONS                                 ║");
  console.log("║  3. Everything is uniform - no special types                  ║");
  console.log("║  4. Nested systems work naturally (systems in systems)        ║");
  console.log("║  5. Composable with higher-order functions                    ║");
  console.log("║  6. System's Map is PRIVATE (encapsulated in closure)        ║");
  console.log("║  7. Pure message passing - everything via messages           ║");
  console.log("║                                                                ║");
  console.log("║  This is the purest actor model - uniform composition!        ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

if (import.meta.main) {
  main().catch(console.error);
}

export { System, EchoActor, CoordinatorActor, ChainActor };
