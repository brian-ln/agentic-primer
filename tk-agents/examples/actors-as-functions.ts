// Alternative 2: Actors Are Just Functions
// Simplest possible - actors are pure functions, no metadata objects

import type { Message, Response } from "../src/actors/base";

// Actor is JUST a function
type Actor = (message: Message) => Promise<Response>;

// System is JUST a Map of ID -> function
type System = Map<string, Actor>;

// ============================================================================
// System Operations (No class needed!)
// ============================================================================

function createSystem(): System {
  return new Map();
}

// Register actor with explicit ID
function register(system: System, id: string, actor: Actor): void {
  system.set(id, actor);
}

// Send message to actor by ID
async function sendTo(system: System, id: string, message: Message): Promise<Response> {
  const actor = system.get(id);
  if (!actor) {
    return { success: false, error: `Actor not found: ${id}` };
  }
  return actor(message);
}

// List all actor IDs
function listActors(system: System): string[] {
  return Array.from(system.keys());
}

// ============================================================================
// Echo Actor - Just a Function Factory
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
// Coordinator Actor - Gets System via Closure
// ============================================================================

function CoordinatorActor(id: string, workerIds: string[], system: System): Actor {
  return async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    if (message.type !== 'coordinate') {
      return { success: false, error: 'Unknown message type' };
    }

    console.log(`  ${id}: coordinating ${workerIds.length} workers`);

    // Send to all workers via system (closure)
    const results = await Promise.all(
      workerIds.map(async (workerId, index) => {
        console.log(`  ${id}: sending to worker ${workerId}`);

        const response = await sendTo(system, workerId, {
          id: crypto.randomUUID(),
          type: 'work',
          payload: { task: message.payload, workerId: index },
          sender: id
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

function ChainActor(id: string, nextActorId: string | undefined, system: System): Actor {
  return async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    const processed = `[${id}] processed: ${String(message.payload)}`;
    console.log(`  ${id}: received from ${message.sender}`);
    console.log(`  ${id}: processing...`);

    if (nextActorId) {
      console.log(`  ${id}: forwarding to ${nextActorId}`);

      const response = await sendTo(system, nextActorId, {
        id: crypto.randomUUID(),
        type: 'process',
        payload: processed,
        sender: id
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

async function demo1_BasicUsage() {
  console.log("\n=== Demo 1: Actors as Pure Functions ===\n");

  // System is just a Map
  const system = createSystem();

  // Actor is just a function
  const echo = EchoActor("echo-1");

  // Register with explicit ID
  register(system, "echo-1", echo);

  console.log("Actors:", listActors(system));

  // Can call actor directly!
  console.log("\nCalling actor directly:");
  const directResponse = await echo({
    id: "msg-1",
    type: "echo",
    payload: "Direct call!",
    sender: "external"
  });
  console.log(directResponse);

  // Or via system
  console.log("\nCalling via system:");
  const systemResponse = await sendTo(system, "echo-1", {
    id: "msg-2",
    type: "echo",
    payload: "Via system!",
    sender: "external"
  });
  console.log(systemResponse);
}

async function demo2_Coordination() {
  console.log("\n=== Demo 2: Coordination Pattern ===\n");

  const system = createSystem();

  // Create workers (just functions)
  const worker1 = EchoActor("worker-1");
  const worker2 = EchoActor("worker-2");
  const worker3 = EchoActor("worker-3");

  // Register workers
  register(system, "worker-1", worker1);
  register(system, "worker-2", worker2);
  register(system, "worker-3", worker3);

  // Create coordinator (gets system via closure)
  const coordinator = CoordinatorActor(
    "coordinator",
    ["worker-1", "worker-2", "worker-3"],
    system  // ← System passed as closure
  );

  register(system, "coordinator", coordinator);

  console.log("Actors:", listActors(system));

  // Send coordination request
  console.log("\nSending coordination request...");
  const response = await sendTo(system, "coordinator", {
    id: "msg-1",
    type: "coordinate",
    payload: "Task for all workers",
    sender: "external"
  });

  console.log("\nResult:", JSON.stringify(response, null, 2));
}

async function demo3_ChainPattern() {
  console.log("\n=== Demo 3: Chain Pattern ===\n");

  const system = createSystem();

  // Create chain: actor1 -> actor2 -> actor3
  const actor3 = ChainActor("chain-3", undefined, system);
  const actor2 = ChainActor("chain-2", "chain-3", system);
  const actor1 = ChainActor("chain-1", "chain-2", system);

  register(system, "chain-1", actor1);
  register(system, "chain-2", actor2);
  register(system, "chain-3", actor3);

  console.log("Sending to chain-1...\n");
  const response = await sendTo(system, "chain-1", {
    id: "msg-1",
    type: "process",
    payload: "Initial data",
    sender: "external"
  });

  console.log("\nFinal Response:", JSON.stringify(response, null, 2));
}

async function demo4_ActorComposition() {
  console.log("\n=== Demo 4: Function Composition ===\n");

  const system = createSystem();

  // Compose actors using higher-order functions
  function withLogging(id: string, actor: Actor): Actor {
    return async (message: Message): Promise<Response> => {
      console.log(`[${id}] Received:`, message.type);
      const response = await actor(message);
      console.log(`[${id}] Responded:`, response.success ? "✓" : "✗");
      return response;
    };
  }

  function withRetry(actor: Actor, maxRetries: number = 3): Actor {
    return async (message: Message): Promise<Response> => {
      for (let i = 0; i < maxRetries; i++) {
        const response = await actor(message);
        if (response.success) return response;
        console.log(`  Retry ${i + 1}/${maxRetries}`);
      }
      return { success: false, error: "Max retries exceeded" };
    };
  }

  // Create base actor
  const baseActor = EchoActor("echo-1");

  // Compose with logging and retry
  const composedActor = withLogging("echo-1", withRetry(baseActor));

  register(system, "echo-1", composedActor);

  console.log("Sending message...\n");
  const response = await sendTo(system, "echo-1", {
    id: "msg-1",
    type: "echo",
    payload: "Composed actor!",
    sender: "external"
  });

  console.log("\nResponse:", response);
}

async function demo5_SharedActor() {
  console.log("\n=== Demo 5: Shared Actor Across Systems ===\n");

  const system1 = createSystem();
  const system2 = createSystem();

  // Single actor function
  const sharedActor = EchoActor("shared");

  // Register in BOTH systems
  register(system1, "shared", sharedActor);
  register(system2, "shared", sharedActor);

  console.log("System 1 actors:", listActors(system1));
  console.log("System 2 actors:", listActors(system2));

  // Call via system1
  console.log("\nVia system1:");
  const response1 = await sendTo(system1, "shared", {
    id: "msg-1",
    type: "ping",
    payload: {}
  });
  console.log(response1);

  // Call via system2
  console.log("\nVia system2:");
  const response2 = await sendTo(system2, "shared", {
    id: "msg-2",
    type: "ping",
    payload: {}
  });
  console.log(response2);

  // Call directly (no system!)
  console.log("\nDirect call:");
  const response3 = await sharedActor({
    id: "msg-3",
    type: "ping",
    payload: {}
  });
  console.log(response3);
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║             Actors as Pure Functions (Alternative 2)          ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_BasicUsage();
  await demo2_Coordination();
  await demo3_ChainPattern();
  await demo4_ActorComposition();
  await demo5_SharedActor();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Key Benefits:                                                 ║");
  console.log("║                                                                ║");
  console.log("║  1. Actors are JUST FUNCTIONS - simplest possible             ║");
  console.log("║  2. System is JUST A MAP - no class needed                    ║");
  console.log("║  3. Can call actors DIRECTLY (no system required)             ║");
  console.log("║  4. Easy COMPOSITION with higher-order functions              ║");
  console.log("║  5. SHARED actors across multiple systems                     ║");
  console.log("║  6. Testable - just call the function                         ║");
  console.log("║  7. No inheritance, no classes, no complexity                 ║");
  console.log("║                                                                ║");
  console.log("║  This is the SIMPLEST actor model possible!                   ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

if (import.meta.main) {
  main().catch(console.error);
}

export { createSystem, register, sendTo, listActors, EchoActor, CoordinatorActor, ChainActor };
