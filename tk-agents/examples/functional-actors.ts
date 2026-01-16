// Functional Actor Design Example
// Uses React FunctionalComponent pattern (EchoActor instead of createEchoActor)

import type { Message, Response, ActorType } from "../src/actors/base";

// ============================================================================
// Core Types
// ============================================================================

// Actor is just a function
type ActorFn = (message: Message) => Promise<Response>;

// Actor State (what was hidden in classes)
interface ActorState {
  id: string;
  type: ActorType;
  receive: ActorFn;
}

// System State
interface SystemState {
  id: string;
  actors: Map<string, ActorState>;
}

// ============================================================================
// System (Functional Component Pattern)
// ============================================================================

function System(id: string = "system"): ActorState {
  const state: SystemState = {
    id,
    actors: new Map(),
  };

  const receive: ActorFn = async (message: Message): Promise<Response> => {
    switch (message.type) {
      case 'ping':
        return { success: true, data: { alive: true, timestamp: Date.now() } };

      case 'list':
        return {
          success: true,
          data: {
            actors: Array.from(state.actors.values()).map(a => ({
              id: a.id,
              type: a.type
            }))
          }
        };

      case 'register':
        const { actor } = message.payload as { actor: ActorState };
        if (state.actors.has(actor.id)) {
          return { success: false, error: `Actor already registered: ${actor.id}` };
        }
        state.actors.set(actor.id, actor);
        return { success: true, data: { actorId: actor.id } };

      case 'unregister':
        const { actorId } = message.payload as { actorId: string };
        const existed = state.actors.delete(actorId);
        return existed
          ? { success: true, data: { actorId } }
          : { success: false, error: `Actor not found: ${actorId}` };

      case 'route':
        const { targetId, message: innerMessage } = message.payload as {
          targetId: string;
          message: Message;
        };
        const targetActor = state.actors.get(targetId);
        if (!targetActor) {
          return { success: false, error: `Actor not found: ${targetId}` };
        }
        return targetActor.receive(innerMessage);

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  };

  return { id, type: "deterministic", receive };
}

// ============================================================================
// Simple Actor (Functional Component Pattern)
// ============================================================================

function EchoActor(id: string): ActorState {
  const receive: ActorFn = async (message: Message): Promise<Response> => {
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

  return { id, type: "deterministic", receive };
}

// ============================================================================
// Coordinating Actor (Gets System via Closure)
// ============================================================================

function CoordinatorActor(
  id: string,
  workerIds: string[],
  systemReceive: ActorFn  // System passed as closure!
): ActorState {
  const receive: ActorFn = async (message: Message): Promise<Response> => {
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

        const response = await systemReceive({
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

  return { id, type: "deterministic", receive };
}

// ============================================================================
// Chain Actor (Functional Composition)
// ============================================================================

function ChainActor(
  id: string,
  nextActorId: string | undefined,
  systemReceive: ActorFn
): ActorState {
  const receive: ActorFn = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    const processed = `[${id}] processed: ${String(message.payload)}`;
    console.log(`  ${id}: received from ${message.sender}`);
    console.log(`  ${id}: processing...`);

    if (nextActorId) {
      console.log(`  ${id}: forwarding to ${nextActorId}`);

      const response = await systemReceive({
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

  return { id, type: "deterministic", receive };
}

// ============================================================================
// Demo Functions
// ============================================================================

async function demo1_BasicReceive() {
  console.log("\n=== Demo 1: Basic Receive (Functional) ===\n");

  const system = System();
  const echo = EchoActor("echo-1");

  // Register actor
  await system.receive({
    id: "reg-1",
    type: 'register',
    payload: { actor: echo }
  });

  // Send message
  console.log("Sending message to echo-1...");
  const response = await system.receive({
    id: "msg-1",
    type: 'route',
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-2",
        type: "echo",
        payload: "Hello, functional actors!",
        sender: "external"
      }
    }
  });

  console.log("Response:", JSON.stringify(response, null, 2));
}

async function demo2_CoordinationPattern() {
  console.log("\n=== Demo 2: Coordination (Functional) ===\n");

  const system = System();

  // Create workers
  const worker1 = EchoActor("worker-1");
  const worker2 = EchoActor("worker-2");
  const worker3 = EchoActor("worker-3");

  // Create coordinator with system closure
  const coordinator = CoordinatorActor(
    "coordinator",
    ["worker-1", "worker-2", "worker-3"],
    system.receive  // ← System passed as closure!
  );

  // Register all
  await system.receive({ id: "r1", type: 'register', payload: { actor: worker1 } });
  await system.receive({ id: "r2", type: 'register', payload: { actor: worker2 } });
  await system.receive({ id: "r3", type: 'register', payload: { actor: worker3 } });
  await system.receive({ id: "r4", type: 'register', payload: { actor: coordinator } });

  // Send coordination request
  console.log("Sending coordination request...\n");
  const response = await system.receive({
    id: "msg-coord",
    type: 'route',
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

  console.log("\nCoordination Result:", JSON.stringify(response, null, 2));
}

async function demo3_ChainPattern() {
  console.log("\n=== Demo 3: Chain Pattern (Functional) ===\n");

  const system = System();

  // Create chain: actor1 -> actor2 -> actor3
  // Note: Pass system.receive to each for inter-actor communication
  const actor3 = ChainActor("chain-3", undefined, system.receive);
  const actor2 = ChainActor("chain-2", "chain-3", system.receive);
  const actor1 = ChainActor("chain-1", "chain-2", system.receive);

  // Register all
  await system.receive({ id: "r1", type: 'register', payload: { actor: actor1 } });
  await system.receive({ id: "r2", type: 'register', payload: { actor: actor2 } });
  await system.receive({ id: "r3", type: 'register', payload: { actor: actor3 } });

  // Send to first actor
  console.log("Sending to chain-1...\n");
  const response = await system.receive({
    id: "msg-chain",
    type: 'route',
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

async function demo4_EmptySystem() {
  console.log("\n=== Demo 4: Empty System (System IS an Actor) ===\n");

  const system = System();

  console.log("Pinging empty system...");
  const pingResponse = await system.receive({
    id: "ping-1",
    type: "ping",
    payload: {}
  });
  console.log("Ping response:", pingResponse);

  console.log("\nListing actors in empty system...");
  const listResponse = await system.receive({
    id: "list-1",
    type: "list",
    payload: {}
  });
  console.log("List response:", listResponse);

  console.log("\n✓ System works standalone - no actors required!");
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║           Functional Actor Design (No Classes)                 ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_BasicReceive();
  await demo2_CoordinationPattern();
  await demo3_ChainPattern();
  await demo4_EmptySystem();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Key Differences from Class-Based:                            ║");
  console.log("║                                                                ║");
  console.log("║  1. Actors are FUNCTIONS + STATE (not classes)                ║");
  console.log("║  2. System passed via CLOSURE (not setSystem injection)       ║");
  console.log("║  3. Single-phase initialization (no two-step setup)           ║");
  console.log("║  4. No BaseActor needed (use factory functions)               ║");
  console.log("║  5. State is EXPLICIT (visible in function signatures)        ║");
  console.log("║                                                                ║");
  console.log("║  See FUNCTIONAL_ACTOR_DESIGN.md for detailed analysis         ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

if (import.meta.main) {
  main().catch(console.error);
}

export { System, EchoActor, CoordinatorActor, ChainActor };
