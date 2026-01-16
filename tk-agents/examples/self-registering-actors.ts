// Self-Registering Actors - Actor sends metadata to System
// Actor takes responsibility for its own registration

import type { Message, Response, ActorType } from "../src/actors/base";

// Actor is JUST a function
type ActorFn = (message: Message) => Promise<Response>;

// System State
interface SystemState {
  id: string;
  actors: Map<string, ActorFn>;  // Just stores ID -> function!
}

// ============================================================================
// System (Simplified - doesn't need to know about actor structure)
// ============================================================================

function System(id: string = "system"): ActorFn {
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
            actors: Array.from(state.actors.keys())  // Just return IDs
          }
        };

      case 'register':
        // Actor sends: { id, type, receive }
        const { id: actorId, type, receive: actorReceive } = message.payload as {
          id: string;
          type: ActorType;
          receive: ActorFn;
        };

        if (state.actors.has(actorId)) {
          return { success: false, error: `Actor already registered: ${actorId}` };
        }

        state.actors.set(actorId, actorReceive);
        return { success: true, data: { actorId, type } };

      case 'route':
        const { targetId, message: innerMessage } = message.payload as {
          targetId: string;
          message: Message;
        };
        const targetActor = state.actors.get(targetId);
        if (!targetActor) {
          return { success: false, error: `Actor not found: ${targetId}` };
        }
        return targetActor(innerMessage);

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  };

  return receive;
}

// ============================================================================
// Self-Registering Echo Actor
// ============================================================================

function EchoActor(id: string, system: ActorFn): ActorFn {
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

  // Actor registers itself by sending message to System!
  system({
    id: crypto.randomUUID(),
    type: 'register',
    payload: {
      id,
      type: "deterministic" as ActorType,
      receive
    }
  });

  return receive;
}

// ============================================================================
// Self-Registering Coordinator
// ============================================================================

function CoordinatorActor(id: string, workerIds: string[], system: ActorFn): ActorFn {
  const receive: ActorFn = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    if (message.type !== 'coordinate') {
      return { success: false, error: 'Unknown message type' };
    }

    console.log(`  ${id}: coordinating ${workerIds.length} workers`);

    // Send to all workers
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

  // Self-register
  system({
    id: crypto.randomUUID(),
    type: 'register',
    payload: { id, type: "deterministic" as ActorType, receive }
  });

  return receive;
}

// ============================================================================
// Demos
// ============================================================================

async function demo1_SelfRegistering() {
  console.log("\n=== Demo 1: Self-Registering Actors ===\n");

  const system = System();

  // Actor registers itself when created!
  const echo = EchoActor("echo-1", system);

  console.log("List actors:");
  const listResponse = await system({
    id: "list-1",
    type: "list",
    payload: {}
  });
  console.log(listResponse);

  // Send message
  console.log("\nSending message to echo-1...");
  const response = await system({
    id: "msg-1",
    type: 'route',
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-2",
        type: "echo",
        payload: "Hello, self-registering actor!",
        sender: "external"
      }
    }
  });

  console.log("Response:", JSON.stringify(response, null, 2));
}

async function demo2_MultipleActors() {
  console.log("\n=== Demo 2: Multiple Self-Registering Actors ===\n");

  const system = System();

  // All actors register themselves when created
  const worker1 = EchoActor("worker-1", system);
  const worker2 = EchoActor("worker-2", system);
  const worker3 = EchoActor("worker-3", system);
  const coordinator = CoordinatorActor("coordinator", ["worker-1", "worker-2", "worker-3"], system);

  console.log("List all actors:");
  const listResponse = await system({
    id: "list-1",
    type: "list",
    payload: {}
  });
  console.log(listResponse);

  // Send coordination request
  console.log("\nSending coordination request...");
  const response = await system({
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

async function demo3_MultipleSystemsHint() {
  console.log("\n=== Demo 3: Actor in Multiple Systems (Hint) ===\n");

  const system1 = System("system-1");
  const system2 = System("system-2");

  // Create actor function
  const receive: ActorFn = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, from: "shared-actor" } };
    }
    return { success: true, data: { echo: message.payload } };
  };

  // Register in BOTH systems by sending register messages
  await system1({
    id: "reg-1",
    type: "register",
    payload: { id: "shared-actor", type: "deterministic" as ActorType, receive }
  });

  await system2({
    id: "reg-2",
    type: "register",
    payload: { id: "shared-actor", type: "deterministic" as ActorType, receive }
  });

  console.log("Ping via system1:");
  const response1 = await system1({
    id: "ping-1",
    type: "route",
    payload: {
      targetId: "shared-actor",
      message: { id: "msg-1", type: "ping", payload: {} }
    }
  });
  console.log(response1);

  console.log("\nPing via system2:");
  const response2 = await system2({
    id: "ping-2",
    type: "route",
    payload: {
      targetId: "shared-actor",
      message: { id: "msg-2", type: "ping", payload: {} }
    }
  });
  console.log(response2);

  console.log("\n✓ Same actor function callable from multiple systems!");
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║              Self-Registering Actors Pattern                  ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_SelfRegistering();
  await demo2_MultipleActors();
  await demo3_MultipleSystemsHint();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Key Benefits:                                                 ║");
  console.log("║                                                                ║");
  console.log("║  1. Actors are JUST FUNCTIONS (simplest possible)             ║");
  console.log("║  2. Actors TELL System their metadata (not extracted)         ║");
  console.log("║  3. Self-registration at creation time                        ║");
  console.log("║  4. Actors can register with MULTIPLE systems                 ║");
  console.log("║  5. System doesn't need to know actor structure               ║");
  console.log("║  6. Everything happens through MESSAGES                       ║");
  console.log("║                                                                ║");
  console.log("║  This is more Hewitt-compliant - pure message passing!        ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

if (import.meta.main) {
  main().catch(console.error);
}

export { System, EchoActor, CoordinatorActor };
