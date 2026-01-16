// Hewitt Actor Model Example
//
// This example demonstrates proper Hewitt Actor Model semantics:
// 1. Actors RECEIVE messages (public interface)
// 2. Actors SEND through System (private implementation)
// 3. System IS an actor (uniform composition)
// 4. Location transparency (actors use addresses, not direct refs)
//
// This example uses the actual library implementations from src/actors/

import type { Actor, Message, Response } from "../src/actors/base";
import { System } from "../src/actors/system";
import { BaseActor } from "../src/actors/base-actor";

// ============================================================================
// Example 1: Simple Deterministic Actor
// ============================================================================

class EchoActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    // Handle ping
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Echo the payload back
    return {
      success: true,
      data: {
        echo: message.payload,
        receivedFrom: message.sender,
        actorId: this.id
      },
    };
  }
}

// ============================================================================
// Example 2: Actor That Sends to Others (Chain Pattern)
// ============================================================================

class ChainActor extends BaseActor {
  constructor(id: string, private nextActorId?: string) {
    super(id, "deterministic");
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    // Process message
    const processed = `[${this.id}] processed: ${String(message.payload)}`;

    console.log(`  ${this.id}: received from ${message.sender}`);
    console.log(`  ${this.id}: processing...`);

    // If there's a next actor, forward to it
    if (this.nextActorId) {
      console.log(`  ${this.id}: forwarding to ${this.nextActorId}`);

      const response = await this.send(this.nextActorId, {
        id: crypto.randomUUID(),
        type: 'process',
        payload: processed,
        sender: this.id
      });

      return {
        success: true,
        data: {
          processed,
          forwardedTo: this.nextActorId,
          finalResult: response.data
        }
      };
    }

    // Last actor in chain
    console.log(`  ${this.id}: end of chain`);
    return {
      success: true,
      data: { processed, endOfChain: true }
    };
  }
}

// ============================================================================
// Example 3: Coordinator Actor (Orchestration Pattern)
// ============================================================================

class CoordinatorActor extends BaseActor {
  constructor(id: string, private workerIds: string[]) {
    super(id, "deterministic");
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    if (message.type !== 'coordinate') {
      return { success: false, error: 'Unknown message type' };
    }

    console.log(`  ${this.id}: coordinating ${this.workerIds.length} workers`);

    // Send to all workers in parallel
    const results = await Promise.all(
      this.workerIds.map(async (workerId, index) => {
        console.log(`  ${this.id}: sending to worker ${workerId}`);

        const response = await this.send(workerId, {
          id: crypto.randomUUID(),
          type: 'work',
          payload: { task: message.payload, workerId: index },
          sender: this.id
        });

        return { workerId, response: response.data };
      })
    );

    return {
      success: true,
      data: {
        coordinatedBy: this.id,
        workerCount: this.workerIds.length,
        results
      }
    };
  }
}

// ============================================================================
// Example 4: Nested Systems (Supervision Tree Pattern)
// ============================================================================

// SubSystem is just another System (uniform composition!)
class SubSystem extends System {
  constructor(id: string) {
    super();
    // Override the id
    (this as { id: string }).id = id;
  }
}

// ============================================================================
// Demo Functions
// ============================================================================

async function demo1_BasicReceive() {
  console.log("\n=== Demo 1: Basic Receive Pattern ===\n");

  const system = new System();
  const echo = new EchoActor("echo-1");

  // Register actor
  await system.receive({
    id: "reg-1",
    type: 'register',
    payload: { actor: echo }
  });

  // Send message through System
  console.log("Sending message to echo-1...");
  const response = await system.receive({
    id: "msg-1",
    type: 'route',
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-2",
        type: "echo",
        payload: "Hello, Hewitt Actor Model!",
        sender: "external"
      }
    }
  });

  console.log("Response:", JSON.stringify(response, null, 2));
}

async function demo2_ChainPattern() {
  console.log("\n=== Demo 2: Chain Pattern (Actor-to-Actor via System) ===\n");

  const system = new System();

  // Create chain: actor1 -> actor2 -> actor3
  const actor3 = new ChainActor("chain-3"); // End of chain
  const actor2 = new ChainActor("chain-2", "chain-3");
  const actor1 = new ChainActor("chain-1", "chain-2");

  // Register all
  await system.receive({ id: "r1", type: 'register', payload: { actor: actor1 } });
  await system.receive({ id: "r2", type: 'register', payload: { actor: actor2 } });
  await system.receive({ id: "r3", type: 'register', payload: { actor: actor3 } });

  // Send to first actor in chain
  console.log("Sending to chain-1 (will propagate to chain-2 -> chain-3)...\n");
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

async function demo3_CoordinationPattern() {
  console.log("\n=== Demo 3: Coordination Pattern (Parallel Workers) ===\n");

  const system = new System();

  // Create workers
  const worker1 = new EchoActor("worker-1");
  const worker2 = new EchoActor("worker-2");
  const worker3 = new EchoActor("worker-3");

  // Create coordinator
  const coordinator = new CoordinatorActor("coordinator", ["worker-1", "worker-2", "worker-3"]);

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

async function demo4_NestedSystems() {
  console.log("\n=== Demo 4: Nested Systems (Supervision Tree) ===\n");

  const rootSystem = new System();
  const subSystem1 = new SubSystem("subsystem-1");
  const subSystem2 = new SubSystem("subsystem-2");

  // Register subsystems as actors in root system
  await rootSystem.receive({
    id: "r1",
    type: 'register',
    payload: { actor: subSystem1 }
  });
  await rootSystem.receive({
    id: "r2",
    type: 'register',
    payload: { actor: subSystem2 }
  });

  // Create actors in subsystems
  const actor1 = new EchoActor("actor-1");
  const actor2 = new EchoActor("actor-2");

  await subSystem1.receive({ id: "r3", type: 'register', payload: { actor: actor1 } });
  await subSystem2.receive({ id: "r4", type: 'register', payload: { actor: actor2 } });

  // Send message through root -> subsystem -> actor
  console.log("Sending to actor-1 in subsystem-1...");
  const response1 = await rootSystem.receive({
    id: "msg-1",
    type: 'route',
    payload: {
      targetId: "subsystem-1",
      message: {
        id: "msg-2",
        type: 'route',
        payload: {
          targetId: "actor-1",
          message: {
            id: "msg-3",
            type: "echo",
            payload: "Hello from nested system!",
            sender: "root"
          }
        }
      }
    }
  });

  console.log("Response:", JSON.stringify(response1, null, 2));

  console.log("\nSending to actor-2 in subsystem-2...");
  const response2 = await rootSystem.receive({
    id: "msg-4",
    type: 'route',
    payload: {
      targetId: "subsystem-2",
      message: {
        id: "msg-5",
        type: 'route',
        payload: {
          targetId: "actor-2",
          message: {
            id: "msg-6",
            type: "echo",
            payload: "Nested actors work!",
            sender: "root"
          }
        }
      }
    }
  });

  console.log("Response:", JSON.stringify(response2, null, 2));
}

async function demo5_PingHealthCheck() {
  console.log("\n=== Demo 5: Ping/Health Check Pattern ===\n");

  const system = new System();
  const actor = new EchoActor("echo-1");

  await system.receive({ id: "r1", type: 'register', payload: { actor } });

  // Ping the system itself
  console.log("Pinging system...");
  const systemPing = await system.receive({
    id: "ping-1",
    type: 'ping',
    payload: {}
  });
  console.log("System ping:", systemPing);

  // Ping actor through system
  console.log("\nPinging actor through system...");
  const actorPing = await system.receive({
    id: "ping-2",
    type: 'route',
    payload: {
      targetId: "echo-1",
      message: {
        id: "ping-3",
        type: 'ping',
        payload: {},
        sender: "healthcheck"
      }
    }
  });
  console.log("Actor ping:", actorPing);
}

// ============================================================================
// Run All Demos
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║       Hewitt Actor Model Examples - Proposed Design           ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_BasicReceive();
  await demo2_ChainPattern();
  await demo3_CoordinationPattern();
  await demo4_NestedSystems();
  await demo5_PingHealthCheck();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Key Takeaways from Hewitt Actor Model:                       ║");
  console.log("║                                                                ║");
  console.log("║  1. Actors RECEIVE messages (public interface)                ║");
  console.log("║  2. Actors SEND through System (private implementation)       ║");
  console.log("║  3. System IS an actor (uniform composition)                  ║");
  console.log("║  4. Location transparency (use IDs, not direct refs)          ║");
  console.log("║  5. Enables supervision trees (systems contain systems)       ║");
  console.log("║                                                                ║");
  console.log("║  See MIGRATION_PLAN.md for phased migration strategy          ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

// Run if executed directly
if (import.meta.main) {
  main().catch(console.error);
}

// Export for testing
export { System, BaseActor, EchoActor, ChainActor, CoordinatorActor, SubSystem };
