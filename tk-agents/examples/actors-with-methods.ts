// Actors with Methods - Attach convenience methods to actor functions
// Functions are objects, so we can add methods to them!

import type { Message, Response } from "../src/actors/base";

// Actor is a function
type Actor = (message: Message) => Promise<Response>;

// Actor WITH convenience methods
interface ActorWithMethods extends Actor {
  // Convenience methods
  ping?: () => Promise<Response>;
  register?: (id: string, actor: Actor) => Promise<Response>;
  unregister?: (id: string) => Promise<Response>;
  route?: (targetId: string, message: Message) => Promise<Response>;
  list?: () => Promise<Response>;
}

// ============================================================================
// System with Convenience Methods
// ============================================================================

function System(): ActorWithMethods {
  const actors = new Map<string, Actor>();

  // The core function
  const receive: ActorWithMethods = async (message: Message): Promise<Response> => {
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

  // Attach convenience methods to the function!
  receive.ping = async () => {
    return receive({
      id: crypto.randomUUID(),
      type: "ping",
      payload: {}
    });
  };

  receive.register = async (id: string, actor: Actor) => {
    return receive({
      id: crypto.randomUUID(),
      type: "register",
      payload: { id, actor }
    });
  };

  receive.unregister = async (id: string) => {
    return receive({
      id: crypto.randomUUID(),
      type: "unregister",
      payload: { id }
    });
  };

  receive.route = async (targetId: string, message: Message) => {
    return receive({
      id: crypto.randomUUID(),
      type: "route",
      payload: { targetId, message }
    });
  };

  receive.list = async () => {
    return receive({
      id: crypto.randomUUID(),
      type: "list",
      payload: {}
    });
  };

  return receive;
}

// ============================================================================
// Echo Actor with Methods
// ============================================================================

interface EchoActorWithMethods extends Actor {
  ping?: () => Promise<Response>;
  echo?: (payload: unknown) => Promise<Response>;
}

function EchoActor(id: string): EchoActorWithMethods {
  const receive: EchoActorWithMethods = async (message: Message): Promise<Response> => {
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

  // Attach convenience methods
  receive.ping = async () => {
    return receive({
      id: crypto.randomUUID(),
      type: "ping",
      payload: {}
    });
  };

  receive.echo = async (payload: unknown) => {
    return receive({
      id: crypto.randomUUID(),
      type: "echo",
      payload,
      sender: "direct"
    });
  };

  return receive;
}

// ============================================================================
// Coordinator Actor with Methods
// ============================================================================

interface CoordinatorActorWithMethods extends Actor {
  ping?: () => Promise<Response>;
  coordinate?: (task: unknown) => Promise<Response>;
}

function CoordinatorActor(
  id: string,
  workerIds: string[],
  system: ActorWithMethods
): CoordinatorActorWithMethods {
  const receive: CoordinatorActorWithMethods = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    if (message.type !== 'coordinate') {
      return { success: false, error: 'Unknown message type' };
    }

    console.log(`  ${id}: coordinating ${workerIds.length} workers`);

    // Can use system.route() method!
    const results = await Promise.all(
      workerIds.map(async (workerId, index) => {
        console.log(`  ${id}: sending to worker ${workerId}`);

        const response = await system.route!(workerId, {
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

  // Attach convenience method
  receive.ping = async () => {
    return receive({
      id: crypto.randomUUID(),
      type: "ping",
      payload: {}
    });
  };

  receive.coordinate = async (task: unknown) => {
    return receive({
      id: crypto.randomUUID(),
      type: "coordinate",
      payload: task,
      sender: "direct"
    });
  };

  return receive;
}

// ============================================================================
// Demos
// ============================================================================

async function demo1_ConvenienceMethods() {
  console.log("\n=== Demo 1: Convenience Methods ===\n");

  const system = System();

  // Can call as function (traditional)
  console.log("1. Traditional function call:");
  const response1 = await system({
    id: "msg-1",
    type: "ping",
    payload: {}
  });
  console.log(response1);

  // Or use convenience method!
  console.log("\n2. Convenience method:");
  const response2 = await system.ping!();
  console.log(response2);

  console.log("\n✓ Both work! Methods are syntactic sugar.");
}

async function demo2_RegisterWithMethods() {
  console.log("\n=== Demo 2: Register with Methods ===\n");

  const system = System();
  const echo = EchoActor("echo-1");

  // Traditional way
  console.log("1. Traditional registration:");
  const response1 = await system({
    id: "reg-1",
    type: "register",
    payload: { id: "echo-1", actor: echo }
  });
  console.log(response1);

  // Convenience method way
  console.log("\n2. Convenience method registration:");
  const echo2 = EchoActor("echo-2");
  const response2 = await system.register!("echo-2", echo2);
  console.log(response2);

  // List actors
  console.log("\n3. List actors:");
  const listResponse = await system.list!();
  console.log(listResponse);
}

async function demo3_RouteWithMethods() {
  console.log("\n=== Demo 3: Route with Methods ===\n");

  const system = System();
  const echo = EchoActor("echo-1");

  await system.register!("echo-1", echo);

  // Traditional route
  console.log("1. Traditional route:");
  const response1 = await system({
    id: "route-1",
    type: "route",
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-1",
        type: "echo",
        payload: "Hello traditional!",
        sender: "external"
      }
    }
  });
  console.log(response1);

  // Convenience method route
  console.log("\n2. Convenience method route:");
  const response2 = await system.route!("echo-1", {
    id: "msg-2",
    type: "echo",
    payload: "Hello convenience!",
    sender: "external"
  });
  console.log(response2);

  // Direct call to actor (bypassing system)
  console.log("\n3. Direct actor call:");
  const response3 = await echo.echo!("Hello direct!");
  console.log(response3);
}

async function demo4_CoordinationWithMethods() {
  console.log("\n=== Demo 4: Coordination with Methods ===\n");

  const system = System();

  // Register workers
  const worker1 = EchoActor("worker-1");
  const worker2 = EchoActor("worker-2");
  const worker3 = EchoActor("worker-3");

  await system.register!("worker-1", worker1);
  await system.register!("worker-2", worker2);
  await system.register!("worker-3", worker3);

  // Create coordinator (uses system.route method internally)
  const coordinator = CoordinatorActor(
    "coordinator",
    ["worker-1", "worker-2", "worker-3"],
    system
  );

  await system.register!("coordinator", coordinator);

  // Use convenience method to coordinate
  console.log("Coordinating workers...\n");
  const response = await coordinator.coordinate!("Task for all workers");

  console.log("\nResult:", JSON.stringify(response, null, 2));
}

async function demo5_BestOfBothWorlds() {
  console.log("\n=== Demo 5: Best of Both Worlds ===\n");

  const system = System();
  const echo = EchoActor("echo-1");

  await system.register!("echo-1", echo);

  console.log("You can mix and match styles:\n");

  // Function call with custom message
  const customResponse = await system({
    id: "custom-1",
    type: "route",
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-1",
        type: "custom-type",
        payload: { foo: "bar" },
        sender: "custom-sender",
        correlationId: "custom-correlation"
      }
    }
  });
  console.log("1. Custom message (function):", customResponse);

  // Convenience method for common operations
  const quickResponse = await system.route!("echo-1", {
    id: "quick-1",
    type: "echo",
    payload: "Quick message"
  });
  console.log("\n2. Quick message (method):", quickResponse);

  console.log("\n✓ Use functions for flexibility, methods for convenience!");
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║         Actors with Convenience Methods Pattern               ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_ConvenienceMethods();
  await demo2_RegisterWithMethods();
  await demo3_RouteWithMethods();
  await demo4_CoordinationWithMethods();
  await demo5_BestOfBothWorlds();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Key Benefits:                                                 ║");
  console.log("║                                                                ║");
  console.log("║  1. Functions are objects - can have methods                  ║");
  console.log("║  2. Convenience methods for common operations                 ║");
  console.log("║  3. Function call for flexibility and custom messages         ║");
  console.log("║  4. Methods are just syntactic sugar (wrap function call)     ║");
  console.log("║  5. Best of both worlds - ergonomics + power                  ║");
  console.log("║                                                                ║");
  console.log("║  system.ping() → system({ type: 'ping', payload: {} })       ║");
  console.log("║  system.route(id, msg) → system({ type: 'route', ... })      ║");
  console.log("║                                                                ║");
  console.log("║  This is the most ergonomic actor API!                        ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

if (import.meta.main) {
  main().catch(console.error);
}

export { System, EchoActor, CoordinatorActor };
