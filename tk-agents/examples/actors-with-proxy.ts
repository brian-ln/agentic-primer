// Actors with Proxy - method_missing pattern (like Smalltalk!)
// Any method call becomes a message send automatically

import type { Message, Response } from "../src/actors/base";

// Actor is a function
type Actor = (message: Message) => Promise<Response>;

// Actor with dynamic methods (any method name works!)
type DynamicActor = Actor & {
  [key: string]: (...args: any[]) => Promise<Response>;
};

// ============================================================================
// System with Proxy (method_missing pattern)
// ============================================================================

function System(): DynamicActor {
  const actors = new Map<string, Actor>();

  // The core actor function
  const receive: Actor = async (message: Message): Promise<Response> => {
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

  // Wrap with Proxy to intercept method calls!
  return new Proxy(receive, {
    get(target, prop: string) {
      // If property exists on the function, return it
      if (prop in target) {
        return (target as any)[prop];
      }

      // Otherwise, create a method that sends a message
      // This is like Smalltalk's doesNotUnderstand:
      return (...args: any[]) => {
        return target({
          id: crypto.randomUUID(),
          type: prop,
          payload: args.length === 1 ? args[0] : args
        });
      };
    },

    // Make it callable (function call goes through)
    apply(target, thisArg, args) {
      return target.apply(thisArg, args as [Message]);
    }
  }) as DynamicActor;
}

// ============================================================================
// Echo Actor with Proxy
// ============================================================================

function EchoActor(id: string): DynamicActor {
  const receive: Actor = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    return {
      success: true,
      data: {
        echo: message.payload,
        receivedFrom: message.sender,
        actorId: id,
        messageType: message.type
      }
    };
  };

  // Wrap with Proxy
  return new Proxy(receive, {
    get(target, prop: string) {
      if (prop in target) {
        return (target as any)[prop];
      }

      return (...args: any[]) => {
        return target({
          id: crypto.randomUUID(),
          type: prop,
          payload: args.length === 1 ? args[0] : args,
          sender: "direct"
        });
      };
    },

    apply(target, thisArg, args) {
      return target.apply(thisArg, args as [Message]);
    }
  }) as DynamicActor;
}

// ============================================================================
// Coordinator Actor with Proxy
// ============================================================================

function CoordinatorActor(
  id: string,
  workerIds: string[],
  system: DynamicActor
): DynamicActor {
  const receive: Actor = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    if (message.type !== 'coordinate') {
      return { success: false, error: 'Unknown message type' };
    }

    console.log(`  ${id}: coordinating ${workerIds.length} workers`);

    // Use dynamic methods on system!
    const results = await Promise.all(
      workerIds.map(async (workerId, index) => {
        console.log(`  ${id}: sending to worker ${workerId}`);

        // system.route() is dynamically created by Proxy!
        const response = await (system as any).route({
          targetId: workerId,
          message: {
            id: crypto.randomUUID(),
            type: 'work',
            payload: { task: message.payload, workerId: index },
            sender: id
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

  return new Proxy(receive, {
    get(target, prop: string) {
      if (prop in target) {
        return (target as any)[prop];
      }

      return (...args: any[]) => {
        return target({
          id: crypto.randomUUID(),
          type: prop,
          payload: args.length === 1 ? args[0] : args,
          sender: "direct"
        });
      };
    },

    apply(target, thisArg, args) {
      return target.apply(thisArg, args as [Message]);
    }
  }) as DynamicActor;
}

// ============================================================================
// Demos
// ============================================================================

async function demo1_DynamicMethods() {
  console.log("\n=== Demo 1: Dynamic Methods (method_missing) ===\n");

  const system = System();

  // These methods don't exist - created dynamically by Proxy!
  console.log("1. system.ping():");
  const response1 = await (system as any).ping();
  console.log(response1);

  console.log("\n2. system.list():");
  const response2 = await (system as any).list();
  console.log(response2);

  console.log("\n3. system.customMessage('foo', 'bar'):");
  const response3 = await (system as any).customMessage("foo", "bar");
  console.log(response3);

  console.log("\n✓ All methods created dynamically - like Smalltalk!");
}

async function demo2_RegisterAndRoute() {
  console.log("\n=== Demo 2: Register and Route with Dynamic Methods ===\n");

  const system = System();
  const echo = EchoActor("echo-1");

  // Register using dynamic method
  console.log("1. Register echo-1:");
  const regResponse = await (system as any).register({ id: "echo-1", actor: echo });
  console.log(regResponse);

  // List using dynamic method
  console.log("\n2. List actors:");
  const listResponse = await (system as any).list();
  console.log(listResponse);

  // Route using dynamic method
  console.log("\n3. Route to echo-1:");
  const routeResponse = await (system as any).route({
    targetId: "echo-1",
    message: {
      id: "msg-1",
      type: "echo",
      payload: "Hello!",
      sender: "external"
    }
  });
  console.log(routeResponse);
}

async function demo3_ArbitraryMessages() {
  console.log("\n=== Demo 3: Send Arbitrary Messages ===\n");

  const echo = EchoActor("echo-1");

  // Call ANY method name - Proxy converts to message!
  console.log("1. echo.ping():");
  const response1 = await (echo as any).ping();
  console.log(response1);

  console.log("\n2. echo.sayHello('World'):");
  const response2 = await (echo as any).sayHello("World");
  console.log(response2);

  console.log("\n3. echo.customEvent({ foo: 'bar' }):");
  const response3 = await (echo as any).customEvent({ foo: "bar" });
  console.log(response3);

  console.log("\n4. echo.doSomethingWeird(1, 2, 3):");
  const response4 = await (echo as any).doSomethingWeird(1, 2, 3);
  console.log(response4);

  console.log("\n✓ Any method name works - converted to message type!");
}

async function demo4_Coordination() {
  console.log("\n=== Demo 4: Coordination with Dynamic Methods ===\n");

  const system = System();

  // Register workers
  const worker1 = EchoActor("worker-1");
  const worker2 = EchoActor("worker-2");
  const worker3 = EchoActor("worker-3");

  await (system as any).register({ id: "worker-1", actor: worker1 });
  await (system as any).register({ id: "worker-2", actor: worker2 });
  await (system as any).register({ id: "worker-3", actor: worker3 });

  // Create coordinator
  const coordinator = CoordinatorActor(
    "coordinator",
    ["worker-1", "worker-2", "worker-3"],
    system
  );

  await (system as any).register({ id: "coordinator", actor: coordinator });

  // Coordinate using dynamic method
  console.log("Coordinating workers...\n");
  const response = await (system as any).route({
    targetId: "coordinator",
    message: {
      id: "msg-1",
      type: "coordinate",
      payload: "Task for all workers",
      sender: "external"
    }
  });

  console.log("\nResult:", JSON.stringify(response, null, 2));
}

async function demo5_CompareApproaches() {
  console.log("\n=== Demo 5: Compare Function vs Dynamic Method ===\n");

  const system = System();
  const echo = EchoActor("echo-1");

  await (system as any).register({ id: "echo-1", actor: echo });

  console.log("1. Function call (explicit message):");
  const response1 = await system({
    id: "msg-1",
    type: "route",
    payload: {
      targetId: "echo-1",
      message: {
        id: "msg-2",
        type: "echo",
        payload: "Function style",
        sender: "external"
      }
    }
  });
  console.log(response1);

  console.log("\n2. Dynamic method (implicit message):");
  const response2 = await (system as any).route({
    targetId: "echo-1",
    message: {
      id: "msg-3",
      type: "echo",
      payload: "Method style",
      sender: "external"
    }
  });
  console.log(response2);

  console.log("\n✓ Both work - pick your style!");
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║        Actors with Proxy (method_missing pattern)             ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_DynamicMethods();
  await demo2_RegisterAndRoute();
  await demo3_ArbitraryMessages();
  await demo4_Coordination();
  await demo5_CompareApproaches();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Key Benefits:                                                 ║");
  console.log("║                                                                ║");
  console.log("║  1. Proxy intercepts method calls (like method_missing)       ║");
  console.log("║  2. ANY method name works - no pre-definition needed          ║");
  console.log("║  3. Method name becomes message type automatically            ║");
  console.log("║  4. Just like Smalltalk's doesNotUnderstand:                  ║");
  console.log("║  5. Super flexible - invent methods on the fly                ║");
  console.log("║                                                                ║");
  console.log("║  system.ping() → system({ type: 'ping', payload: [] })       ║");
  console.log("║  system.foo(x) → system({ type: 'foo', payload: x })         ║");
  console.log("║  echo.customMsg(1,2) → echo({ type: 'customMsg', ... })      ║");
  console.log("║                                                                ║");
  console.log("║  This is the most Smalltalk-like actor API!                   ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

if (import.meta.main) {
  main().catch(console.error);
}

export { System, EchoActor, CoordinatorActor };
