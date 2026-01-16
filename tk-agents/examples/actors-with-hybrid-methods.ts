// Hybrid Methods - Defined methods + Dynamic methods (best of both worlds!)
// Have your cake and eat it too: Pre-defined methods work, AND method_missing works

import type { Message, Response } from "../src/actors/base";

type Actor = (message: Message) => Promise<Response>;

type DynamicActor = Actor & {
  [key: string]: (...args: any[]) => Promise<Response>;
};

// ============================================================================
// System with BOTH defined methods AND dynamic dispatch
// ============================================================================

function System(): DynamicActor {
  const actors = new Map<string, Actor>();

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

  // Define some EXPLICIT methods (these take priority)
  const explicitMethods = {
    // Special ping with extra info
    specialPing: async () => {
      return {
        success: true,
        data: {
          alive: true,
          timestamp: Date.now(),
          special: true,
          message: "This is a DEFINED method, not dynamic!"
        }
      };
    },

    // Convenience register
    quickRegister: async (id: string, actor: Actor) => {
      console.log(`  [DEFINED METHOD] quickRegister called for ${id}`);
      return receive({
        id: crypto.randomUUID(),
        type: "register",
        payload: { id, actor }
      });
    },

    // Convenience list
    quickList: async () => {
      console.log(`  [DEFINED METHOD] quickList called`);
      return receive({
        id: crypto.randomUUID(),
        type: "list",
        payload: {}
      });
    }
  };

  // Wrap with Proxy - checks defined methods FIRST, then dynamic
  return new Proxy(receive, {
    get(target, prop: string) {
      // 1. Check if it's a defined property on the function itself
      if (prop in target) {
        return (target as any)[prop];
      }

      // 2. Check if it's an explicitly defined method
      if (prop in explicitMethods) {
        console.log(`  → Using DEFINED method: ${prop}`);
        return (explicitMethods as any)[prop];
      }

      // 3. Fall back to dynamic method creation
      console.log(`  → Creating DYNAMIC method: ${prop}`);
      return (...args: any[]) => {
        return target({
          id: crypto.randomUUID(),
          type: prop,
          payload: args.length === 1 ? args[0] : args
        });
      };
    },

    apply(target, thisArg, args) {
      return target.apply(thisArg, args as [Message]);
    }
  }) as DynamicActor;
}

// ============================================================================
// Echo Actor with BOTH defined and dynamic methods
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

  // Define some explicit methods
  const explicitMethods = {
    // Custom echo with formatting
    fancyEcho: async (text: string) => {
      console.log(`  [DEFINED METHOD] fancyEcho called`);
      return {
        success: true,
        data: {
          echo: `✨ ${text} ✨`,
          receivedFrom: "fancyEcho",
          actorId: id,
          fancy: true
        }
      };
    },

    // Shorthand ping
    quickPing: async () => {
      console.log(`  [DEFINED METHOD] quickPing called`);
      return receive({
        id: crypto.randomUUID(),
        type: "ping",
        payload: {}
      });
    }
  };

  return new Proxy(receive, {
    get(target, prop: string) {
      if (prop in target) {
        return (target as any)[prop];
      }

      if (prop in explicitMethods) {
        console.log(`  → Using DEFINED method: ${prop}`);
        return (explicitMethods as any)[prop];
      }

      console.log(`  → Creating DYNAMIC method: ${prop}`);
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

async function demo1_DefinedVsDynamic() {
  console.log("\n=== Demo 1: Defined vs Dynamic Methods ===\n");

  const system = System();

  console.log("1. Call DEFINED method (specialPing):");
  const response1 = await (system as any).specialPing();
  console.log(response1);

  console.log("\n2. Call DYNAMIC method (ping):");
  const response2 = await (system as any).ping();
  console.log(response2);

  console.log("\n✓ Both work! Defined methods take priority.");
}

async function demo2_RegisterBothWays() {
  console.log("\n=== Demo 2: Register with Defined and Dynamic ===\n");

  const system = System();
  const echo1 = EchoActor("echo-1");
  const echo2 = EchoActor("echo-2");

  console.log("1. Register using DEFINED method (quickRegister):");
  const response1 = await (system as any).quickRegister("echo-1", echo1);
  console.log(response1);

  console.log("\n2. Register using DYNAMIC method (register):");
  const response2 = await (system as any).register({ id: "echo-2", actor: echo2 });
  console.log(response2);

  console.log("\n3. List using DEFINED method (quickList):");
  const listResponse = await (system as any).quickList();
  console.log(listResponse);
}

async function demo3_EchoDefinedVsDynamic() {
  console.log("\n=== Demo 3: Echo Actor - Defined vs Dynamic ===\n");

  const echo = EchoActor("echo-1");

  console.log("1. Call DEFINED method (fancyEcho):");
  const response1 = await (echo as any).fancyEcho("Hello World");
  console.log(response1);

  console.log("\n2. Call DEFINED method (quickPing):");
  const response2 = await (echo as any).quickPing();
  console.log(response2);

  console.log("\n3. Call DYNAMIC method (sayHello):");
  const response3 = await (echo as any).sayHello("Dynamic Method!");
  console.log(response3);

  console.log("\n4. Call DYNAMIC method (customMessage):");
  const response4 = await (echo as any).customMessage({ foo: "bar" });
  console.log(response4);
}

async function demo4_PriorityRules() {
  console.log("\n=== Demo 4: Priority Rules (Defined > Dynamic) ===\n");

  const system = System();

  console.log("Priority order:");
  console.log("  1. Function properties (like 'apply', 'call')");
  console.log("  2. Explicitly defined methods");
  console.log("  3. Dynamic method creation (method_missing)\n");

  console.log("Calling 'specialPing' (defined):");
  await (system as any).specialPing();

  console.log("\nCalling 'unknownMethod' (dynamic):");
  await (system as any).unknownMethod();

  console.log("\n✓ System checks in order: properties → defined → dynamic");
}

async function demo5_FlexibilityDemo() {
  console.log("\n=== Demo 5: Maximum Flexibility ===\n");

  const echo = EchoActor("echo-1");

  console.log("You can:");
  console.log("  1. Call as function (full control)");
  console.log("  2. Use defined methods (optimized, documented)");
  console.log("  3. Use dynamic methods (explore, prototype)");
  console.log("");

  console.log("1. Function call:");
  const response1 = await echo({
    id: "custom-1",
    type: "custom-type",
    payload: "function call",
    sender: "manual",
    correlationId: "abc-123"
  });
  console.log(response1);

  console.log("\n2. Defined method (fancyEcho):");
  const response2 = await (echo as any).fancyEcho("defined method");
  console.log(response2);

  console.log("\n3. Dynamic method (experimentalFeature):");
  const response3 = await (echo as any).experimentalFeature("dynamic method");
  console.log(response3);

  console.log("\n✓ All three work! Use what fits your need.");
}

async function demo6_AddMethodsAtRuntime() {
  console.log("\n=== Demo 6: Add Methods at Runtime ===\n");

  const system = System() as any;

  console.log("1. Before adding method:");
  console.log("  system.customMethod is dynamic (converts to message)");
  await system.customMethod("test");

  console.log("\n2. Add explicit method at runtime:");
  system.runtimeMethod = async (msg: string) => {
    console.log(`  [RUNTIME METHOD] Called with: ${msg}`);
    return {
      success: true,
      data: { message: `Runtime method got: ${msg}`, addedAtRuntime: true }
    };
  };

  console.log("\n3. Call the new method:");
  const response = await system.runtimeMethod("Hello!");
  console.log(response);

  console.log("\n✓ Can add methods dynamically - full JavaScript flexibility!");
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║        Hybrid Methods - Defined + Dynamic (Best of Both!)     ║");
  console.log("╚════════════════════════════════════════════════════════════════╝");

  await demo1_DefinedVsDynamic();
  await demo2_RegisterBothWays();
  await demo3_EchoDefinedVsDynamic();
  await demo4_PriorityRules();
  await demo5_FlexibilityDemo();
  await demo6_AddMethodsAtRuntime();

  console.log("\n╔════════════════════════════════════════════════════════════════╗");
  console.log("║  Dispatch Priority:                                            ║");
  console.log("║                                                                ║");
  console.log("║  1. Function properties (apply, call, bind, etc.)             ║");
  console.log("║  2. Explicitly defined methods (pre-defined)                  ║");
  console.log("║  3. Dynamic method creation (method_missing)                  ║");
  console.log("║                                                                ║");
  console.log("║  You can:                                                      ║");
  console.log("║  • Define common methods for optimization                     ║");
  console.log("║  • Use dynamic methods for exploration                        ║");
  console.log("║  • Add methods at runtime for flexibility                     ║");
  console.log("║  • Still call as function for full control                    ║");
  console.log("║                                                                ║");
  console.log("║  This is the most flexible actor API possible!                ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");
}

if (import.meta.main) {
  main().catch(console.error);
}

export { System, EchoActor };
