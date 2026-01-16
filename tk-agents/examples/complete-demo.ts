/**
 * Complete Demo - All Patterns
 *
 * This example demonstrates all key patterns from ACTOR_SPEC.md:
 * 1. Pure function actors
 * 2. Send injection
 * 3. Bridge API (external -> actor)
 * 4. Internal API (actor -> actor)
 * 5. Uniform composition (systems in systems)
 * 6. Opaque addressing
 */

import { createSystem } from "../src/actors-new/index.ts";
import type { ActorFactory, Message } from "../src/actors-new/index.ts";

console.log("=== Complete Actor System Demo ===\n");

// ============================================================================
// 1. PURE FUNCTION ACTORS
// ============================================================================

console.log("1. Pure Function Actors");
console.log("   - All dependencies explicit (data, send)");
console.log("   - No hidden state or context\n");

interface CounterData {
  count: number;
  name: string;
}

const CounterActor: ActorFactory<CounterData> = (data, send) => {
  // This is a pure function - all dependencies are parameters
  return {
    send: async (message: Message) => {
      switch (message.type) {
        case "increment":
          data.count++;
          console.log(`   [${data.name}] Incremented to ${data.count}`);
          return { success: true, data: data.count };
        case "get":
          return { success: true, data: data.count };
        default:
          return { success: false, error: "Unknown message type" };
      }
    },
  };
};

// ============================================================================
// 2. SEND INJECTION
// ============================================================================

console.log("2. Send Injection");
console.log("   - Actors receive send function at creation");
console.log("   - Send available in actor scope\n");

interface AggregatorData {
  name: string;
  counterIds: string[];
}

const AggregatorActor: ActorFactory<AggregatorData> = (data, send) => {
  // send is injected and available in scope
  return {
    send: async (message: Message) => {
      if (message.type === "aggregate") {
        console.log(`   [${data.name}] Aggregating from ${data.counterIds.length} counters`);

        let total = 0;
        // Use send from scope to query other actors
        for (const counterId of data.counterIds) {
          const response = await send(counterId, {
            id: crypto.randomUUID(),
            type: "get",
            payload: {},
          });
          if (response.success) {
            total += response.data as number;
          }
        }

        console.log(`   [${data.name}] Total: ${total}`);
        return { success: true, data: total };
      }
      return { success: false };
    },
  };
};

// ============================================================================
// 3. BRIDGE API (External -> Actor)
// ============================================================================

console.log("3. Bridge API (External -> Actor)");
console.log("   - External code uses actor.send(message)");
console.log("   - One argument: message only\n");

async function demonstrateBridgeAPI() {
  const system = createSystem();

  const counter = CounterActor({ count: 0, name: "counter-1" }, system.send);
  system.register("counter-1", counter);

  // External code bridges into actor world via actor.send()
  await counter.send({
    id: "msg-1",
    type: "increment",
    payload: {},
  });

  const result = await counter.send({
    id: "msg-2",
    type: "get",
    payload: {},
  });

  console.log(`   External call result: ${result.data}\n`);
}

await demonstrateBridgeAPI();

// ============================================================================
// 4. INTERNAL API (Actor -> Actor)
// ============================================================================

console.log("4. Internal API (Actor -> Actor)");
console.log("   - Actors use send(targetId, message)");
console.log("   - Two arguments: targetId and message\n");

async function demonstrateInternalAPI() {
  const system = createSystem();

  // Create counters
  const counter1 = CounterActor({ count: 5, name: "counter-1" }, system.send);
  const counter2 = CounterActor({ count: 10, name: "counter-2" }, system.send);
  const counter3 = CounterActor({ count: 15, name: "counter-3" }, system.send);

  system.register("counter-1", counter1);
  system.register("counter-2", counter2);
  system.register("counter-3", counter3);

  // Create aggregator that talks to counters
  const aggregator = AggregatorActor(
    { name: "aggregator", counterIds: ["counter-1", "counter-2", "counter-3"] },
    system.send
  );
  system.register("aggregator", aggregator);

  // External call to aggregator (bridge API)
  const result = await aggregator.send({
    id: "msg-1",
    type: "aggregate",
    payload: {},
  });

  console.log(`   Aggregation result: ${result.data}\n`);
}

await demonstrateInternalAPI();

// ============================================================================
// 5. UNIFORM COMPOSITION (Systems in Systems)
// ============================================================================

console.log("5. Uniform Composition");
console.log("   - Systems ARE actors");
console.log("   - Systems can contain systems\n");

async function demonstrateUniformComposition() {
  // Create root system
  const root = createSystem();

  // Create subsystems
  const teamA = createSystem();
  const teamB = createSystem();

  // Add actors to subsystems
  const counterA1 = CounterActor({ count: 10, name: "teamA-counter1" }, teamA.send);
  const counterA2 = CounterActor({ count: 20, name: "teamA-counter2" }, teamA.send);
  teamA.register("counter-1", counterA1);
  teamA.register("counter-2", counterA2);

  const counterB1 = CounterActor({ count: 30, name: "teamB-counter1" }, teamB.send);
  teamB.register("counter-1", counterB1);

  // Register subsystems in root (systems ARE actors!)
  root.register("team-a", teamA);
  root.register("team-b", teamB);

  console.log("   Created hierarchy: root -> [team-a, team-b]");

  // Query subsystems
  const statsA = await root.send("team-a", {
    id: "1",
    type: "stats",
    payload: {},
  });
  console.log(`   Team A: ${JSON.stringify(statsA.data)}`);

  const statsB = await root.send("team-b", {
    id: "2",
    type: "stats",
    payload: {},
  });
  console.log(`   Team B: ${JSON.stringify(statsB.data)}`);

  // Route message through hierarchy
  const routeResult = await root.send("team-a", {
    id: "3",
    type: "route",
    payload: {
      targetId: "counter-1",
      message: { id: "4", type: "increment", payload: {} },
    },
  });
  console.log(`   Routed increment: ${JSON.stringify(routeResult)}\n`);
}

await demonstrateUniformComposition();

// ============================================================================
// 6. OPAQUE ADDRESSING
// ============================================================================

console.log("6. Opaque Addressing");
console.log("   - Actors don't know HOW send works");
console.log("   - Implementation can change without affecting actors\n");

async function demonstrateOpaqueAddressing() {
  // Same actor factory, two different systems
  const system1 = createSystem();
  const system2 = createSystem();

  // Create identical actors in different systems
  const actor1 = CounterActor({ count: 0, name: "actor-in-system1" }, system1.send);
  const actor2 = CounterActor({ count: 0, name: "actor-in-system2" }, system2.send);

  system1.register("counter", actor1);
  system2.register("counter", actor2);

  console.log("   Created same actor type in two different systems");

  // Both work identically despite different routing implementations
  await actor1.send({ id: "1", type: "increment", payload: {} });
  await actor2.send({ id: "1", type: "increment", payload: {} });

  console.log("   Both actors work identically - routing is opaque\n");
}

await demonstrateOpaqueAddressing();

// ============================================================================
// COMPLETE EXAMPLE: All Patterns Together
// ============================================================================

console.log("7. Complete Example: All Patterns Together");
console.log("   - Hierarchical system with cross-subsystem communication\n");

async function completeExample() {
  // Root system
  const root = createSystem();

  // Subsystems
  const counters = createSystem();
  const aggregators = createSystem();

  // Create counters in counters subsystem
  const c1 = CounterActor({ count: 100, name: "c1" }, counters.send);
  const c2 = CounterActor({ count: 200, name: "c2" }, counters.send);
  const c3 = CounterActor({ count: 300, name: "c3" }, counters.send);

  counters.register("c1", c1);
  counters.register("c2", c2);
  counters.register("c3", c3);

  // Create aggregator with ROOT send (can reach across subsystems)
  const globalAggregator = AggregatorActor(
    { name: "global-aggregator", counterIds: ["counters", "counters", "counters"] },
    root.send // Note: using root.send, not aggregators.send
  );

  aggregators.register("global", globalAggregator);

  // Register subsystems in root
  root.register("counters", counters);
  root.register("aggregators", aggregators);

  console.log("   System hierarchy:");
  console.log("   root");
  console.log("   ├── counters [c1, c2, c3]");
  console.log("   └── aggregators [global]");
  console.log("");

  // Increment counters through routing
  await root.send("counters", {
    id: "1",
    type: "route",
    payload: {
      targetId: "c1",
      message: { id: "2", type: "increment", payload: {} },
    },
  });

  await root.send("counters", {
    id: "3",
    type: "route",
    payload: {
      targetId: "c2",
      message: { id: "4", type: "increment", payload: {} },
    },
  });

  // Query stats
  const counterStats = await root.send("counters", {
    id: "5",
    type: "stats",
    payload: {},
  });
  console.log(`   Counter subsystem: ${JSON.stringify(counterStats.data)}`);

  const aggStats = await root.send("aggregators", {
    id: "6",
    type: "stats",
    payload: {},
  });
  console.log(`   Aggregator subsystem: ${JSON.stringify(aggStats.data)}`);
}

await completeExample();

console.log("\n=== Demo Complete ===");
console.log("\nKey Takeaways:");
console.log("1. Actors are pure functions - testable and composable");
console.log("2. Send is injected - actors have no hidden dependencies");
console.log("3. Two APIs - external uses actor.send(), internal uses send()");
console.log("4. Systems ARE actors - uniform composition enables hierarchy");
console.log("5. Addressing is opaque - actors don't know routing mechanism");
console.log("6. Everything is type-safe and verifiable");
