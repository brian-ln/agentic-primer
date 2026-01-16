/**
 * Complete Demo - Address Proxy Pattern
 *
 * This example demonstrates all key patterns from ACTOR_SYSTEM.spec.md:
 * 1. Addresses are first-class objects
 * 2. Actor factories return addresses
 * 3. Two send styles (ergonomic and explicit)
 * 4. Pure functions with explicit system dependency
 * 5. No magic strings - type-safe addresses
 */

import type { ActorFactory, Address, Message, System as SystemType } from "../src/actors/index.ts";
import { System } from "../src/actors/index.ts";

console.log("=== Complete Actor System Demo (Address Proxy Pattern) ===\n");

// ============================================================================
// 1. ADDRESSES ARE FIRST-CLASS OBJECTS
// ============================================================================

console.log("1. Addresses are First-Class Objects");
console.log("   - Address has __id (symbol) and .send() method");
console.log("   - No magic strings - type-safe references\n");

interface CounterData {
  count: number;
  name: string;
  system: SystemType;
}

const CounterActor: ActorFactory<CounterData> = (data) => {
  const actor = {
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
  return data.system.register(actor);
};

async function demonstrateFirstClassAddresses() {
  const system = System();

  const counter = CounterActor({ count: 0, name: "counter-1", system });

  // Address is an object with __id and send
  console.log(`   Address has __id: ${typeof counter.__id === "symbol"}`);
  console.log(`   Address has send: ${typeof counter.send === "function"}`);
  console.log(`   NOT a string: ${typeof counter !== "string"}\n`);
}

await demonstrateFirstClassAddresses();

// ============================================================================
// 2. ACTOR FACTORIES RETURN ADDRESSES
// ============================================================================

console.log("2. Actor Factories Return Addresses");
console.log("   - Single call creates, registers, returns");
console.log("   - System injected via data parameter\n");

async function demonstrateFactoryReturnsAddress() {
  const system = System();

  // Single call does everything
  const counter = CounterActor({ count: 0, name: "counter-2", system });

  // Immediately usable - no separate registration step
  await counter.send({
    id: crypto.randomUUID(),
    type: "increment",
    payload: {},
  });

  const result = await counter.send({
    id: crypto.randomUUID(),
    type: "get",
    payload: {},
  });

  console.log(`   Factory returned usable Address: ${result.data}\n`);
}

await demonstrateFactoryReturnsAddress();

// ============================================================================
// 3. TWO SEND STYLES
// ============================================================================

console.log("3. Two Send Styles");
console.log("   - Ergonomic: address.send(message)");
console.log("   - Explicit: system.send(address, message)\n");

async function demonstrateTwoSendStyles() {
  const system = System();

  const counter = CounterActor({ count: 0, name: "counter-3", system });

  // Ergonomic style
  await counter.send({
    id: crypto.randomUUID(),
    type: "increment",
    payload: {},
  });

  // Explicit style
  await system.send(counter, {
    id: crypto.randomUUID(),
    type: "increment",
    payload: {},
  });

  const result = await counter.send({
    id: crypto.randomUUID(),
    type: "get",
    payload: {},
  });

  console.log(`   Both styles work: count = ${result.data}\n`);
}

await demonstrateTwoSendStyles();

// ============================================================================
// 4. ACTOR-TO-ACTOR COMMUNICATION
// ============================================================================

console.log("4. Actor-to-Actor Communication");
console.log("   - Actors hold Address references");
console.log("   - Use either .send() or system.send()\n");

interface AggregatorData {
  name: string;
  counters: Address[];  // Address references, not strings!
  system: SystemType;
}

const AggregatorActor: ActorFactory<AggregatorData> = (data) => {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "aggregate") {
        console.log(`   [${data.name}] Aggregating from ${data.counters.length} counters`);

        let total = 0;
        // Use Address references directly with ergonomic .send()
        for (const counterAddr of data.counters) {
          const response = await counterAddr.send({
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
  return data.system.register(actor);
};

async function demonstrateActorToActorCommunication() {
  const system = System();

  // Create counters
  const counter1 = CounterActor({ count: 5, name: "counter-1", system });
  const counter2 = CounterActor({ count: 10, name: "counter-2", system });
  const counter3 = CounterActor({ count: 15, name: "counter-3", system });

  // Create aggregator with Address references
  const aggregator = AggregatorActor({
    name: "aggregator",
    counters: [counter1, counter2, counter3], // Pass addresses directly!
    system,
  });

  const result = await aggregator.send({
    id: crypto.randomUUID(),
    type: "aggregate",
    payload: {},
  });

  console.log(`   Aggregation result: ${result.data}\n`);
}

await demonstrateActorToActorCommunication();

// ============================================================================
// 5. ADDRESSES IN MESSAGES
// ============================================================================

console.log("5. Addresses in Messages");
console.log("   - Addresses can be passed in message payloads");
console.log("   - Enables dynamic actor discovery\n");

interface RegistryData {
  name: string;
  registered: Address[];
  system: SystemType;
}

const RegistryActor: ActorFactory<RegistryData> = (data) => {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "register") {
        const { addr } = message.payload as { addr: Address };
        data.registered.push(addr);
        console.log(`   [${data.name}] Registered actor (total: ${data.registered.length})`);
        return { success: true };
      }

      if (message.type === "notifyAll") {
        const { text } = message.payload as { text: string };
        console.log(`   [${data.name}] Notifying ${data.registered.length} actors`);

        for (const addr of data.registered) {
          await addr.send({
            id: crypto.randomUUID(),
            type: "notification",
            payload: { text },
          });
        }

        return { success: true };
      }

      return { success: false };
    },
  };
  return data.system.register(actor);
};

interface ListenerData {
  name: string;
  system: SystemType;
}

const ListenerActor: ActorFactory<ListenerData> = (data) => {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "notification") {
        const { text } = message.payload as { text: string };
        console.log(`   [${data.name}] Received: ${text}`);
        return { success: true };
      }
      return { success: false };
    },
  };
  return data.system.register(actor);
};

async function demonstrateAddressesInMessages() {
  const system = System();

  const registry = RegistryActor({ name: "registry", registered: [], system });
  const listener1 = ListenerActor({ name: "listener-1", system });
  const listener2 = ListenerActor({ name: "listener-2", system });

  // Pass addresses in messages
  await registry.send({
    id: crypto.randomUUID(),
    type: "register",
    payload: { addr: listener1 },
  });

  await registry.send({
    id: crypto.randomUUID(),
    type: "register",
    payload: { addr: listener2 },
  });

  // Registry can now send to registered addresses
  await registry.send({
    id: crypto.randomUUID(),
    type: "notifyAll",
    payload: { text: "Hello from registry!" },
  });

  console.log();
}

await demonstrateAddressesInMessages();

// ============================================================================
// 6. PURE FUNCTIONS WITH EXPLICIT DEPENDENCIES
// ============================================================================

console.log("6. Pure Functions with Explicit Dependencies");
console.log("   - All dependencies in data parameter");
console.log("   - System is just another dependency\n");

async function demonstratePureFunctions() {
  const system = System();

  // All dependencies explicit
  const counter = CounterActor({
    count: 0,        // State
    name: "pure",    // Config
    system,          // Dependency
  });

  console.log("   Created actor with explicit dependencies:");
  console.log("   - count: 0 (state)");
  console.log("   - name: 'pure' (config)");
  console.log("   - system: System (dependency)");
  console.log("   No hidden globals or context\n");
}

await demonstratePureFunctions();

// ============================================================================
// COMPLETE EXAMPLE: All Patterns Together
// ============================================================================

console.log("7. Complete Example: All Patterns Together");
console.log("   - Multiple actors with Address references");
console.log("   - Both send styles");
console.log("   - Addresses in messages\n");

async function completeExample() {
  const system = System();

  // Create counters
  const c1 = CounterActor({ count: 100, name: "c1", system });
  const c2 = CounterActor({ count: 200, name: "c2", system });
  const c3 = CounterActor({ count: 300, name: "c3", system });

  console.log("   Created 3 counters: c1(100), c2(200), c3(300)");

  // Create aggregator with Address references
  const aggregator = AggregatorActor({
    name: "global-aggregator",
    counters: [c1, c2, c3],
    system,
  });

  console.log("   Created aggregator with references to all counters");

  // Increment counters using ergonomic style
  await c1.send({ id: crypto.randomUUID(), type: "increment", payload: {} });
  await c2.send({ id: crypto.randomUUID(), type: "increment", payload: {} });

  // Increment using explicit style
  await system.send(c3, { id: crypto.randomUUID(), type: "increment", payload: {} });

  console.log("   Incremented all counters\n");

  // Aggregate using actor-to-actor communication
  const result = await aggregator.send({
    id: crypto.randomUUID(),
    type: "aggregate",
    payload: {},
  });

  console.log(`   Final aggregation: ${result.data}`);
}

await completeExample();

console.log("\n=== Demo Complete ===");
console.log("\nKey Takeaways:");
console.log("1. Addresses are first-class objects with __id and .send()");
console.log("2. Actor factories return Addresses directly");
console.log("3. Two send styles: ergonomic (addr.send) and explicit (system.send)");
console.log("4. System in data - pure functions with explicit dependencies");
console.log("5. No magic strings - type-safe Address objects everywhere");
console.log("6. Addresses can be passed in messages and stored as data");
console.log("7. Everything is composable and testable");
