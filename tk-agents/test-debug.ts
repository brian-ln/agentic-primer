import { Registry } from "./src/actors/registry";
import { MockActor } from "./src/actors/mock";

const registry = new Registry();

// Create actor that fails on ping
let callCount = 0;
const unresponsiveActor = new MockActor({
  id: "unresponsive",
  handler: (msg) => {
    console.log(`Handler called: type=${msg.type}, callCount=${callCount}`);
    if (msg.type === 'ping') {
      callCount++;
      if (callCount > 1) {
        console.log("Throwing exception");
        throw new Error("Actor became unresponsive");
      }
    }
    return { response: { success: true, data: {} } };
  },
});

registry.register(unresponsiveActor);

// Listen for actor_died event
let deathEvent: any = null;
registry.on('actor_died', (event) => {
  console.log("actor_died event received:", event);
  deathEvent = event;
});

// Start heartbeat with short interval
console.log("Starting heartbeat");
registry.startHeartbeat("unresponsive", 100);

// Wait for heartbeat to detect failure
await new Promise(r => setTimeout(r, 300));

console.log("After wait, deathEvent:", deathEvent);
console.log("Actor still in registry?", registry.has("unresponsive"));
