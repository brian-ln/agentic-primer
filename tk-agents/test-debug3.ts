import { Registry } from "./src/actors/registry";
import { MockActor } from "./src/actors/mock";

const registry = new Registry();

const actor = new MockActor({
  id: "test",
  handler: (msg) => {
    console.log("Handler received:", msg.type);
    return { response: { success: true, data: {} } };
  },
});

registry.register(actor);

console.log("Registered actor:", registry.has("test"));
console.log("Starting heartbeat with 100ms interval...");

const info = (registry as any).actors.get("test");
console.log("Actor info before startHeartbeat:", info);

registry.startHeartbeat("test", 100);

console.log("Heartbeat started, interval ID:", info.heartbeatInterval);

await new Promise(r => setTimeout(r, 350));

console.log("After 350ms");
console.log("Received messages:", actor.receivedMessages.map(m => m.type));
