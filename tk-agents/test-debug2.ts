import { Registry } from "./src/actors/registry";
import { createEchoMock } from "./src/actors/mock";

const registry = new Registry();
const actor = createEchoMock("test");
registry.register(actor);

console.log("Starting heartbeat");
registry.startHeartbeat("test", 100);

console.log("Waiting...");
await new Promise(r => setTimeout(r, 250));

console.log("Messages received by actor:", actor.receivedMessages.length);
actor.receivedMessages.forEach((msg, i) => {
  console.log(`  ${i}: type=${msg.type}, id=${msg.id}`);
});

registry.stopHeartbeat("test");
