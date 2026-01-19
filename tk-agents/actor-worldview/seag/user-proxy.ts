import { Actor, Message } from "./kernel";

/**
 * UserProxy: The actor representing the Human user in the SEAG.
 * Follows ap/REPL_AGENT.model.lisp
 */
export class UserProxy extends Actor {
  
  async receive(msg: Message) {
    if (msg.type === "INPUT") {
      const text = msg.payload.text;
      console.log(`[UserProxy] Received: ${text}`);

      // 1. Record the interaction
      this.send("seag://system/interaction-log", {
        type: "APPEND",
        payload: { from: "user", content: text }
      });

      // 2. Trigger Brain
      this.send("seag://system/brain", {
        type: "THINK",
        payload: { input: text }
      });
    }

    if (msg.type === "OUTPUT") {
      // Send result back to the user (via Gateway relay)
      console.log(`[UserProxy] Replying: ${msg.payload.content}`);
      // In MVP, we just console log, but this would push back to WS
    }
  }
}
