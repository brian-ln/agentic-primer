import { System, Message } from "./kernel";

/**
 * Gateway: Bridges the Web Platform (WebSocket) to the Actor System.
 * Follows ap/REPL.spec.md
 */
export class Gateway {
  private system: System;
  private server: any;

  constructor(system: System) {
    this.system = system;
  }

  /**
   * Start the Bun WebSocket server.
   */
  start(port: number = 3000) {
    this.server = Bun.serve({
      port,
      fetch(req, server) {
        if (server.upgrade(req)) return;
        return new Response("SEAG Gateway Active");
      },
      websocket: {
        open: (ws) => {
          console.log("[Gateway] Connection opened");
          // Every connection gets a UserProxy (Simple mapping for MVP)
          this.system.send("seag://system/router", {
            type: "USER_CONNECTED",
            payload: { socketId: ws.remoteAddress }
          });
        },
        message: (ws, message) => {
          try {
            const data = JSON.parse(message.toString());
            // Route incoming JSON to the UserProxy
            this.system.send("seag://local/user-proxy", {
              ...data,
              sender: "seag://system/gateway"
            });
          } catch (err) {
            console.error("[Gateway] Message parse error:", err);
          }
        },
        close: (ws) => {
          console.log("[Gateway] Connection closed");
        }
      }
    });

    console.log(`[Gateway] SEAG Server running on port ${port}`);
  }

  stop() {
    this.server?.stop();
  }
}
