import { System, Message, Actor as SystemActor } from "./kernel";
import * as path from "path";

import { Actor as ActorModel, Handler } from "./lib/meta";

// Configurable debug flag and allowed origins for WebSocket upgrades
const DEBUG = (process.env.SEAG_DEBUG === '1' || process.env.SEAG_DEBUG === 'true');
const ALLOWED_WS_ORIGINS: string[] = (process.env.SEAG_ALLOWED_WS_ORIGINS || 'http://localhost:3000,http://127.0.0.1:3000,http://[::1]:3000')
  .split(',')
  .map(s => s.trim())
  .filter(Boolean);

@ActorModel("GatewayRelay")
export class GatewayRelay extends SystemActor {
  // Inject ws via constructor or state? 
  // System.spawn instantiates with (id, system).
  // We need to pass the 'ws' socket to it.
  public ws: any;

  @Handler("OUTPUT")
  @Handler("SIGNAL")
  async receive(msg: Message) {
    if (this.ws && (msg.type === "OUTPUT" || msg.type === "SIGNAL")) {
      try {
        if (DEBUG) console.log('GatewayRelay: sending message back to client', msg.type, msg.payload);
        this.ws.send(JSON.stringify(msg));
      } catch (err) {
        console.error('GatewayRelay: ws.send error', err);
      }
    }
  }
}

const REPL = `<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>SEAG REPL</title>
    <style>
      body { background: #121212; color: #00ff00; font-family: monospace; padding: 20px; }
      #log { height: 400px; overflow-y: auto; border: 1px solid #333; padding: 10px; margin-bottom: 10px; }
      input { background: #000; color: #00ff00; border: 1px solid #333; width: 100%; padding: 10px; font-family: monospace; }
      .thinking { color: #888; font-style: italic; }
      .output { color: #00ffff; }
    </style>
  </head>
  <body>
    <h1>🌌 SEAG Actor Graph REPL</h1>
    <div id="log"></div>
    <input id="input" type="text" placeholder="Type a message to the graph" autofocus>
    <script>
      const scheme = (location.protocol === 'https:' ? 'wss:' : 'ws:');
      const ws = new WebSocket(scheme + '//' + location.host + '/ws');
      const log = document.getElementById('log');
      const input = document.getElementById('input');
      function append(text, className) {
        const div = document.createElement('div');
        div.className = className;
        div.textContent = text;
        log.appendChild(div);
        log.scrollTop = log.scrollHeight;
      }
      ws.onmessage = (ev) => {
        const msg = JSON.parse(ev.data);
        if (msg.type === "SIGNAL") append("Think: " + msg.payload.detail, "thinking");
        if (msg.type === "OUTPUT") append("Brain: " + msg.payload.content, "output");
      };
      input.onkeydown = (ev) => {
        if (ev.key === 'Enter') {
          const text = input.value;
          if (!text) return;
          append("You: " + text, "");
          ws.send(JSON.stringify({ type: "INPUT", payload: { text } }));
          input.value = '';
        }
      };
    </script>
  </body>
</html>
-        
`


export class Gateway {
  private system: System;
  private server: any;

  constructor(system: System) { this.system = system; }

  start(port: number = 3000) {
    this.server = Bun.serve({
      port,
      async fetch(req, server) {
        const url = new URL(req.url);

        // Strict WebSocket upgrade handling: only accept upgrades at /ws
        const method = (req.method || '').toUpperCase();
        const upgradeHeader = (req.headers.get("upgrade") || '').toLowerCase();
        const connectionHeader = (req.headers.get("connection") || '').toLowerCase();
        const origin = req.headers.get("origin");

        if (
          method === 'GET' &&
          upgradeHeader === 'websocket' &&
          connectionHeader.includes('upgrade') &&
          url.pathname === '/ws'
        ) {
          // Optional origin restriction: check against configured whitelist
          if (origin && ALLOWED_WS_ORIGINS.length > 0 && !ALLOWED_WS_ORIGINS.includes(origin)) {
            if (DEBUG) console.warn('WebSocket connection rejected due to origin:', origin);
            return new Response('Forbidden', { status: 403 });
          }

          if (DEBUG) console.log("WebSocket upgrade requested for", url.pathname, "(origin:", origin, ")");
          try {
            if (server.upgrade(req)) return; // upgrade succeeded
            if (DEBUG) console.error("WebSocket upgrade returned false for", url.pathname);
            return new Response("Upgrade failed", { status: 500 });
          } catch (err) {
            console.error("WebSocket upgrade error:", err);
            return new Response("Upgrade failed", { status: 500 });
          }
        }

        if (url.pathname === '/') {
          return new Response(REPL, { headers: { 'Content-Type': 'text\/html' } });
        }

        if (url.pathname === '/health') {
          return new Response(JSON.stringify({ status: "ok" }), {
            headers: { "Content-Type": "application\/json" },
            status: 200
          });
        }

        return new Response("Not Found", { status: 404 });
      },
      websocket: {
        open: (ws) => {
          if (DEBUG) console.log("Gateway: WebSocket connection opened");
          const actor = this.system.spawn("seag://system/gateway-relay", GatewayRelay, "transient", true);
          actor.ws = ws;
          if (DEBUG) console.log("Gateway: Spawned GatewayRelay and attached ws (actor id):", actor.id);
          // Optional: log on close to observe disconnections
          try {
            if (typeof ws.onclose === 'function') {
              const orig = ws.onclose;
              ws.onclose = (ev) => {
                if (DEBUG) console.log('Gateway: WebSocket closed for actor', actor.id, ev);
                orig(ev);
              };
            }
          } catch (e) {
            // Some runtimes may not expose onclose; ignore
          }
        },
        message: (ws, message) => {
          try {
            if (DEBUG) console.log("Gateway raw message:", message.toString());
            const data = JSON.parse(message.toString());
            if (DEBUG) console.log("Gateway parsed message:", data);
            this.system.send("seag://local/user-proxy", {
              ...data,
              sender: "seag://system/gateway-relay"
            });
          } catch (err) {
            console.error("Gateway parse error:", err);
          }
        }
      }
    });
    console.log("Gateway started on " + port);
  }

  stop() { this.server?.stop(); }
}