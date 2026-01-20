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

  async onStart() {
    this.send("seag://system/topic/trace", {
      type: "SUBSCRIBE",
      payload: { consumer_id: this.id }
    });
  }

  @Handler("OUTPUT")
  @Handler("SIGNAL")
  @Handler("NOTIFY")
  async receive(msg: Message) {
    if (!this.ws) return;

    if (msg.type === "NOTIFY") {
      // Re-wrap notification (trace) as a SIGNAL for the client
      this.ws.send(JSON.stringify({
        type: "SIGNAL",
        payload: msg.payload
      }));
      return;
    }

    if (msg.type === "OUTPUT" || msg.type === "SIGNAL") {
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
      body { background: #121212; color: #00ff00; font-family: monospace; padding: 20px; display: flex; flex-direction: column; height: 100vh; box-sizing: border-box; margin: 0; }
      h1 { margin: 0 0 10px 0; font-size: 1.2em; }
      #container { display: flex; flex: 1; gap: 20px; min-height: 0; }
      #main-panel { flex: 2; display: flex; flex-direction: column; }
      #trace-panel { flex: 1; display: flex; flex-direction: column; border-left: 1px solid #333; padding-left: 20px; }
      
      .log-window { flex: 1; overflow-y: auto; border: 1px solid #333; padding: 10px; margin-bottom: 10px; background: #000; }
      input { background: #111; color: #00ff00; border: 1px solid #333; width: 100%; padding: 10px; font-family: monospace; box-sizing: border-box; }
      
      .thinking { color: #888; font-style: italic; }
      .output { color: #00ffff; }
      .trace-item { font-size: 0.85em; color: #ccc; border-bottom: 1px solid #222; padding: 4px 8px; font-family: 'Consolas', 'Monaco', monospace; line-height: 1.4; }
      .trace-item:nth-child(odd) { background: #0a0a0a; }
      .trace-item .time { color: #666; margin-right: 8px; font-size: 0.9em; min-width: 50px; display: inline-block; }
      .trace-sender { color: #4db6ac; font-weight: bold; }
      .trace-arrow { color: #555; margin: 0 5px; }
      .trace-target { color: #81c784; font-weight: bold; }
      .trace-type { color: #ffb74d; margin-left: 8px; font-weight: bold; float: right; }
      
      .header-container { display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px; }
      .status-indicator { width: 10px; height: 10px; border-radius: 50%; background: #f44336; display: inline-block; box-shadow: 0 0 5px #f44336; transition: all 0.3s; }
      .status-indicator.connected { background: #4caf50; box-shadow: 0 0 8px #4caf50; }
      .status-text { font-size: 0.8em; color: #666; margin-left: 8px; }
    </style>
  </head>
  <body>
    <div class="header-container">
      <h1>🌌 SEAG Actor Graph REPL</h1>
      <div>
        <span id="status" class="status-indicator"></span>
        <span id="status-text" class="status-text">Disconnected</span>
      </div>
    </div>
    <div id="container">
      <div id="main-panel">
        <div id="log" class="log-window"></div>
        <input id="input" type="text" placeholder="Type a message (e.g. 'trace ask ...')" autofocus>
      </div>
      <div id="trace-panel">
        <div style="color: #888; margin-bottom: 5px; font-size: 0.9em; text-transform: uppercase; letter-spacing: 1px;">Trace Log</div>
        <div id="trace-log" class="log-window"></div>
      </div>
    </div>

    <script>
      const scheme = (location.protocol === 'https:' ? 'wss:' : 'ws:');
      const ws = new WebSocket(scheme + '//' + location.host + '/ws');
      const log = document.getElementById('log');
      const traceLog = document.getElementById('trace-log');
      const input = document.getElementById('input');
      const statusInd = document.getElementById('status');
      const statusText = document.getElementById('status-text');

      ws.onopen = () => {
        statusInd.classList.add('connected');
        statusText.textContent = "Connected";
        statusText.style.color = "#4caf50";
        append("System connected.", "thinking");
      };

      ws.onclose = () => {
        statusInd.classList.remove('connected');
        statusText.textContent = "Disconnected";
        statusText.style.color = "#f44336";
        append("System disconnected.", "thinking");
      };

      function append(text, className) {
        const div = document.createElement('div');
        div.className = className;
        div.textContent = text;
        log.appendChild(div);
        log.scrollTop = log.scrollHeight;
      }

      function appendTrace(event) {
        const div = document.createElement('div');
        div.className = "trace-item";
        
        const time = new Date(event.timestamp).toLocaleTimeString().split(' ')[0];
        
        const timeSpan = document.createElement('span');
        timeSpan.className = "time";
        timeSpan.textContent = time;
        div.appendChild(timeSpan);

        const sSpan = document.createElement('span');
        sSpan.className = "trace-sender";
        sSpan.textContent = event.sender.replace('seag://system/', '').replace('seag://local/', ''); 
        
        const aSpan = document.createElement('span');
        aSpan.className = "trace-arrow";
        aSpan.textContent = "→";
        
        const tSpan = document.createElement('span');
        tSpan.className = "trace-target";
        tSpan.textContent = event.target.replace('seag://system/', '').replace('seag://local/', '');
        
        const tySpan = document.createElement('span');
        tySpan.className = "trace-type";
        tySpan.textContent = "[" + event.messageType + "]";
        
        div.appendChild(sSpan);
        div.appendChild(aSpan);
        div.appendChild(tSpan);
        div.appendChild(tySpan);
        
        traceLog.appendChild(div);
        traceLog.scrollTop = traceLog.scrollHeight;
      }

      ws.onmessage = (ev) => {
        const msg = JSON.parse(ev.data);
        
        if (msg.type === "SIGNAL") {
          // Check if it's a structured trace event
          if (msg.payload.sender && msg.payload.target) {
            appendTrace(msg.payload);
          } else {
            // Legacy/Normal signal
            const detail = msg.payload.detail || JSON.stringify(msg.payload);
            append("Think: " + detail, "thinking");
          }
        }
        
        if (msg.type === "OUTPUT") {
          append("Brain: " + msg.payload.content, "output");
        }
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