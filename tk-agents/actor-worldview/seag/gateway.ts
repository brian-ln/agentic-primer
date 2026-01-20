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
    <link rel="stylesheet" href="/style.css">
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
      // --- Client-Side Actor Kernel ---
      
      class Message {
        constructor(type, payload, sender, target) {
          this.type = type;
          this.payload = payload;
          this.sender = sender;
          this.target = target;
          this.traceId = null;
        }
      }

      class ClientSystem {
        constructor() {
          this.actors = new Map();
        }

        spawn(id, actorClass) {
          const actor = new actorClass(id, this);
          this.actors.set(id, actor);
          actor.onStart();
          return actor;
        }

        send(target, msg) {
          // Local routing
          if (this.actors.has(target)) {
            const actor = this.actors.get(target);
            setTimeout(() => actor.receive(msg), 0); // Async dispatch
          } 
          // Remote routing (default to NetworkActor for server)
          else if (target.startsWith("seag://system/") || target.startsWith("seag://local/")) {
             this.send("seag://client/network", { ...msg, target }); 
          }
          else {
            console.warn("Dead Letter:", target, msg);
          }
        }
      }

      class Actor {
        constructor(id, system) {
          this.id = id;
          this.system = system;
        }
        onStart() {}
        receive(msg) {}
        send(target, payload, type="SIGNAL") {
          this.system.send(target, { type, payload, sender: this.id });
        }
      }

      // --- Concrete Actors ---

      class REPLActor extends Actor {
        onStart() {
          this.log = document.getElementById('log');
          this.traceLog = document.getElementById('trace-log');
          this.input = document.getElementById('input');
          
          this.input.onkeydown = (ev) => {
            if (ev.key === 'Enter') {
              const text = this.input.value;
              if (!text) return;
              this.append("You: " + text, "");
              
              // Route to Inference Proxy
              this.send("seag://client/inference", { text }, "PROMPT");
              this.input.value = '';
            }
          };
        }

        receive(msg) {
          if (msg.type === "OUTPUT") {
            this.append("Brain: " + msg.payload.content, "output");
          }
          if (msg.type === "SIGNAL") {
             // Detect structured trace event
             if (msg.payload.sender && msg.payload.target) {
               this.appendTrace(msg.payload); 
             } else {
               const detail = msg.payload.detail || JSON.stringify(msg.payload);
               this.append("Think: " + detail, "thinking");
             }
          }
          if (msg.type === "STATUS_CHANGE") {
            this.updateStatus(msg.payload.connected);
          }
        }

        append(text, className) {
          const div = document.createElement('div');
          div.className = className;
          div.textContent = text;
          this.log.appendChild(div);
          this.log.scrollTop = this.log.scrollHeight;
        }
        
        appendTrace(event) {
          // Re-use the structured rendering logic we built
          const div = document.createElement('div');
          div.className = "trace-item";
          
          // Handle string detail (legacy) vs structured object
          if (typeof event.detail === 'string') {
             // Fallback or parse string
             div.textContent = event.detail; 
          } else {
             // It's the structured TraceEvent (passed as event)
             // Wait, msg.payload is the TraceEvent? Yes.
             // GatewayRelay passes msg.payload.
             
             // ... rendering logic from previous feature ...
             const time = new Date(event.timestamp || Date.now()).toLocaleTimeString().split(' ')[0];
             const timeSpan = document.createElement('span');
             timeSpan.className = "time";
             timeSpan.textContent = time;
             div.appendChild(timeSpan);

             const sSpan = document.createElement('span');
             sSpan.className = "trace-sender";
             sSpan.textContent = (event.sender || "?").replace('seag://system/', '').replace('seag://local/', ''); 
             
             const aSpan = document.createElement('span');
             aSpan.className = "trace-arrow";
             aSpan.textContent = "→";
             
             const tSpan = document.createElement('span');
             tSpan.className = "trace-target";
             tSpan.textContent = (event.target || "?").replace('seag://system/', '').replace('seag://local/', '');
             
             const tySpan = document.createElement('span');
             tySpan.className = "trace-type";
             tySpan.textContent = "[" + (event.messageType || "?") + "]";
             
             div.appendChild(sSpan);
             div.appendChild(aSpan);
             div.appendChild(tSpan);
             div.appendChild(tySpan);
          }
          
          this.traceLog.appendChild(div);
          this.traceLog.scrollTop = this.traceLog.scrollHeight;
        }

        updateStatus(connected) {
          const ind = document.getElementById('status');
          const txt = document.getElementById('status-text');
          if (connected) {
            ind.classList.add('connected');
            txt.textContent = "Connected";
            txt.style.color = "#4caf50";
            this.append("System connected.", "thinking");
          } else {
            ind.classList.remove('connected');
            txt.textContent = "Disconnected";
            txt.style.color = "#f44336";
            this.append("System disconnected.", "thinking");
          }
        }
      }

      class NetworkActor extends Actor {
        onStart() {
          this.scheme = (location.protocol === 'https:' ? 'wss:' : 'ws:');
          this.url = this.scheme + '//' + location.host + '/ws';
          this.retryDelay = 1000;
          this.connect();
        }

        connect() {
          this.ws = new WebSocket(this.url);
          this.ws.onopen = () => {
            this.send("seag://client/repl", { connected: true }, "STATUS_CHANGE");
            this.retryDelay = 1000;
          };
          this.ws.onclose = () => {
            this.send("seag://client/repl", { connected: false }, "STATUS_CHANGE");
            setTimeout(() => {
              this.retryDelay = Math.min(this.retryDelay * 2, 60000);
              this.connect();
            }, this.retryDelay);
          };
          this.ws.onmessage = (ev) => {
            try {
              const data = JSON.parse(ev.data);
              // Server messages usually go to REPL, but we could route them dynamically
              // Server sends: { type, payload, sender }
              // For now, forward everything to REPL
              this.send("seag://client/repl", data.payload, data.type);
            } catch(e) { console.error(e); }
          };
        }

        receive(msg) {
          // If we receive a message intended for remote, send it over WS
          // In ClientSystem.send, we wrap the original msg in { ...msg, target }
          // Or we just send the raw fields.
          
          // Currently msg is the internal client message.
          // We want to send to server: { type: "INPUT", payload: { text } }
          
          if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            // Transform client PROMPT to server INPUT/THINK format?
            // UserProxy expects: { type: "INPUT", payload: { text } }
            
            if (msg.type === "PROMPT") {
               this.ws.send(JSON.stringify({ 
                 type: "INPUT", 
                 payload: { text: msg.payload.text } 
               }));
            }
          } else {
            // Notify sender of failure?
            // Fallback logic could go here or in InferenceProxy
          }
        }
      }

      class InferenceProxy extends Actor {
        receive(msg) {
          if (msg.type === "PROMPT") {
            const net = this.system.actors.get("seag://client/network");
            if (net && net.ws && net.ws.readyState === WebSocket.OPEN) {
              // Online: Route to Network
              this.send("seag://client/network", msg.payload, "PROMPT");
            } else {
              // Offline: Try Local
              if (window.ai) {
                this.send("seag://client/repl", { content: "(Local AI) Processing..." }, "OUTPUT");
                // window.ai.createTextSession()... (Mock for now)
                setTimeout(() => {
                   this.send("seag://client/repl", { content: "(Local AI) I am a simple offline browser agent." }, "OUTPUT");
                }, 500);
              } else {
                this.send("seag://client/repl", { content: "Error: Offline and no local AI." }, "OUTPUT");
              }
            }
          }
        }
      }

      // --- Boot ---
      const system = new ClientSystem();
      system.spawn("seag://client/repl", REPLActor);
      system.spawn("seag://client/network", NetworkActor);
      system.spawn("seag://client/inference", InferenceProxy);

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

        if (url.pathname === '/style.css') {
          const css = await Bun.file("public/style.css").text();
          return new Response(css, { headers: { "Content-Type": "text\/css" } });
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