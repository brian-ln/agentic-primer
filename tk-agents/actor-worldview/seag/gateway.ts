import { System, Message, Actor as SystemActor } from "./kernel";
import { Actor as ActorModel, Handler } from "./lib/meta";

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
      this.ws.send(JSON.stringify(msg));
    }
  }
}

export class Gateway {
  private system: System;
  private server: any;

  constructor(system: System) { this.system = system; }

  start(port: number = 3000) {
    this.server = Bun.serve({
      port,
      fetch(req, server) {
        if (server.upgrade(req)) return;
        return new Response(`
          <!DOCTYPE html>
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
                const ws = new WebSocket('ws://' + location.host);
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
        `, { headers: { "Content-Type": "text/html" } });
      },
      websocket: {
        open: (ws) => {
          const actor = this.system.spawn("seag://system/gateway-relay", GatewayRelay, "transient", true);
          actor.ws = ws;
        },
        message: (ws, message) => {
          try {
            const data = JSON.parse(message.toString());
            this.system.send("seag://local/user-proxy", {
              ...data,
              sender: "seag://system/gateway-relay"
            });
          } catch (err) {
            console.error("Gateway parse error");
          }
        }
      }
    });
    console.log("Gateway started on " + port);
  }

  stop() { this.server?.stop(); }
}