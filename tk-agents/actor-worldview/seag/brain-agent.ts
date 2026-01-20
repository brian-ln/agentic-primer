import { Actor, Message, CapabilityToken } from "./kernel";
import { DocumentActor } from "./document-actor";
import { Actor as ActorModel, Handler } from "./lib/meta";

@ActorModel("BrainAgent")
export class BrainAgent extends Actor {
  
  @Handler("THINK")
  @Handler("QUERY_RESULT")
  @Handler("FILE_CONTENT")
  @Handler("STATE")
  async receive(msg: Message) {
    if (msg.type === "THINK") {
      const input = msg.payload.input as string;
      
      if (input.startsWith("mount ")) {
        await this.handleMount(input.split(" ")[1], msg.sender!);
        return;
      }

      if (input.startsWith("explore ")) {
        await this.handleExplore(input.split(" ")[1], msg.sender!);
        return;
      }

      if (input.startsWith("watch ")) {
        await this.handleWatch(input.split(" ")[1], msg.sender!);
        return;
      }

      if (input.startsWith("get ")) {
        await this.handleGet(input.split(" ")[1], msg.sender!);
        return;
      }

      if (input.startsWith("set ")) {
        const parts = input.split(" ");
        const id = parts[1];
        const value = parts.slice(2).join(" ");
        await this.handleSet(id, value, msg.sender!);
        return;
      }

      if (input.startsWith("ask ")) {
        const text = input.slice(4);
        this.send("seag://system/inference", {
          type: "PROMPT",
          payload: { text }
        });
        return;
      }

      if (input.startsWith("search ")) {
        const text = input.slice(7);
        this.send("seag://system/embedder", {
          type: "EMBED",
          payload: { text }
        });
        return;
      }

      if (input.startsWith("embed ")) {
        const id = input.slice(6);
        // 1. Get the content
        this.send(id, { type: "GET", sender: this.id });
        return;
      }

      this.send(msg.sender!, {
        type: "OUTPUT",
        payload: { content: "Unknown command: " + input }
      });
    }

    if (msg.type === "RESPONSE") {
      this.send("seag://local/user-proxy", {
        type: "OUTPUT",
        payload: { content: "Gemini: " + msg.payload.text }
      });
    }

    if (msg.type === "VECTOR") {
      // If we have a current context (the node we were embedding), store it
      const targetId = msg.traceId?.startsWith("embed-") ? msg.traceId.slice(6) : null;
      
      if (targetId) {
        this.send("seag://system/projector", {
          type: "SET_VECTOR",
          payload: { id: targetId, vector: msg.payload.floats }
        });
        this.send("seag://local/user-proxy", { type: "OUTPUT", payload: { content: `Embedded and Indexed: ${targetId}` } });
      } else {
        // This was likely a search query vector
        this.send("seag://system/projector", {
          type: "QUERY",
          payload: { 
            predicate: "vector_search", 
            args: { vector: msg.payload.floats, limit: 3 } 
          }
        });
      }
    }

    if (msg.type === "QUERY_RESULT") {
      this.send("seag://local/user-proxy", {
        type: "OUTPUT",
        payload: { content: "Search Results: " + JSON.stringify(msg.payload, null, 2) }
      });
    }

    if (msg.type === "FILE_CONTENT") {
      this.send("seag://system/parser", { type: "SHRED", payload: { content: msg.payload.data, format: "json", docId: "seag://local/active-doc" } });
      this.send("seag://local/user-proxy", { type: "OUTPUT", payload: { content: `Mounted ${msg.payload.path}` } });
    }

    if (msg.type === "STATE") {
      // If this came from an 'embed' command, send to embedder
      this.send("seag://system/embedder", {
        type: "EMBED",
        payload: { text: typeof msg.payload === 'string' ? msg.payload : JSON.stringify(msg.payload) },
        traceId: `embed-${msg.sender}` // Correlation
      });
    }
  }

  @Handler("HANDLE_GET")
  private async handleGet(id: string, replyTo: string) {
    this.send(id, { type: "GET", sender: this.id });
  }

  @Handler("HANDLE_MOUNT")
  private async handleMount(path: string, replyTo: string) {
    this.send(replyTo, { type: "SIGNAL", payload: { status: "thinking", detail: "Mounting " + path } });
    const ct: CapabilityToken = { resource: "*", action: "*", expiresAt: Date.now() + 10000 };
    const token = Buffer.from(JSON.stringify(ct)).toString('base64');

    this.system.spawn("seag://local/active-doc", DocumentActor);
    this.send("seag://local/active-doc", { 
      type: "INIT_DOCUMENT", 
      payload: { path, format: "json" },
      capabilityToken: token
    });

    this.send("seag://system/file-io", {
      type: "READ_FILE",
      payload: { path },
      capabilityToken: token
    });
  }

  @Handler("HANDLE_WATCH")
  private async handleWatch(path: string, replyTo: string) {
    this.send(replyTo, { type: "SIGNAL", payload: { status: "thinking", detail: "Watching " + path } });
    const ct: CapabilityToken = { resource: "*", action: "*", expiresAt: Date.now() + 10000 };
    const token = Buffer.from(JSON.stringify(ct)).toString('base64');

    this.send("seag://system/file-io", {
      type: "WATCH_FILE",
      sender: "seag://local/active-doc",
      payload: { path },
      capabilityToken: token
    });

    this.send(replyTo, { type: "OUTPUT", payload: { content: "Watching " + path } });
  }

  @Handler("HANDLE_EXPLORE")
  private async handleExplore(nodeId: string, replyTo: string) {
    this.send("seag://system/projector", {
      type: "QUERY",
      payload: { predicate: "reachable", args: { from: nodeId } }
    });
  }

  @Handler("HANDLE_SET")
  private async handleSet(id: string, val: string, replyTo: string) {
    this.send(id, {
      type: "PATCH",
      payload: val
    });
    this.send(replyTo, { type: "OUTPUT", payload: { content: "Updated " + id } });
  }
}