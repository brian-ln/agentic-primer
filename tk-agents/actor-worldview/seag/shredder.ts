import { Actor, Message, ActorAddress } from "./kernel";
import { Actor as ActorModel, Handler, Implements } from "./lib/meta";

/**
 * FragmentNode: Represents a granular piece of a larger document.
 * Follows ap/STRUCTURE.model.lisp
 */
@ActorModel("FragmentNode")
@Implements("DataNode") // From model, though protocol not defined yet
export class FragmentNode extends Actor {
  public content: any;
  public parentId: ActorAddress | null = null;

  async receive(msg: Message) {
    if (msg.type === "INIT_FRAGMENT") await this.handleInit(msg);
    if (msg.type === "GET") await this.handleGetState(msg);
    if (msg.type === "PATCH") await this.handlePatch(msg);
  }

  @Handler("INIT_FRAGMENT")
  private async handleInit(msg: Message) {
    if (this.content === msg.payload.content) return; // No-Op
    this.content = msg.payload.content;
    this.parentId = msg.payload.parentId;
  }

  @Handler("GET")
  private async handleGetState(msg: Message) {
    this.send(msg.sender!, { type: "STATE", payload: this.content });
  }

  @Handler("PATCH")
  private async handlePatch(msg: Message) {
    const oldContent = JSON.stringify(this.content);
    // If payload is an object, merge it. Otherwise, replace it.
    if (typeof msg.payload === 'object' && msg.payload !== null && !Array.isArray(msg.payload)) {
      this.content = { ...this.content, ...msg.payload };
    } else {
      this.content = msg.payload;
    }
    
    if (JSON.stringify(this.content) === oldContent) return; // No-Op
    
    // Notify parent of the change
    if (this.parentId) {
      this.send(this.parentId, { 
        type: "FRAGMENT_UPDATED", 
        payload: { id: this.id, content: this.content } 
      });
    }
  }
}

/**
 * DocumentParser (The Shredder): Decomposes blobs into actor sub-graphs.
 */
@ActorModel("DocumentParser")
export class DocumentParser extends Actor {
  
  @Handler("SHRED")
  async receive(msg: Message) {
    console.log(`[DocumentParser] Received: ${msg.type} (Trace: ${msg.traceId})`);
    if (msg.type === "SHRED") {
      const { content, format, docId } = msg.payload;
      
      if (format === "json") {
        await this.shredJson(content, docId);
      } else {
        await this.shredLines(content, docId);
      }
    }

    if (msg.type === "VECTOR") {
      const targetId = msg.traceId?.startsWith("shred-") ? msg.traceId.slice(6) : null;
      if (targetId) {
        console.log(`[DocumentParser] Received vector for ${targetId}`);
        this.send("seag://system/projector", {
          type: "SET_VECTOR",
          payload: { id: targetId, vector: msg.payload.floats }
        });
      }
    }
  }

  private async shredJson(content: string, docId: ActorAddress) {
    const data = JSON.parse(content);
    
    for (const [key, value] of Object.entries(data)) {
      const fragId = `${docId}/fragments/${key}`;
      this.system.spawn(fragId, FragmentNode);
      
      this.send(fragId, { 
        type: "INIT_FRAGMENT", 
        payload: { content: value, parentId: docId } 
      });

      // Register with parent for re-assembly
      this.send(docId, {
        type: "FRAGMENT_REGISTERED",
        payload: { id: fragId, content: value }
      });

      // Establish the graph edge
      this.send("seag://system/projector", {
        type: "LINK_TO",
        payload: { from: docId, to: fragId, type: "contains" }
      });

      // AI: Request embedding
      console.log(`[DocumentParser] Requesting embedding for ${fragId}`);
      this.send("seag://system/embedder", {
        type: "EMBED",
        payload: { text: typeof value === 'string' ? value : JSON.stringify(value) },
        traceId: `shred-${fragId}`
      });
    }
  }

  private async shredLines(content: string, docId: ActorAddress) {
    const lines = content.split("\n");
    
    lines.forEach((line, index) => {
      if (line.trim().length === 0) return;
      
      const fragId = `${docId}/lines/${index}`;
      this.system.spawn(fragId, FragmentNode);
      
      this.send(fragId, { 
        type: "INIT_FRAGMENT", 
        payload: { content: line, parentId: docId } 
      });

      this.send("seag://system/projector", {
        type: "LINK_TO",
        payload: { from: docId, to: fragId, type: "contains" }
      });

      // AI: Request embedding
      console.log(`[DocumentParser] Requesting embedding for ${fragId}`);
      this.send("seag://system/embedder", {
        type: "EMBED",
        payload: { text: line },
        traceId: `shred-${fragId}`
      });
    });
  }
}
