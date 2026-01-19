import { Actor, Message, ActorAddress } from "./kernel";

/**
 * FragmentNode: Represents a granular piece of a larger document.
 * Follows ap/STRUCTURE.model.lisp
 */
export class FragmentNode extends Actor {
  public content: any;
  public parentId: ActorAddress | null = null;

  async receive(msg: Message) {
    if (msg.type === "INIT_FRAGMENT") {
      this.content = msg.payload.content;
      this.parentId = msg.payload.parentId;
    }

    if (msg.type === "GET_STATE") {
      this.send(msg.sender!, { type: "STATE", payload: this.content });
    }

    if (msg.type === "PATCH") {
      // If payload is an object, merge it. Otherwise, replace it.
      if (typeof msg.payload === 'object' && msg.payload !== null && !Array.isArray(msg.payload)) {
        this.content = { ...this.content, ...msg.payload };
      } else {
        this.content = msg.payload;
      }
      
      // Notify parent of the change
      if (this.parentId) {
        this.send(this.parentId, { 
          type: "FRAGMENT_UPDATED", 
          payload: { id: this.id, content: this.content } 
        });
      }
    }
  }
}

/**
 * DocumentParser (The Shredder): Decomposes blobs into actor sub-graphs.
 */
export class DocumentParser extends Actor {
  
  async receive(msg: Message) {
    if (msg.type === "SHRED") {
      const { content, format, docId } = msg.payload;
      
      if (format === "json") {
        await this.shredJson(content, docId);
      } else {
        await this.shredLines(content, docId);
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
        payload: { to: fragId, type: "contains" },
        sender: docId
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
        payload: { to: fragId, type: "contains" },
        sender: docId
      });
    });
  }
}
