import { Actor, Message, ActorAddress } from "./kernel";

/**
 * DocumentActor: Coordinates a set of fragments and handles re-assembly.
 * Follows ap/STRUCTURE.model.lisp
 */
export class DocumentActor extends Actor {
  private fragments: Map<ActorAddress, any> = new Map();
  private filePath: string | null = null;
  private format: string = "json";
  private activeToken: string | null = null; // Store the granted capability

  async receive(msg: Message) {
    // Capture the token from any incoming message that has one
    if (msg.capabilityToken) {
      this.activeToken = msg.capabilityToken;
    }

    if (msg.type === "INIT_DOCUMENT") {
      this.filePath = msg.payload.path;
      this.format = msg.payload.format;
    }

    if (msg.type === "FRAGMENT_REGISTERED") {
      this.fragments.set(msg.payload.id, msg.payload.content);
    }

    if (msg.type === "FRAGMENT_UPDATED") {
      this.fragments.set(msg.payload.id, msg.payload.content);
      await this.persist();
    }

    if (msg.type === "FILE_CHANGED") {
      // Trigger re-shredding. DocumentParser will reconcile with existing fragment actors.
      this.send("seag://system/parser", {
        type: "SHRED",
        payload: { content: msg.payload.content, format: this.format, docId: this.id }
      });
    }
  }

  private async persist() {
    if (!this.filePath || !this.activeToken) return;

    let content = "";
    if (this.format === "json") {
      const data: any = {};
      const sortedKeys = Array.from(this.fragments.keys()).sort();
      for (const id of sortedKeys) {
        const key = id.split("/").pop()!;
        data[key] = this.fragments.get(id);
      }
      content = JSON.stringify(data, null, 2);
    } else {
      content = Array.from(this.fragments.values()).join("\n");
    }

    this.send("seag://system/file-io", {
      type: "WRITE_FILE",
      payload: { path: this.filePath, data: content },
      capabilityToken: this.activeToken
    });
  }
}