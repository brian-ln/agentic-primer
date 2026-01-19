import { Actor, Message, ActorAddress } from "./kernel";
import { Actor as ActorModel, Handler } from "./lib/meta";

/**
 * DocumentActor: Coordinates a set of fragments and handles re-assembly.
 * Follows ap/STRUCTURE.model.lisp
 */
@ActorModel("DocumentActor")
export class DocumentActor extends Actor {
  private fragments: Map<ActorAddress, any> = new Map();
  private filePath: string | null = null;
  private format: string = "json";
  private activeToken: string | null = null; // Store the granted capability

  async receive(msg: Message) {
    if (msg.capabilityToken) {
      this.activeToken = msg.capabilityToken;
    }
    
    // Dispatch to decorated handlers (manual dispatch for now until Kernel supports it)
    if (msg.type === "INIT_DOCUMENT") await this.handleInit(msg);
    if (msg.type === "FRAGMENT_REGISTERED") await this.handleFragmentRegistered(msg);
    if (msg.type === "FRAGMENT_UPDATED") await this.handleFragmentUpdated(msg);
    if (msg.type === "FILE_CHANGED") await this.handleFileChanged(msg);
  }

  @Handler("INIT_DOCUMENT")
  private async handleInit(msg: Message) {
    this.filePath = msg.payload.path;
    this.format = msg.payload.format;
  }

  @Handler("FRAGMENT_REGISTERED")
  private async handleFragmentRegistered(msg: Message) {
    this.fragments.set(msg.payload.id, msg.payload.content);
  }

  @Handler("FRAGMENT_UPDATED")
  private async handleFragmentUpdated(msg: Message) {
    this.fragments.set(msg.payload.id, msg.payload.content);
    await this.persist();
  }

  @Handler("FILE_CHANGED")
  private async handleFileChanged(msg: Message) {
    // Trigger re-shredding. DocumentParser will reconcile with existing fragment actors.
    this.send("seag://system/parser", {
      type: "SHRED",
      payload: { content: msg.payload.content, format: this.format, docId: this.id }
    });
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