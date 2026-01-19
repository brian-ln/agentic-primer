import { Actor, Message, CapabilityToken } from "./kernel";

/**
 * FileEffectActor: The "Spoon Manager" for the local disk.
 */
export class FileEffectActor extends Actor {
  
  async receive(msg: Message) {
    if (!this.isAuthorized(msg)) {
      console.error(`[FileEffect] Unauthorized ${msg.type} for ${msg.payload?.path}`);
      this.send(msg.sender!, { 
        type: "ERROR", 
        payload: { message: "Unauthorized: Invalid or missing Capability Token." } 
      });
      return;
    }

    switch (msg.type) {
      case "READ_FILE":
        await this.handleRead(msg);
        break;
      case "WRITE_FILE":
        await this.handleWrite(msg);
        break;
      default:
        console.warn(`[FileEffect] Unknown message type: ${msg.type}`);
    }
  }

  private isAuthorized(msg: Message): boolean {
    if (!msg.capabilityToken) {
      return false;
    }
    
    try {
      const ct: CapabilityToken = JSON.parse(Buffer.from(msg.capabilityToken, 'base64').toString());
      return ct.resource === msg.payload?.path || ct.resource === "*" ;
    } catch {
      return false;
    }
  }

  private async handleRead(msg: Message) {
    const fs = await import("node:fs/promises");
    try {
      const data = await fs.readFile(msg.payload.path, "utf-8");
      this.send(msg.sender!, { type: "FILE_CONTENT", payload: { path: msg.payload.path, data } });
    } catch (err: any) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: err.message } });
    }
  }

  private async handleWrite(msg: Message) {
    const fs = await import("node:fs/promises");
    const path = await import("node:path");
    try {
      await fs.mkdir(path.dirname(msg.payload.path), { recursive: true });
      await fs.writeFile(msg.payload.path, msg.payload.data);
      this.send(msg.sender!, { type: "WRITE_OK", payload: { path: msg.payload.path } });
    } catch (err: any) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: err.message } });
    }
  }
}