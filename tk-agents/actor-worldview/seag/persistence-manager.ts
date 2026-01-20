import { Actor, Message, ActorAddress, CapabilityToken } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * PersistenceManager: Coordinates actor state durability and recovery.
 * Follows ap/STRUCTURE.model.lisp
 */
@ActorModel("PersistenceManager")
@Implements("Persistence")
export class PersistenceManager extends Actor {
  private registry: Map<ActorAddress, string> = new Map();

  private generateToken(): string {
    const ct: CapabilityToken = { resource: "*", action: "*", expiresAt: Date.now() + 10000 };
    return Buffer.from(JSON.stringify(ct)).toString('base64');
  }

  @Handler("SNAPSHOT")
  @Handler("RESTORE")
  @Handler("COMMIT_LOG")
  async receive(msg: Message) {
    // 1. Snapshot Flow: Request state from actor, then write to URI
    if (msg.type === "SNAPSHOT") {
      const { target_uri } = msg.payload;
      const targetActor = msg.sender!; // Usually the actor snapshotting itself
      
      // Request state from the target
      this.send(targetActor, { 
        type: "GET", 
        sender: this.id,
        traceId: `snap-${target_uri}` 
      });
    }

    // 2. Handle the returned state from the GET request
    if (msg.type === "STATE") {
      const targetUri = msg.traceId?.startsWith("snap-") ? msg.traceId.slice(5) : null;
      if (targetUri) {
        this.send("seag://system/file-io", {
          type: "WRITE_FILE",
          payload: { path: targetUri, data: JSON.stringify(msg.payload, null, 2) },
          capabilityToken: this.generateToken()
        });
        console.log(`[Persistence] Snapshot committed: ${targetUri}`);
      }
    }

    // 3. Restore Flow: Read from URI, then patch the actor
    if (msg.type === "RESTORE") {
      const { source_uri, target_actor } = msg.payload;
      
      this.send("seag://system/file-io", {
        type: "READ_FILE",
        payload: { path: source_uri },
        traceId: `restore-${target_actor}`,
        capabilityToken: this.generateToken()
      });
    }

    // 4. Handle the file content for restoration
    if (msg.type === "FILE_CONTENT") {
      const targetActor = msg.traceId?.startsWith("restore-") ? msg.traceId.slice(8) : null;
      if (targetActor) {
        this.send(targetActor, {
          type: "PATCH",
          payload: JSON.parse(msg.payload.data)
        });
        console.log(`[Persistence] Actor restored: ${targetActor}`);
      }
    }
  }
}
