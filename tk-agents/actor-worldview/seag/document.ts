import { Actor, Message, ActorAddress } from "./kernel";
import { Actor as ActorModel, Handler } from "./lib/meta";

/**
 * Document: Coordinator for fragments.
 * Follows ap/STRUCTURE.model.lisp
 */
@ActorModel("DocumentActor") // Keep model name as DocumentActor for consistency
export class Document extends Actor {
  public path: string = "";
  public format: string = ""; // e.g., 'json', 'markdown'
  public fragments: Map<string, any> = new Map(); // frag_id -> content

  @Handler("INIT_DOCUMENT")
  @Handler("FRAGMENT_REGISTERED")
  @Handler("FRAGMENT_UPDATED")
  async receive(msg: Message) {
    if (msg.type === "INIT_DOCUMENT") {
      this.path = msg.payload.path;
      this.format = msg.payload.format;
      console.log(`[Document] Initialized document: ${this.id} from ${this.path}`);
    }

    if (msg.type === "FRAGMENT_REGISTERED") {
      const { id, content } = msg.payload;
      this.fragments.set(id, content);
    }

    if (msg.type === "FRAGMENT_UPDATED") {
      const { id, content } = msg.payload;
      this.fragments.set(id, content);
      // Trigger re-assembly or persistence logic here if needed
      console.log(`[Document] Fragment updated: ${id}`);
    }
  }
}
