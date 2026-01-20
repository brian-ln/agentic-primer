import { Actor, Message, ActorAddress } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * InferenceRouter: The Virtual Stable Identity for all Inference.
 * Orchestrates multiple concrete model providers.
 */
@ActorModel("InferenceRouter")
@Implements("Inference")
@Implements("ModelRegistry")
export class InferenceRouter extends Actor {
  private routingTable: Map<string, ActorAddress> = new Map();
  private defaultModel: string | undefined;

  @Handler("PROMPT")
  @Handler("REGISTER_PROVIDER")
  @Handler("GET_PROVIDER")
  @Handler("LIST_MODELS")
  async receive(msg: Message) {
    // 1. Inference Protocol
    if (msg.type === "PROMPT") {
      const modelId = msg.payload.params?.model || this.defaultModel;
      const target = this.routingTable.get(modelId!);
      
      if (!target) {
        this.send(msg.sender!, { 
          type: "ERROR", 
          payload: { message: `No provider registered for model: ${modelId}` } 
        });
        return;
      }

      // Delegate the prompt (preserving the original sender for the reply)
      this.send(target, {
        ...msg,
        sender: msg.sender // Keep original requester as the reply target
      });
    }

    // 2. ModelRegistry Protocol
    if (msg.type === "REGISTER_PROVIDER") {
      const { model_id, actor_address, is_default } = msg.payload;
      this.routingTable.set(model_id, actor_address);
      if (is_default) this.defaultModel = model_id;
      
      console.log(`[Router] Registered ${model_id} -> ${actor_address}`);
      this.send(msg.sender!, { type: "REGISTER_OK" });
    }

    if (msg.type === "GET_PROVIDER") {
      const addr = this.routingTable.get(msg.payload.model_id);
      if (addr) {
        this.send(msg.sender!, { type: "PROVIDER_INFO", payload: { actor_address: addr } });
      } else {
        this.send(msg.sender!, { type: "ERROR", payload: { message: "Not found" } });
      }
    }

    if (msg.type === "LIST_MODELS") {
      this.send(msg.sender!, { 
        type: "MODEL_LIST", 
        payload: { models: Array.from(this.routingTable.keys()) } 
      });
    }
  }
}
