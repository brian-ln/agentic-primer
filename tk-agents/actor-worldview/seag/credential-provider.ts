import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * CredentialProviderActor: A centralized secret store for the SEAG system.
 * Follows ap/CREDENTIALS.model.lisp
 */
@ActorModel("CredentialProviderActor")
@Implements("CredentialProvider")
export class CredentialProviderActor extends Actor {
  private secrets: Map<string, { type: "bearer" | "api_key", value: string | (() => Promise<string>) }> = new Map();

  async onStart() {
    console.log("[Credentials] Starting...");
    // 1. Register API Keys (Static Secrets)
    if (process.env.GEMINI_API_KEY) {
      this.secrets.set("google-ai-studio", { type: "api_key", value: process.env.GEMINI_API_KEY });
      console.log("[Credentials] Registered google-ai-studio API key.");
    }
    if (process.env.ANTHROPIC_API_KEY) {
      this.secrets.set("anthropic", { type: "api_key", value: process.env.ANTHROPIC_API_KEY });
    }

    // 2. Register Dynamic Token Generators
    this.secrets.set("google-vertex-ai", { type: "bearer", value: this.getVertexToken });
    console.log("[Credentials] Registered google-vertex-ai token generator.");
  }

  private async getVertexToken(): Promise<string> {
    const { $ } = await import("bun");
    try {
      const output = await $`gcloud auth print-access-token`.text();
      return output.trim();
    } catch (err) {
      console.error("[Credentials] Failed to get gcloud token:", err);
      throw new Error("gcloud token failure");
    }
  }

  @Handler("GET_CREDENTIALS")
  async receive(msg: Message) {
    if (msg.type === "GET_CREDENTIALS") {
      const { service_id } = msg.payload;
      console.log(`[Credentials] Request for: ${service_id}`);
      const entry = this.secrets.get(service_id);

      if (!entry) {
        console.error(`[Credentials] No entry for ${service_id}`);
        this.send(msg.sender!, { type: "ERROR", payload: { message: `No credentials for ${service_id}` } });
        return;
      }

      try {
        const credential = typeof entry.value === 'function' ? await entry.value() : entry.value;
        this.send(msg.sender!, {
          type: "CREDENTIALS",
          payload: { credential_type: entry.type, credential }
        });
      } catch (err: any) {
        this.send(msg.sender!, { type: "ERROR", payload: { message: err.message } });
      }
    }
  }
}
