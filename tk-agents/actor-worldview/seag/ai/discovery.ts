import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Handler } from "./lib/meta";

/**
 * GeminiDiscoveryActor: Programmatically retrieves available Gemini model IDs.
 */
@ActorModel("GeminiDiscoveryActor")
export class GeminiDiscoveryActor extends Actor {
  private models: any[] = [];
  private apiKey: string | undefined;

  async onStart() {
    this.apiKey = process.env.GEMINI_API_KEY;
    await this.refreshModels();
  }

  async receive(msg: Message) {
    if (msg.type === "REFRESH_MODELS") {
      await this.refreshModels();
    }
    if (msg.type === "GET_LATEST_MODEL") {
      const type = msg.payload.type || "inference";
      const model = this.findBestModel(type);
      this.send(msg.sender!, { type: "LATEST_MODEL", payload: { model, type } });
    }
  }

  private async refreshModels() {
    if (!this.apiKey) return;
    try {
      const response = await fetch(`https://generativelanguage.googleapis.com/v1beta/models?key=${this.apiKey}`);
      const data = await response.json();
      if (data.models) {
        this.models = data.models;
        console.log(`[Discovery] Refreshed ${this.models.length} models.`);
      }
    } catch (err) {
      console.error("[Discovery] Failed to refresh models:", err);
    }
  }

  private findBestModel(type: string): string {
    if (type === "embedding") {
      return this.models.find(m => m.name.includes("embedding-004"))?.name || "models/text-embedding-004";
    }
    // Prioritize Gemini 3, then 2.5, then 2.0
    const priorities = ["gemini-3-pro", "gemini-3-flash", "gemini-2.5-pro", "gemini-2.5-flash", "gemini-2.0-flash"];
    for (const p of priorities) {
      const found = this.models.find(m => m.name.includes(p));
      if (found) return found.name;
    }
    return "models/gemini-1.5-flash"; // Final fallback
  }
}
