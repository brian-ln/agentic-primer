import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * GeminiEmbeddingActor: Bridges to Google Text Embedding.
 */
@ActorModel("GeminiEmbeddingActor")
@Implements("Embedding")
export class GeminiEmbeddingActor extends Actor {
  private apiKey: string | undefined;
  private defaultModel: string = "models/text-embedding-004";

  async onStart() {
    this.apiKey = process.env.GEMINI_API_KEY;
  }

  async receive(msg: Message) {
    if (msg.type === "EMBED") {
      await this.handleEmbed(msg);
    }
  }

  @Handler("EMBED")
  private async handleEmbed(msg: Message) {
    const { text } = msg.payload;
    let model = this.defaultModel;
    
    if (!this.apiKey) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: "GEMINI_API_KEY missing" } });
      return;
    }

    const url = `https://generativelanguage.googleapis.com/v1beta/${model}:embedContent?key=${this.apiKey}`;

    try {
      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          content: { parts: [{ text }] }
        })
      });

      if (!response.ok) {
        const err = await response.json();
        throw new Error(`Embedding Error: ${err.error?.message || response.statusText}`);
      }

      const data = await response.json();
      const vector = data.embedding?.values;

      if (!vector) throw new Error("No vector in response");

      this.send(msg.sender!, { 
        type: "VECTOR", 
        payload: { floats: vector },
        traceId: msg.traceId
      });

    } catch (err: any) {
      console.error("[GeminiEmbedding] Error:", err.message);
      this.send(msg.sender!, { 
        type: "ERROR", 
        payload: { message: err.message } 
      });
    }
  }
}
