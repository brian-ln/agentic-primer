import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * GeminiEmbeddingActor: Bridges to Google Text Embedding 004.
 * Follows ap/PROTOCOLS.model.lisp (Embedding)
 */
@ActorModel("GeminiEmbeddingActor")
@Implements("Embedding")
export class GeminiEmbeddingActor extends Actor {
  private apiKey: string | undefined;

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
    if (!this.apiKey) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: "API Key not configured" } });
      return;
    }

    const { text } = msg.payload;
    const url = `https://generativelanguage.googleapis.com/v1beta/models/text-embedding-004:embedContent?key=${this.apiKey}`;

    try {
      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          model: "models/text-embedding-004",
          content: { parts: [{ text }] }
        })
      });

      const data = await response.json();
      
      if (data.error) {
        throw new Error(data.error.message);
      }

      const vector = data.embedding?.values || [];
      
      this.send(msg.sender!, { 
        type: "VECTOR", 
        payload: { floats: vector } 
      });

    } catch (err: any) {
      this.send(msg.sender!, { 
        type: "ERROR", 
        payload: { message: err.message } 
      });
    }
  }
}
