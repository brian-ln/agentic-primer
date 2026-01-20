import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * GeminiEmbeddingActor: Bridges to Google Text Embedding.
 * Relies on CredentialProvider for authentication.
 */
@ActorModel("GeminiEmbeddingActor")
@Implements("Embedding")
export class GeminiEmbeddingActor extends Actor {
  private credential: { type: "bearer" | "api_key"; value: string } | null = null;
  private defaultModel: string = "models/text-embedding-004";
  private useVertex: boolean = false;
  private pendingEmbeds: Message[] = [];

  async onStart() {
    this.useVertex = process.env.GOOGLE_GENAI_USE_VERTEXAI === "true";
    const serviceId = this.useVertex ? "google-vertex-ai" : "google-ai-studio";
    this.send("seag://system/credentials", {
      type: "GET_CREDENTIALS",
      sender: this.id,
      payload: { service_id: serviceId }
    });
  }

  @Handler("CREDENTIALS")
  @Handler("EMBED")
  async receive(msg: Message) {
    if (msg.type === "CREDENTIALS") {
      this.credential = { type: msg.payload.credential_type, value: msg.payload.credential };
      console.log(`[GeminiEmbedding] Credentials received. Processing ${this.pendingEmbeds.length} pending embeds.`);
      while (this.pendingEmbeds.length > 0) {
        const embed = this.pendingEmbeds.shift()!;
        await this.handleEmbed(embed);
      }
      return;
    }
    
    if (msg.type === "EMBED") {
      if (!this.credential) {
        this.pendingEmbeds.push(msg);
        return;
      }
      await this.handleEmbed(msg);
    }
  }

  @Handler("HANDLE_EMBED")
  private async handleEmbed(msg: Message) {
    if (!this.credential) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: "No credentials loaded" } });
      return;
    }
    
    const { text } = msg.payload;
    let model = this.defaultModel;

    let url = `https://generativelanguage.googleapis.com/v1beta/${model}:embedContent`;
    const headers: Record<string, string> = { "Content-Type": "application/json" };
    
    if (this.credential.type === "api_key") {
      url += `?key=${this.credential.value}`;
    } else {
      headers["Authorization"] = `Bearer ${this.credential.value}`;
    }

    try {
      const response = await fetch(url, {
        method: "POST",
        headers,
        body: JSON.stringify({ content: { parts: [{ text }] } })
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
      this.send(msg.sender!, { type: "ERROR", payload: { message: err.message } });
    }
  }
}