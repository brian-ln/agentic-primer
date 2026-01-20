import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * GeminiInferenceActor: Bridges to Google Gemini.
 * Relies on CredentialProviderActor for authentication.
 */
@ActorModel("GeminiInferenceActor")
@Implements("Inference")
export class GeminiInferenceActor extends Actor {
  private credential: { type: "bearer" | "api_key"; value: string } | null = null;
  private defaultModel: string = "models/gemini-3-pro-preview";
  private useVertex: boolean = false;
  private pendingPrompts: Message[] = [];

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
  @Handler("PROMPT")
  async receive(msg: Message) {
    if (msg.type === "CREDENTIALS") {
      this.credential = { type: msg.payload.credential_type, value: msg.payload.credential };
      console.log(`[GeminiInference] Credentials received. Processing ${this.pendingPrompts.length} pending prompts.`);
      // Process any buffered prompts
      while (this.pendingPrompts.length > 0) {
        const prompt = this.pendingPrompts.shift()!;
        await this.handlePrompt(prompt);
      }
      return;
    }

    if (msg.type === "PROMPT") {
      if (!this.credential) {
        this.pendingPrompts.push(msg);
        return;
      }
      await this.handlePrompt(msg);
    }
  }

  @Handler("HANDLE_PROMPT")
  public async handlePrompt(msg: Message) {
    if (!this.credential) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: "No credentials loaded" } });
      return;
    }

    const { text, params } = msg.payload;
    let model = params?.model || this.defaultModel;
    if (!model.startsWith("models/")) model = `models/${model}`;

    let url = `https://generativelanguage.googleapis.com/v1beta/${model}:generateContent`;
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
        body: JSON.stringify({ contents: [{ parts: [{ text }] }] })
      });

      if (!response.ok) {
        const err = await response.json();
        throw new Error(`Gemini Error: ${err.error?.message || response.statusText}`);
      }

      const data = await response.json();
      const replyText = data.candidates?.[0]?.content?.parts?.[0]?.text || "No response content";
      
      this.send(msg.sender!, { type: "RESPONSE", payload: { text: replyText } });
    } catch (err: any) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: err.message } });
    }
  }
}
