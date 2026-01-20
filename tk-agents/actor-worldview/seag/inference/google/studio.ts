import { Actor, Message } from "../../kernel";
import { Actor as ActorModel, Implements, Handler } from "../../lib/meta";

/**
 * StudioInferenceActor: Bridges to Google AI Studio (Hobbyist/Dev).
 * Identity: generativelanguage.googleapis.com
 * Auth: API Keys
 */
@ActorModel("StudioInferenceActor")
@Implements("Inference")
export class StudioInferenceActor extends Actor {
  private credential: { type: "api_key"; value: string } | null = null;
  private defaultModel: string = "gemini-2.0-flash-exp";
  private pendingPrompts: Message[] = [];

  async onStart() {
    this.send("seag://system/credentials", {
      type: "GET_CREDENTIALS",
      sender: this.id,
      payload: { service_id: "google-ai-studio" }
    });
  }

  @Handler("CREDENTIALS")
  @Handler("PROMPT")
  async receive(msg: Message) {
    if (msg.type === "CREDENTIALS") {
      this.credential = { type: "api_key", value: msg.payload.credential };
      while (this.pendingPrompts.length > 0) {
        await this.handlePrompt(this.pendingPrompts.shift()!);
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

  private async handlePrompt(msg: Message) {
    const { text, params } = msg.payload;
    let model = params?.model || this.defaultModel;
    if (!model.startsWith("models/")) model = `models/${model}`;

    const url = `https://generativelanguage.googleapis.com/v1beta/${model}:generateContent?key=${this.credential!.value}`;
    
    try {
      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ contents: [{ role: "user", parts: [{ text }] }] })
      });

      if (!response.ok) {
        const err = await response.json();
        throw new Error(`Studio Error: ${err.error?.message || response.statusText}`);
      }

      const data = await response.json();
      const replyText = data.candidates?.[0]?.content?.parts?.[0]?.text || "No response content";
      this.send(msg.sender!, { type: "RESPONSE", payload: { text: replyText } });
    } catch (err: any) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: err.message } });
    }
  }
}
