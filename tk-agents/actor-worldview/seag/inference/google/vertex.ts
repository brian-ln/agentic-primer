import { Actor, Message } from "../../kernel";
import { Actor as ActorModel, Implements, Handler } from "../../lib/meta";

/**
 * VertexInferenceActor: Bridges to Google Cloud Vertex AI (Enterprise).
 * Identity: aiplatform.googleapis.com
 * Auth: OAuth2 Bearer Tokens
 */
@ActorModel("VertexInferenceActor")
@Implements("Inference")
export class VertexInferenceActor extends Actor {
  private credential: { type: "bearer"; value: string } | null = null;
  private project: string = "";
  private location: string = "";
  private defaultModel: string = "gemini-2.0-flash-exp";
  private pendingPrompts: Message[] = [];

  async onStart() {
    this.project = process.env.GOOGLE_CLOUD_PROJECT || "";
    this.location = process.env.GOOGLE_CLOUD_LOCATION || "us-central1";

    this.send("seag://system/credentials", {
      type: "GET_CREDENTIALS",
      sender: this.id,
      payload: { service_id: "google-vertex-ai" }
    });
  }

  @Handler("CREDENTIALS")
  @Handler("PROMPT")
  async receive(msg: Message) {
    if (msg.type === "CREDENTIALS") {
      this.credential = { type: "bearer", value: msg.payload.credential };
      while (this.pendingPrompts.length > 0) {
        await this.handlePrompt(this.pendingPrompts.shift()!);
      }
      return;
    }

    if (msg.type === "PROMPT") {
      console.log(`[VertexInference] Received PROMPT from ${msg.sender}: ${msg.payload.text}`);
      if (!this.credential) {
        console.log(`[VertexInference] Waiting for credentials...`);
        this.pendingPrompts.push(msg);
        return;
      }
      await this.handlePrompt(msg);
    }
  }

  private async handlePrompt(msg: Message) {
    const { text, params } = msg.payload;
    const model = (params?.model || this.defaultModel).replace(/^models\//, "");

    const url = `https://${this.location}-aiplatform.googleapis.com/v1/projects/${this.project}/locations/${this.location}/publishers/google/models/${model}:generateContent`;
    console.log(`[VertexInference] Calling Vertex API: ${url}`);
    
    try {
      const response = await fetch(url, {
        method: "POST",
        headers: { 
          "Content-Type": "application/json",
          "Authorization": `Bearer ${this.credential!.value}`
        },
        body: JSON.stringify({ contents: [{ role: "user", parts: [{ text }] }] })
      });

      console.log(`[VertexInference] API Response status: ${response.status}`);

      if (!response.ok) {
        const err = await response.json();
        console.error(`[VertexInference] API Error Body:`, JSON.stringify(err));
        throw new Error(`Vertex Error: ${err.error?.message || response.statusText}`);
      }

      const data = await response.json();
      const replyText = data.candidates?.[0]?.content?.parts?.[0]?.text || "No response content";
      console.log(`[VertexInference] AI Reply: ${replyText.substring(0, 50)}...`);
      this.send(msg.sender!, { type: "RESPONSE", payload: { text: replyText } });
    } catch (err: any) {
      console.error(`[VertexInference] Exception: ${err.message}`);
      this.send(msg.sender!, { type: "ERROR", payload: { message: err.message } });
    }
  }
}
