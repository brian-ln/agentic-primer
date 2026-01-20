import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * GeminiInferenceActor: Bridges to Google Gemini.
 */
@ActorModel("GeminiInferenceActor")
@Implements("Inference")
export class GeminiInferenceActor extends Actor {
  private apiKey: string | undefined;
  private defaultModel: string = "models/gemini-3-pro-preview";

  async onStart() {
    this.apiKey = process.env.GEMINI_API_KEY;
  }

  async receive(msg: Message) {
    if (msg.type === "PROMPT") {
      await this.handlePrompt(msg);
    }
  }

  @Handler("PROMPT")
  private async handlePrompt(msg: Message) {
    const { text, params } = msg.payload;
    let model = params?.model || this.defaultModel;
    if (!model.startsWith("models/")) model = `models/${model}`;
    
    if (!this.apiKey) {
       this.send(msg.sender!, { type: "ERROR", payload: { message: "GEMINI_API_KEY missing" } });
       return;
    }

    const url = `https://generativelanguage.googleapis.com/v1beta/${model}:generateContent?key=${this.apiKey}`;

    try {
      console.log(`[GeminiInference] Calling ${model}...`);
      
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 30000);

      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ contents: [{ parts: [{ text }] }] }),
        signal: controller.signal
      });
      clearTimeout(timeoutId);

      console.log(`[GeminiInference] Fetch status: ${response.status}`);
      const rawData = await response.text();
      console.log(`[GeminiInference] RAW RESPONSE: ${rawData}`);

      if (!response.ok) {
        throw new Error(`Gemini API Error (${response.status}): ${rawData}`);
      }

      const data = JSON.parse(rawData);
      
      // Robust extraction: Join all text parts from the first candidate
      const candidate = data.candidates?.[0];
      const parts = candidate?.content?.parts || [];
      const replyText = parts
        .map((p: any) => p.text || "")
        .join("") || "No text in response";
      
      console.log(`[GeminiInference] Extracted reply: ${replyText.slice(0, 50)}...`);

      this.send(msg.sender!, { 
        type: "RESPONSE", 
        payload: { 
          text: replyText,
          raw: data // Send back metadata for debugging
        } 
      });

    } catch (err: any) {
      console.error("[GeminiInference] Error:", err.message);
      this.send(msg.sender!, { 
        type: "ERROR", 
        payload: { message: err.message } 
      });
    }
  }
}