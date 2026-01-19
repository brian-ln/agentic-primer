import { Actor, Message } from "./kernel";
import { Actor as ActorModel, Implements, Handler } from "./lib/meta";

/**
 * GeminiInferenceActor: Bridges to Google Gemini 1.5 Flash.
 * Follows ap/PROTOCOLS.model.lisp (Inference)
 */
@ActorModel("GeminiInferenceActor")
@Implements("Inference")
export class GeminiInferenceActor extends Actor {
  private apiKey: string | undefined;

  async onStart() {
    this.apiKey = process.env.GEMINI_API_KEY;
    if (!this.apiKey) {
      console.error("[GeminiInference] Missing GEMINI_API_KEY in environment");
    }
  }

  async receive(msg: Message) {
    if (msg.type === "PROMPT") {
      await this.handlePrompt(msg);
    }
  }

  @Handler("PROMPT")
  private async handlePrompt(msg: Message) {
    if (!this.apiKey) {
      this.send(msg.sender!, { type: "ERROR", payload: { message: "API Key not configured" } });
      return;
    }

    const { text, params } = msg.payload;
    const model = params?.model || "gemini-3-flash-preview";
    const url = `https://generativelanguage.googleapis.com/v1beta/models/${model}:generateContent?key=${this.apiKey}`;

    try {
      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          contents: [{ parts: [{ text }] }],
          generationConfig: {
            temperature: params?.temperature ?? 0.7,
            maxOutputTokens: params?.maxOutputTokens ?? 1000,
            topP: params?.topP ?? 0.95,
            topK: params?.topK ?? 40
          }
        })
      });

      const data = await response.json();
      
      if (data.error) {
        throw new Error(data.error.message);
      }

      const replyText = data.candidates?.[0]?.content?.parts?.[0]?.text || "No response content";
      
      this.send(msg.sender!, { 
        type: "RESPONSE", 
        payload: { text: replyText } 
      });

    } catch (err: any) {
      this.send(msg.sender!, { 
        type: "ERROR", 
        payload: { message: err.message } 
      });
    }
  }
}
